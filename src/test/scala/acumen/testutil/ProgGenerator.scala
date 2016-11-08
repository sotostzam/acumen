package acumen
package testutil

import scala.collection.mutable.{Map => MutMap}
import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck.Properties
import org.scalacheck.Shrink._
import acumen.Pretty.pprint
import acumen.util.Canonical
import spire.math.Rational
import acumen.interpreters.enclosure.Generators.{
  genSubInterval
}
import Generators.{ 
  boundedListOf, boundedListOfIf, completeNames, 
  genBoundedSetOfNonMembers, genCompliantSetOfN, genDistinctSetOfN, 
  genPositiveBoundedInterval, genSetBasedOn, genSmallDouble, listOfNIf
}
import ODESystemGenerator.{
  genLinearODESystem, genPredicate, doubleToLit, nameToVar
}
import acumen.interpreters.enclosure.Interval
import acumen.interpreters.enclosure.TestingContext.{
  rnd
}
import Errors.{
  AcumenError, ContinuousDynamicsUndefined
}

/**
 * Generator of interesting acumen Prog instances (models).
 *
 * Here, interesting means:
 *
 *  - Executable, e.g.
 *    - Discrete assignments never occur on the top level,
 *      meaning that time will proceed.
 *    - No references to unbound variables are made throughout
 *      the model.
 *    - If a variable with Name(ticks,id) is defined, then 
 *      variables with Name(t,i), for all 0 <= t < ticks must
 *      also be defined.
 *  - Representative of models in the examples directory
 *    - Comprises one or more ClassDefs.
 *    - Objects of ClassDefs may be related through equations
 *      or predicates in a parent object.
 *    - May be either purely continuous or include control
 *      statements such as If or Switch, containing discrete
 *      assignments.
 */
class ProgGenerator
  ( val maxConditionalsPerScope     : Int    = 2
  , val maxSimulationTime           : Double = 10.0
  , val maxClassHierarchyDepth      : Int    = 1
  , val maxClassHierarchyLayerWidth : Int    = 2
  , val minContinuousVarsPerClass   : Int    = 1
  , val maxContinuousVarsPerClass   : Int    = 4
  ) {

  /* Types */
  
  // Indicates whether a generator executes in the context of a switch based on a particular mode variable
  type Active = Boolean
  // Used to track whether a given name has been used as the LHS of a continuous or discrete assignment that is in scope
  abstract class ConsumableDot(val dot: Dot) { 
    def isFree: Boolean
    def asUsed = UsedDot(dot)
  }
  case class FreeDot(override val dot: Dot) extends ConsumableDot(dot) { override def isFree = true }
  object FreeDot{ def apply(n: Name): FreeDot = FreeDot(Dot(self,n)) }
  case class UsedDot(override val dot: Dot) extends ConsumableDot(dot) { override def isFree = false }  
  object UsedDot{ def apply(n: Name): UsedDot = UsedDot(Dot(self,n)) }
  
  /* Constants */
  
  val self = Var(Name("self", 0))
  
  /* Generators */
  
  implicit def arbProg: Arbitrary[Prog] = Arbitrary { genProg }
  val emptyProg = Prog(Nil)

  var classNameIndex = 0
  def genClassName(): Gen[ClassName] = {
    val name = "C" + classNameIndex
    classNameIndex += 1
    ClassName(name)
  }
  implicit def arbClassName = Arbitrary(genClassName)

  var varNameIndex = 0
  def genName(): Gen[Name] = 
    for {
      primes <- oneOf(List(0,1,2,3))
    } yield {
      val name = "x" + varNameIndex
      varNameIndex += 1
      Name(name, primes)
    }
  implicit def arbName = Arbitrary(genName)
  
  /**
   * Generator of Acumen models (Prog). 
   * 
   * The structure of the generated models can be configured using the 
   * parameters of the parent class.
   * 
   * Basic algorithm:
   * 
   *  1. Generate list of leaf classes and add these to a environment.
   *  2. A small, random number of times, generate list of classes,
   *     based on the environment, and add these to the environment.
   *  3. Generate a main class based on the environment and wrap in a Prog.
   */
  def genProg: Gen[Prog] = {
    classNameIndex = 0
    varNameIndex = 0
    for {
      depth <- chooseNum[Int](0, maxClassHierarchyDepth)
      env   <- if (depth == 0) value(List[ClassDef]()) 
               else for { 
                 firstLayer <- boundedListOf(1, depth, genClassDef(isMain = false, genClassName, Nil))
                 h <- genClassHierarchy(firstLayer, depth)
               } yield h
      m <- genMain(env)
      p <- ensureGoodProg(m)
    } yield p
  }

  /**
   * If p throws errors upon simulation, try to repair it:
   *  - If a ContinuousDynamicsUndefined error for variable n is 
   *    encountered, insert a continuous assignment to the version
   *    of n in p that has most primes and call ensureGoodProg with
   *    the resulting updated program.
   * If no repair method is known, generate a new Prog.
   */
  private def ensureGoodProg(p: Prog): Gen[Prog] =
    simulationThrowsError(p) match {
      case None => value(p)
      case Some(err@ContinuousDynamicsUndefined(_, n,_, cn, _)) =>
        ensureGoodProg(Prog(p.defs.map {
          case cd @ ClassDef(cn1, fs, ps, body) if cn1.x == cn =>
            val nWithMostPrimes = (fs ++ ps.map(_.x)).filter(_.x == n.x).sortBy(_.primes).last
            val nc = Continuously(EquationT(Var(nWithMostPrimes), Lit(GRational(0))))
            new ClassDef(cn1, fs, ps, nc :: body) { _types = cd._types }
          case cd => cd
        }))
      case _ => genProg
    }
  
  /** Generates a hierarchy of ClassDefs which are related through constructor calls and object references. */
  def genClassHierarchy(children: List[ClassDef], depth: Int): Gen[List[ClassDef]] =
    if (depth < 0) children
    else for {
      layerSize <- chooseNum[Int](1, maxClassHierarchyLayerWidth)
      nextLayer <- listOfN(layerSize, genClassDef(isMain = false, genClassName, children))
      f <- genClassHierarchy(nextLayer ++ children, depth - 1)
    } yield f
  
  /** 
   * Generates a ClassDef for the Main class based on the ClassDefs in env and wraps the 
   * resulting ClassDefs in a Prog. 
   */
  def genMain(env: List[ClassDef]): Gen[Prog] =
    for { main <- genClassDef(isMain = true, ClassName("Main"), env) } yield Prog(main +: env)
    
  /**
   * Attempts to execute the Prog p using the new reference interpreter. 
   * Returns false if an exception is thrown during execution, e.g. when a repeated assignment 
   * is detected in the interpreter, or if a possible infinite loop is detected (more than 1000 
   * consecutive discrete steps).
   */
  def simulationThrowsError(p: Prog): (Option[AcumenError]) =
    try {
      var discreteStepsInARow = 0
      val si = SemanticsImpl.Ref2014
      val CStoreRes(r,_) = si.interpreter.lazyRun(si.applyRequiredPasses(p))
      for (cstore <- r) {
        if (Canonical.getResultType(cstore) == Discrete) discreteStepsInARow += 1
        else discreteStepsInARow = 0
        if (discreteStepsInARow > 1000)
          throw new RuntimeException("Model exceeded maximum number of discrete steps in fixpoint iteration. Possible infinite loop.")
      }
      None
    } catch { case e: AcumenError => Some(e) }
  
  /**
   * Generate a class definition.
   * 
   * TODO Add support for vectors
   */
  def genClassDef( isMain: Boolean
                 , classNameGen: => Gen[ClassName]
                 , env: List[ClassDef] // If empty, no constructor calls or child (Dot) references will be generated.
                 ): Gen[ClassDef] = {
    val (timeVar, timeVarD) = (Name("t", 0), Name("t", 1))
    for {
      // Names for continuous, mode and object variables. For mode names, possible values are also generated. 
      // Names that can be used as LHSs in equations in this ClassDef.
      lhsContNames        <- genBoundedSetOfNonMembers(minContinuousVarsPerClass, maxContinuousVarsPerClass, 
                               Set(timeVar,timeVarD), arbitrary[Name])
      // Names that can be used as LHSs in equations in the parent ClassDef
      lhsContNamesParent  <- genBoundedSetOfNonMembers(minContinuousVarsPerClass/2, maxContinuousVarsPerClass/2, 
                               Set(timeVar,timeVarD) union lhsContNames, arbitrary[Name])
      contNames           =  completeNames(lhsContNames)
      contNamesParent     =  completeNames(lhsContNamesParent)
      contNamesAll        =  contNames union contNamesParent
      bannedModeNames     =  contNamesAll + timeVar + timeVarD
      modes               <- genModes(bannedModeNames)
      objectNames         <- genObjectNames(env, bannedModeNames union modes.map(_._2))
      // Class fields and private variables
      numberOfFields      <- if (isMain) value(0) else chooseNum[Int](0, contNames.size/2)
      (fields, privNames) =  contNamesAll splitAt numberOfFields
      privsNoTime         <- genPrivateSection(privNames.map((n:Name) => FreeDot(n)), modes, objectNames, env)
      privs               =  Init(timeVar, ExprRhs(0)) +: Init(timeVarD, ExprRhs(1)) +: privsNoTime.toList
      privsTypes          =  constructTypeMap(privs, contNames, contNamesParent)
      timeDomain          <- genPositiveBoundedInterval(maxSimulationTime)
      actionsNoTime       <- genActions( isTopLevel  = true
                                       , contNames   = lhsContNames map ((n:Name) => FreeDot(n))
                                       , discrNames  = (privNames ++ fields) map ((n:Name) => FreeDot(n))
                                       , modes       = modes
                                       , childRefs   = findFreeChildPrivs(objectNames, privs, privsTypes, env)
                                       , parentTypes = privsTypes
                                       , env         = env
                                       , time        = (timeVar, timeDomain)
                                       , nesting     = 0
                                       )
      actions             =  Continuously(Equation(timeVarD, 1)) +: actionsNoTime.toList 
      className           <- classNameGen
    } yield {
      val cd = ClassDef(className, if (isMain) List(Name("simulator",0)) else fields.toList, privs, actions)
      // Save type information, used to generate object references
      cd._types = MutMap[Name, TypeLike]()
      cd._types ++= privsTypes
      cd
    }
  }
  
  /** 
   * Create a map from variable names to type descriptors.
   * 
   * Currently only tags variables in the private section with types.
   * Definition is only useful in the context of ProgGen! See source for details.
   */
  def constructTypeMap(privs: List[Init], contNames: Set[Name], contNamesParent: Set[Name]): Map[Name, TypeLike] =
    privs.map(_ match { case Init(name, rhs) =>
      (name -> (rhs match {
        case NewRhs(Var(n), _) =>
          ClassType(NamedClass(ClassName(n.x)))
        case ExprRhs(_) => // mode variables are integers, others floating point numbers
          if (contNamesParent contains name) NumericType 
          else if (contNames contains name) DynamicType //FIXME Gross hack! See explanation in findFreeChildPrivs
          else IntType
      }))}).toMap[Name, TypeLike] 
  
  /** 
   * Collect all private variables of objects listed in objectNames, for which no 
   * continuous/discrete assignments exist in the corresponding ClassDef.
   */
  def findFreeChildPrivs(objectNames: Set[Name], privs: List[Init], privsTypes: Map[Name, TypeLike], env: List[ClassDef]): Set[ConsumableDot] =
    objectNames flatMap { (objName:Name) =>
       privsTypes(objName) match { // Look up the object's class name
         case ClassType(NamedClass(cn)) =>
           val cd = env.find(_.name == cn).get // Look up ClassDef based on class name 
           cd.priv.filter { init => // Find privs known not to have assignments in their ClassDef 
             /* FIXME Gross hack! This relies on the fact that variables that are possibly used 
              * as LHS in discrete/continuous assignments in the ClassDef in which they are 
              * declared have their type set to DynamicType. Only variables that are guaranteed 
              * not to be used as LHS in their host class will be typed as NumericType.
              */
             cd._types(init.x).numericLike && init.x.x != "t"  
           }.map{ init => FreeDot(Dot(objName, init.x)) } 
         case _ => List()
       }
     }
  
  /** Generate object names, i.e. names of variables which will be initialized using constructor calls. */
  def genObjectNames(env: List[ClassDef], banned: Set[Name]): Gen[Set[Name]] = 
    if (env.isEmpty) Set[Name]() // Leaf class
    else for {
      constructThese <- someOf(env)
      objectNames    <- genSetBasedOn(constructThese.toSet, 
                          (cd: ClassDef) => for { n <- arbitrary[Name] } yield Name("obj_" + cd.name.x + "_" + n.x, 0))
      res            <- if (objectNames.exists(banned.contains(_))) genObjectNames(env, banned) else value(objectNames)            
    } yield res
    
  /**
   * Generates triples (a,n,vs) representing a mode variable.
   * 
   * a:  When this boolean is set to true, this means that the current generator is 
   *     executed in the context of a switch statement which switches on the mode 
   *     variable corresponding to the Name in the same triple.
   * n:  The name of the mode variable.
   * vs: The possible values to which the mode variable can be switched (one case
   *     clause per such value will be generated for a switch based on this triple).
   */
  def genModes(banned: Set[Name]): Gen[Set[(Active,Name,List[StaticGroundValue])]] =
    for {
      modeNames    <- genBoundedSetOfNonMembers(1, 4, banned, for { n <- arbitrary[Name] } yield Name(n.x, 0))
      modesPerName <- listOfN(modeNames.size, choose[Int](2, 5))
      modes        =  (modeNames zip modesPerName.map(n => (0 to n).toList.map(x => GRational(Rational(x))): List[StaticGroundValue])).
                      map { case (m, n) => (false, m, n) }
    } yield modes
    
  /**
   * Generate actions for a scope.
   * 
   * In order to avoid generation of continuous assignments to variables for which a
   * continuous assignment is already in scope, a set of available equations is maintained. 
   * If a continuous assignment to a variable is generated, it is will be removed from 
   * the from the list of available names passed to generators of actions for new scopes 
   * (e.g. those of then and else statements of conditionals).
   * 
   * Note: Each mode triple contains a boolean indicating whether the action is generated in 
   *       the context of a switch statement based on the given mode variable. 
   */
  def genActions
    ( isTopLevel: Boolean
    , contNames: Set[ConsumableDot]
    , discrNames: Set[ConsumableDot]
    , modes: Set[(Active,Name,List[StaticGroundValue])]
    , childRefs: Set[ConsumableDot]
    , parentTypes: Map[Name, TypeLike]
    , env: List[ClassDef]
    , time: (Name,Interval)
    , nesting: Int
    ): Gen[Set[Action]] = {
      val (freeContNames, usedContNames) = contNames.partition(_.isFree)
      val (freeRefs, usedRefs) = childRefs.partition(_.isFree)
      if (freeContNames.isEmpty) Set[Action]()
      else for {
        nbrContNamesToUse        <- chooseNum[Int](1, freeContNames.size) // How many to use in this scope
        nbrRefsNamesToUse        <- chooseNum[Int](0, freeRefs.size) // 0 in leaf classes
        (nowC,laterC)            =  freeContNames.toList.splitAt(nbrContNamesToUse)
        (nowR,laterR)            =  freeRefs.toList.splitAt(nbrRefsNamesToUse)
        updContNames             =  nowC.map(_.asUsed).toSet union laterC.toSet union usedContNames
        updRefNames              =  nowR.map(_.asUsed).toSet union laterR.toSet union usedRefs
        equationLHSs             =  highestDerivatives(nowC ++ nowR).toSet
        equations                <- genLinearODESystem(equationLHSs).map(_.map(Continuously): Set[Action])
        (discrAs, updDiscrNames) <- genDiscreteActions(isTopLevel, discrNames, modes, parentTypes, time)
        nbrConditionalStmts      <- chooseNum[Int](0, maxConditionalsPerScope) 
        nbrSwitchStmts           <- chooseNum[Int](0, maxConditionalsPerScope - nbrConditionalStmts)
        conditionals             <- listOfNIf(laterC.nonEmpty, nbrConditionalStmts, 
                                      genIfThenElse(updContNames, updDiscrNames, modes, updRefNames, parentTypes, env, time, nesting))
        switches                 <- listOfNIf(laterC.nonEmpty && modes.nonEmpty, nbrSwitchStmts,
                                      genSwitch(updContNames, updDiscrNames, modes, updRefNames, parentTypes, env, time, nesting))
      } yield equations union conditionals.toSet union switches.toSet union discrAs
    }
  
  def highestDerivatives(names: List[ConsumableDot]): List[Dot] = {
    val dots = names.map(_.dot)
    dots.filter(n => dots.forall(m => n.obj != m.obj || n.field.primes >= m.field.primes))
  }
   

  def genDiscreteActions
    ( isTopLevel: Boolean
    , discrVarNames: Set[ConsumableDot]
    , modes: Set[(Active,Name,List[StaticGroundValue])]
    , parentTypes: Map[Name, TypeLike]
    , time: (Name, Interval)
    ): Gen[(Set[Action], Set[ConsumableDot])] = {
   val freeVarNames = discrVarNames.filter(_.isFree)
   if (isTopLevel || freeVarNames.isEmpty) (Set[Action](), discrVarNames) 
   else for {
      nbrDiscrAssgsToUse  <- chooseNum[Int](1, freeVarNames.size) // How many to use in this scope
      (now,later)         =  freeVarNames.toList.splitAt(nbrDiscrAssgsToUse)
      discreteAssignments <- genDiscreteAssignments(isTopLevel, now.toSet, modes, time)
      updatedNames        =  now.map(_.asUsed).toSet union later.toSet
    } yield (discreteAssignments, updatedNames)
  }
    
  def genIfThenElse
    ( equationNames: Set[ConsumableDot]
    , varNames: Set[ConsumableDot]
    , modes: Set[(Active,Name,List[StaticGroundValue])]
    , objectNames: Set[ConsumableDot]
    , parentTypes: Map[Name, TypeLike]
    , env: List[ClassDef]
    , time: (Name, Interval)
    , nesting: Int
    ): Gen[Action] =
    for {
      cond       <- genPredicate(varNames.map(_.dot), time)
      thenClause <- genActions(isTopLevel = false, equationNames, varNames, modes, objectNames, parentTypes, env, time, nesting + 1)
      elseClause <- genActions(isTopLevel = false, equationNames, varNames, modes, objectNames, parentTypes, env, time, nesting + 1)
    } yield IfThenElse(cond, thenClause.toList, elseClause.toList)
  
  def genSwitch
    ( contNames: Set[ConsumableDot]
    , discrNames: Set[ConsumableDot]
    , modes: Set[(Active,Name,List[StaticGroundValue])]
    , objectNames: Set[ConsumableDot]
    , parentTypes: Map[Name, TypeLike]
    , env: List[ClassDef]
    , time: (Name, Interval)
    , nesting: Int
    ): Gen[Action] =
    for {
      m@(active, name, vs) <- oneOf(modes.toList)
      mA                   =  (true, name, vs)
      clauses              <- listOfN(vs.size, 
                                genActions( isTopLevel  = true
                                          , contNames   = contNames
                                          , discrNames  = discrNames
                                          , modes       = modes - m + mA
                                          , childRefs   = objectNames
                                          , parentTypes = parentTypes
                                          , env         = env
                                          , time        = time
                                          , nesting     = nesting + 1
                                          ))
      assertion            =  Lit(GBool(true)) // TODO Generate mode invariants
    } yield Switch(name, vs.zip(clauses).map{ case (v, as) => Clause(v, assertion, as.toList) })
  
  def genDiscreteAssignments
    ( isTopLevel: Boolean
    , varNames: Set[ConsumableDot]
    , modes: Set[(Active,Name,List[StaticGroundValue])]
    , time: (Name,Interval)
    ): Gen[Set[Action]] =
    for {
      lhss           <- resize(varNames.size / 4, someOf(varNames))
      rhss           <- listOfN(lhss.size, genSmallDouble).map(_.map(d => Lit(GRational(d)))) //TODO More interesting, state-based resets
      activeModeVars =  modes.toList.filter { case (active, _, _) => active }
      modeResets     <-  for {
                          (_, m, vs)  <- oneOf(activeModeVars)
                          mAfterReset <- oneOf(vs)
                          cond        <- genPredicate(varNames.map(_.dot), time)
                        } yield Set[Action](IfThenElse(cond, List(Discretely(Assign(Var(m), Lit(mAfterReset)))), Nil))
    } yield {
      val nonModeResets = ((lhss zip rhss) map { case (cd, e) => Discretely(Assign(cd.dot, e)) }).toSet[Action]
      nonModeResets union modeResets 
    }
  
  /**
   * Generates initialization statements for the input variable names.
   * NOTE: Assumes that the initial conditions for differential equations can be chosen arbitrarily!
   */
  def genPrivateSection( continuousNames: Set[ConsumableDot]
                       , modes: Set[(Active, Name, List[StaticGroundValue])]
                       , objectNames: Set[Name]
                       , env: List[ClassDef]
                       ): Gen[Set[Init]] = {
    for { //TODO Generate more interesting initial conditions!
      // Generate constructor calls
      objectRhss   <- if (objectNames.isEmpty) value(Nil) else listOfN(objectNames.size, genConstructorCall(env))
      objectInits  =  (objectNames zip objectRhss).map { case (l, r) => Init(l, r) }
      // Generate initial conditions for mode variables 
      modeInits    =  modes.map{ case (_active, n, vs) => Init(n, ExprRhs(Lit(scala.util.Random.shuffle(vs).head))) }
      // Generate initial conditions for continuous variables
      contNames    =  continuousNames.filter(_.isFree)
      contRhss     <- listOfN(contNames.size, genSmallDouble)
      contInits    =  (contNames zip contRhss).map { case (l, r) => Init(l.dot.field, ExprRhs(Lit(GRational(r)))) }
    } yield objectInits union modeInits union contInits
  }
  
  def genConstructorCall(env: List[ClassDef]): Gen[InitRhs] =
    for {
      cn <- oneOf(env)
      fieldValues <- listOfN(cn.fields.size, genSmallDouble)
    } yield NewRhs(Var(Name(cn.name.x,0)), fieldValues.map(v => Lit(GRational(v))))
  
  /**
   * Generate set of distinct variable Names that is complete, in the sense that if the set 
   * contains a Name(n,primes) it will also contain all Name(n,p), where 0 <= p < primes.
   */
  def genCompleteNameSet(minSize: Int): Gen[Set[Name]] =
    for { initial <- genDistinctSetOfN(minSize, arbitrary[Name]) } yield completeNames(initial)

}
