package acumen
package testutil

import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck.Properties
import org.scalacheck.Shrink._

import acumen.Pretty.pprint
import acumen.util.Canonical
import acumen.interpreters.enclosure.Generators.{
  genSubInterval
}
import Generators.{ 
  arbName, completeNames, genSetBasedOn, genCompliantSetOfN, genDistinctSetOfN, genSmallDouble
}
import ODESystemGenerator.{
  genLinearODESystem, genPredicate, doubleToLit, nameToVar
}
import acumen.interpreters.enclosure.Interval
import acumen.interpreters.enclosure.TestingContext.{
  rnd
}
import java.io.ByteArrayOutputStream
import java.io.PrintStream

/**
 * Issues:
 *  - When to generate class definitions? One option seems to be to do 
 *    it when a InitRhs is generated (it should randomly choose to make
 *    a new class definition instead of using one that is available in 
 *    the program).
 *    
 *  - Start by generating a valid leaf object class. This will not contain any Dot's as:
 *    1. No expression in the class's statements may call a constructor.
 *    2. The object may not modify the state of any other object.
 */

/**
 * Generator of interesting acumen Prog instances (models).
 *
 * Here, interesting means:
 *
 *  - Executable
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
  ( val maxConditionalsPerScope : Int
  , val maxSimulationTime       : Double
  ) {

  /* State */
  
  var skipped = 0

  /* Types */
  
  // Indicates whether a generator executes in the context of a switch based on a particular mode variable
  type Active = Boolean
  // Used to track whether a given name has been used as the LHS of a continuous or discrete assignment that is in scope
  abstract class ConsumableName(val name: Name) { def isFree: Boolean }
  case class FreeName(override val name: Name) extends ConsumableName(name) { override def isFree = true }
  case class UsedName(override val name: Name) extends ConsumableName(name) { override def isFree = false }
  
  /* Generators */
  
  implicit def arbProg: Arbitrary[Prog] = Arbitrary { genProg }
  val emptyProg = Prog(Nil)

  /**
   * TODO Implement generation of models with more than one class.
   * 
   * Basic algorithm:
   * 
   *  1. Generate list of leaf classes and add these to Prog.
   *  2. Randomize a small number n, and do the following n times:
   *  3. Generate list of classes, based on Prog, and add these to Prog.
   */
  def genProg: Gen[Prog] =
      for {
        main <-
          genClassDef( isLeaf = true // Prevent generation of constructor calls and child references
                     , isMain = true // Generate the Main class
                     , emptyProg     // Environment
                     , ClassName("Main")
                     )
        m = Prog(List(main))
        p <- if (simulatesOK(m)) value(m) else genProg
      } yield p
    
  /**
   * Attempts to execute the Prog p using the new reference interpreter. 
   * Returns false if an exception is thrown during execution, e.g. when a repeated assignment 
   * is detected in the interpreter, or if a possible infinite loop is detected (more than 1000 
   * consecutive discrete steps).
   */
  def simulatesOK(p: Prog): Boolean =
    try {
      var discreteStepsInARow = 0
      val CStoreRes(r) = interpreters.newreference.Interpreter.run(Desugarer.desugar(p))
      for (cstore <- r) {
        if (Canonical.getResultType(cstore) == Discrete) discreteStepsInARow += 1
        else discreteStepsInARow = 0
        if (discreteStepsInARow > 1000)
          throw new RuntimeException("Model exceeded maximum number of discrete steps in fixpoint iteration. Possible infinite loop.")
      }
      true
    } catch { case e => false }
  
  /**
   * Generate a class definition.
   * 
   * TODO If isLeaf is set to true, no constructor calls or child (Dot) references will be generated.
   */
  def genClassDef( isLeaf: Boolean
                 , isMain: Boolean
                 , p: Prog
                 , classNameGen: => Gen[ClassName]
                 ): Gen[ClassDef] =
    for {
      name                <- classNameGen
      minVars             <- chooseNum(2,5)
      (timeVar, timeVarD) =  (Name("t", 0), Name("t", 1))
      distinctVarNames    <- genCompliantSetOfN(minVars, arbitrary[Name], 
                               (candidate: Set[Name], extension: Set[Name]) => 
                                 extension.forall(n => !candidate.contains(n) && n != timeVar && n != timeVarD) )
      distinctVarNamesT   =  distinctVarNames + timeVar + timeVarD
      modes               <- genModes(distinctVarNamesT)
      varNames            =  completeNames(distinctVarNames)
      numberOfFields      <- chooseNum[Int](0: Int, if (isMain || varNames.isEmpty) 0 else varNames.size)
      (fields, privNames) =  varNames.splitAt(numberOfFields)
      privs               <- genPrivateSection(privNames.map(FreeName), modes)
      privsT              =  Init(timeVar, ExprRhs(0)) +: Init(timeVarD, ExprRhs(1)) +: privs.toList 
      timeDomain          <- for { m <- choose[Double](Double.MinPositiveValue, maxSimulationTime)
                                   i <- genSubInterval(Interval(0, m))
                                 } yield i 
      actions             <- genActions( isTopLevel = true
                                       , equationNames = distinctVarNames.map(FreeName)
                                       , (privNames ++ fields).map(FreeName)
                                       , modes
                                       , (timeVar, timeDomain)
                                       , nesting = 0
                                       )
      actionsT            =  Continuously(Equation(timeVarD, 1)) +: actions.toList 
    } yield ClassDef(name, if (isMain) List(Name("simulator",0)) else fields.toList, privsT, actionsT)

  /**
   * Generates triples (a,n,vs) representing a mode variable:
   * a:  When this boolean is set to true, this means that the current generator is 
   *     executed in the context of a switch statement which switches on the mode 
   *     variable corresponding to the Name in the same triple.
   * n:  The name of the mode variable.
   * vs: The possible values to which the mode variable can be switched (one case
   *     clause per such value will be generated for a switch based on this triple).
   */
  def genModes(banned: Set[Name]): Gen[Set[(Active,Name,List[GroundValue])]] =
    for {
      minModeVars  <- chooseNum(0,4)
      modeNames    <- genCompliantSetOfN(minModeVars, for { n <- arbitrary[Name] } yield Name(n.x, 0),
                        (candidate: Set[Name], compliant: Set[Name]) =>
                          candidate.forall(e => !compliant.contains(e) && !banned.contains(e)))
      modesPerName <- listOfN(modeNames.size, choose[Int](2, 5))
      modes        =  (modeNames zip modesPerName.map(n => (0 to n).toList.map(GInt): List[GroundValue])).
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
    , equationNames: Set[ConsumableName]
    , disNames: Set[ConsumableName]
    , modes: Set[(Active,Name,List[GroundValue])]
    , time: (Name,Interval)
    , nesting: Int
    ): Gen[Set[Action]] = {
      val (freeEquationNames, usedEquationNames) = equationNames.partition(_.isFree)
      if (freeEquationNames.isEmpty) Set[Action]()
      else for {
        nbrEquationsToUse      <- chooseNum[Int](1, freeEquationNames.size) // How many to use in this scope
        (now,later)            =  freeEquationNames.toList.splitAt(nbrEquationsToUse)
        updContNames           =  now.map(cn => UsedName(cn.name)).toSet union later.toSet union usedEquationNames
        equations: Set[Action] <- genLinearODESystem(now.map(_.name).toSet).map(_.map(Continuously): Set[Action])
        (discrAs, updDisNames) <- genDiscreteActions(isTopLevel, disNames.filter(_.isFree), modes)
        nbrConditionalStmts    <- chooseNum[Int](0, maxConditionalsPerScope) 
        nbrSwitchStmts         <- chooseNum[Int](0, Math.min( maxConditionalsPerScope - nbrConditionalStmts 
                                                            , if (later.size > nesting + 1) 1 else 0 // Avoid nesting explosion
                                                            ))
        conditionals           <- listOfNIf(later.nonEmpty, nbrConditionalStmts, 
                                    genIfThenElse(updContNames, updDisNames, modes, time, nesting))
        switches               <- listOfNIf(later.nonEmpty, nbrSwitchStmts,
                                    genSwitch(updContNames, updDisNames, modes, time, nesting))
      } yield equations union conditionals.toSet union switches.toSet union discrAs
    }

  def listOfNIf[E](cond: Boolean, size: Int, gen: Gen[E]): Gen[List[E]] =
    if (!cond) value(List()) else listOfN(size, gen)
    
  def genDiscreteActions
    ( isTopLevel: Boolean
    , unassignedVars: Set[ConsumableName]
    , modes: Set[(Active,Name,List[GroundValue])]
    ): Gen[(Set[Action], Set[ConsumableName])] =
   for {
      nbrDiscrAssgsToUse  <- chooseNum[Int](1, unassignedVars.size) // How many to use in this scope
      (now,later)         =  unassignedVars.toList.splitAt(nbrDiscrAssgsToUse)
      discreteAssignments <- genDiscreteAssignments(now.toSet, modes)
      updatedNames        =  now.map(n => UsedName(n.name)).toSet union later.toSet
    } yield if (isTopLevel) (Set(), unassignedVars) else (discreteAssignments, updatedNames)
    
  def genIfThenElse
    ( equationNames: Set[ConsumableName]
    , varNames: Set[ConsumableName]
    , modes: Set[(Active,Name,List[GroundValue])]
    , time: (Name, Interval)
    , nesting: Int
    ): Gen[Action] =
    for {
      cond       <- genPredicate(varNames.map(_.name), time)
      thenClause <- genActions(isTopLevel = false, equationNames, varNames, modes, time, nesting + 1)
      elseClause <- genActions(isTopLevel = false, equationNames, varNames, modes, time, nesting + 1)
    } yield IfThenElse(cond, thenClause.toList, elseClause.toList)
  
  def genSwitch
    ( equationNames: Set[ConsumableName]
    , varNames: Set[ConsumableName]
    , modes: Set[(Active,Name,List[GroundValue])]
    , time: (Name, Interval)
    , nesting: Int
    ): Gen[Action] =
    for {
      m@(active, name, vs) <- oneOf(modes.toList)
      mA                   =  (true, name, vs)
      clauses              <- listOfN(vs.size, 
                                genActions( isTopLevel = true
                                          , equationNames
                                          , varNames
                                          , modes - m + mA
                                          , time
                                          , nesting + 1
                                          ))
      assertion            =  Lit(GBool(true)) // TODO Generate mode invariants
    } yield Switch(name, vs.zip(clauses).map{ case (v, as) => Clause(v, assertion, as.toList) })
  
  def genDiscreteAssignments(varNames: Set[ConsumableName], modes: Set[(Active,Name,List[GroundValue])]): Gen[Set[Action]] =
    for {
      lhss           <- resize(varNames.size / 4, someOf(varNames))
      rhss           <- listOfN(lhss.size, genSmallDouble).map(_.map(d => Lit(GDouble(d)))) //TODO More interesting, state-based resets
      activeModeVars =  modes.toList.filter { case (active, _, _) => active }
      modeResets     <-  if (activeModeVars.isEmpty) value(Set[Action]()) 
                         else for {
                           (_, m, vs)  <- oneOf(activeModeVars)
                           mAfterReset <- oneOf(vs)
                         } yield Set[Action](Discretely(Assign(Var(m), Lit(mAfterReset))))
    } yield {
      val nonModeResets: Set[Action] = ((lhss zip rhss) map { case (cn, e) => Discretely(Assign(Var(cn.name), e)) }).toSet
      if (activeModeVars.nonEmpty) nonModeResets union modeResets 
      else nonModeResets
    }
    
  /**
   * Generates initialization statements for the input variable names.
   * NOTE: Assumes that the initial conditions for differential equations can be chosen arbitrarily!      
   */
  def genPrivateSection(varNames: Set[ConsumableName], modes: Set[(Active,Name,List[GroundValue])]): Gen[Set[Init]] = {
    val unassignedVarNames = varNames.filter(_.isFree)
    for { //TODO Generate more interesting initial conditions!
      varRhss   <- listOfN(unassignedVarNames.size, genSmallDouble)
      varInits  =  (unassignedVarNames zip varRhss).map { case (l, r) => Init(l.name, ExprRhs(Lit(GDouble(r)))) }
      modeInits =  modes.map{ case (_active, n, vs) => Init(n, ExprRhs(Lit(vs(0)))) }
    } yield varInits union modeInits
  }
  
  /**
   * Generate set of distinct variable Names that is complete, in the sense that if the set 
   * contains a Name(n,primes) it will also contain all Name(n,p), where 0 <= p < primes.
   */
  def genCompleteNameSet(minSize: Int): Gen[Set[Name]] =
    for { initial <- genDistinctSetOfN(minSize, arbitrary[Name]) } yield completeNames(initial)

}