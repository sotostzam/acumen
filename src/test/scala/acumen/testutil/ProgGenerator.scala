package acumen
package testutil

import org.scalacheck._
import Gen._
import Shrink._
import Arbitrary.arbitrary
import org.scalacheck.Properties
import org.scalacheck.Prop._
import acumen.Pretty.pprint
import acumen.testutil.Generators.{ 
  arbName, genSetBasedOn, genCompliantSetOfN, genDistinctSetOfN, genSmallDouble
}
import ODESystemGenerator.{
  genLinearODESystem, genPredicate, doubleToLit, nameToVar
}
import acumen.interpreters.enclosure.Interval
import acumen.interpreters.enclosure.TestingContext.{
  rnd
}

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
object ProgGenerator extends Properties("Prog") {
  
  type Unassigned = Boolean
  type Active = Boolean
  
  property("extract is semantics preserving") = {
    forAll { (p: Prog) =>
      try {
        println(pprint(p) + "\n\n")
      }
      catch { case e => e.printStackTrace; throw e }
      true
    }
  }
  
  implicit def arbProg: Arbitrary[Prog] = Arbitrary { genProg }
  
  // TODO Implement generation of models with more than one class.
  /**
   * Basic algorithm:
   * 1. Generate list of leaf classes and add these to Prog.
   * 2. Randomize a small number n, and do the following n times:
   * 3. Generate list of classes, based on Prog, and add these to Prog.
   */
  def genProg: Gen[Prog] = {
    val emptyProg = Prog(Nil)
    for {
      main <-
        genClassDef( isLeaf = true
                   , isMain = true
                   , timeDomain = Interval(0, 10)
                   , emptyProg 
                   , oneOf(List(ClassName("Main")))
                   )
    } yield Prog(List(main))
  }
  
  /**
   * Generate a class definition.
   * 
   * TODO If isLeaf is set to true, no constructor calls or child (Dot) references 
   * will be generated.
   */
  def genClassDef( isLeaf: Boolean
                 , isMain: Boolean
                 , timeDomain: Interval
                 , p: Prog
                 , classNameGen: => Gen[ClassName]
                 ): Gen[ClassDef] =
    for {
      name                <- classNameGen
      minVars             <- chooseNum(2,5)
      (timeVar, timeVarD) =  (Name("t", 0), Name("t", 1))
      distinctVarNames    <- genCompliantSetOfN(minVars, 
                                                arbitrary[Name], 
                                                (candidate: Set[Name], extension: Set[Name]) => 
                                                  extension.forall(n => !candidate.contains(n) && 
                                                                        n != timeVar && 
                                                                        n != timeVarD) )
      distinctVarNamesT   =  distinctVarNames + timeVar + timeVarD
      minModeVars         <- chooseNum(0,4)
      modeNames           <- genModeVarNames(minModeVars, distinctVarNamesT)
      modesPerName        <- listOfN(modeNames.size, choose[Int](2, 5))
      modes               =  (modeNames zip modesPerName.map(n => (0 to n).toList.map(GInt): List[GroundValue])).
                             map { case (m, n) => (false, m, n) }
      varNames            =  completeNames(distinctVarNames)
      numberOfFields      <- chooseNum[Int](0: Int, if (isMain || varNames.isEmpty) 0 else varNames.size)
      (fields, privNames) =  varNames.splitAt(numberOfFields)
      privs               <- genPrivateSection(privNames.map((true,_)), modes)
      privsT              =  Init(timeVar, ExprRhs(0)) +: Init(timeVarD, ExprRhs(1)) +: privs.toList 
      equations           <- genLinearODESystem(distinctVarNames)
      actions             <- genActions(isTopLevel = true, equations, (privNames ++ fields).map((true,_)), modes, (timeVar, timeDomain))
      actionsT            =  Continuously(Equation(timeVarD, 1)) +: actions.toList 
    } yield ClassDef(name, if (isMain) List(Name("simulator",0)) else fields.toList, privsT, actionsT)

  def genModeVarNames(minAmount: Int, banned: Set[Name]) =
    genCompliantSetOfN(minAmount, for { n <- arbitrary[Name] } yield Name(n.x, 0),
      (candidate: Set[Name], compliant: Set[Name]) =>
        candidate.forall(e => !compliant.contains(e) && !banned.contains(e)))
    
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
  // TODO Add discrete assignments
  def genActions( isTopLevel: Boolean 
                , availableEquations: Set[Equation]
                , varNames: Set[(Unassigned,Name)]
                , modes: Set[(Active,Name,List[GroundValue])]
                , time: (Name,Interval)
                ): Gen[Set[Action]] =
    if (availableEquations.isEmpty) Set[Action]()
    else for {
      nbrEquationsToUse          <- chooseNum[Int](1, availableEquations.size) // How many to use in this scope
      (now,later)                =  availableEquations.toList.splitAt(nbrEquationsToUse)
      equations: Set[Action]     =  now.map(Continuously).toSet
      someEquations: Set[Action] <- someOf(equations.toList).map(_.toSet)
      es                         =  if (someEquations isEmpty) equations else someEquations
      (discrAs, updVarNames)     <- genDiscreteActions(isTopLevel, varNames.filter(_._1), modes)
      nbrConditionalStmts        <- chooseNum[Int](1, 3)
      nbrSwitchStmts             <- chooseNum[Int](0, 1)
      conditionals               <- listOfNIf(later.nonEmpty, nbrConditionalStmts, 
                                      genIfThenElse(later.toSet, updVarNames, modes, time))
      switches                   <- listOfNIf(later.nonEmpty, nbrSwitchStmts,
                                      genSwitch(later.toSet, updVarNames, modes, time))
    } yield es union conditionals.toSet union switches.toSet union discrAs
  
  def listOfNIf[E](cond: Boolean, size: Int, gen: Gen[E]): Gen[List[E]] =
    if (!cond) value(List()) else listOfN(size, gen)
    
  def genDiscreteActions(isTopLevel: Boolean, unassignedVars: Set[(Unassigned,Name)], modes: Set[(Active,Name,List[GroundValue])]): Gen[(Set[Action], Set[(Unassigned,Name)])] =
   for {
      nbrDiscrAssgsToUse  <- chooseNum[Int](1, unassignedVars.size) // How many to use in this scope
      (now,later)         =  unassignedVars.toList.splitAt(nbrDiscrAssgsToUse)
      discreteAssignments <- genDiscreteAssignments(now.toSet, modes)
    } yield if (isTopLevel) (Set(), unassignedVars) else (discreteAssignments, later.toSet)
    
  def genIfThenElse(availableEquations: Set[Equation], varNames: Set[(Unassigned,Name)], modes: Set[(Active,Name,List[GroundValue])], time: (Name, Interval)): Gen[Action] =
    for {
      cond       <- genPredicate(varNames.map(_._2), time)
      thenClause <- genActions(isTopLevel = false, availableEquations, varNames, modes, time)
      elseClause <- genActions(isTopLevel = false, availableEquations, varNames, modes, time)
    } yield IfThenElse(cond, thenClause.toList, elseClause.toList)
  
  def genSwitch(availableEquations: Set[Equation], varNames: Set[(Unassigned,Name)], modes: Set[(Active,Name,List[GroundValue])], time: (Name, Interval)): Gen[Action] =
    for {
      m@(active, name, vs) <- oneOf(modes.toList)
      mA                   =  (true, name, vs)
      clauses              <- listOfN(vs.size, genActions(isTopLevel = true, availableEquations, varNames, modes - m + mA, time))
      assertion            =  Lit(GBool(true)) // TODO Generate mode invariants
    } yield Switch(name, vs.zip(clauses).map{ case (v, as) => Clause(v, assertion, as.toList) })
  
  def genDiscreteAssignments(varNames: Set[(Unassigned,Name)], modes: Set[(Active,Name,List[GroundValue])]): Gen[Set[Action]] =
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
      val nonModeResets: Set[Action] = ((lhss zip rhss) map { case ((_,n), e) => Discretely(Assign(Var(n), e)) }).toSet
      if (activeModeVars.nonEmpty) nonModeResets union modeResets 
      else nonModeResets
    }
    
  /**
   * Generates initialization statements for the input variable names.
   * NOTE: Assumes that the initial conditions for differential equations can be chosen arbitrarily!      
   */
  def genPrivateSection(varNames: Set[(Unassigned,Name)], modes: Set[(Active,Name,List[GroundValue])]): Gen[Set[Init]] = {
    val unassignedVarNames = varNames.filter(_._1)
    for { //TODO Generate more interesting initial conditions!
      varRhss   <- listOfN(unassignedVarNames.size, genSmallDouble)
      varInits  =  (unassignedVarNames zip varRhss).map { case ((_unassinged,l), r) => Init(l, ExprRhs(Lit(GDouble(r)))) }
      modeInits =  modes.map{ case (_active, n, vs) => Init(n, ExprRhs(Lit(vs(0)))) }
    } yield varInits union modeInits
  }
  
  /**
   * Generate set of distinct variable Names that is complete, in the sense that if the set 
   * contains a Name(n,primes) it will also contain all Name(n,p), where 0 <= p < primes.
   */
  def genCompleteNameSet(minSize: Int): Gen[Set[Name]] =
    for { initial <- genDistinctSetOfN(minSize, arbitrary[Name]) } yield completeNames(initial)

  /**
   * Given a list of Names, completes the list by adding names with lower primes.
   * E.g. the input:
   *   List(Name(2,"x"), Name(0,"y")) 
   * yields
   *   List(Name(1,"x"), Name(1,"x"), Name(2,"x"), Name(0,"y"))  
   */
  def completeNames(l: Set[Name]): Set[Name] =
    l.flatMap(n => if(n.primes == 0) List(n) else for(p <- 0 to n.primes) yield Name(n.x, p))
  
  /* Properties for utility generators */
  // TODO Move these
  
  property("genCompleteNameSet") = 
    forAll(chooseNum[Int](0,50)) { (size: Int) =>
      forAll(genCompleteNameSet(size)) { (ns: Set[Name]) =>
        ns.forall(n => (0 to n.primes).forall(p => ns.contains(Name(n.x, p))))
      }
    }
    
  property("genDistinctSetOfN") =
    forAll(chooseNum[Int](0,10000)) { (size: Int) =>
      forAll(genDistinctSetOfN(size, choose(0,10000))) { (l: Set[Int]) =>
        l.size == l.toSet.size
      }
    }
     
  property("completeNames") =
    forAll { (l: Set[Name]) =>
      val cn = completeNames(l)
      cn.forall(n => (0 to n.primes).forall(p => cn.contains(Name(n.x, p))))
    } 
  
}