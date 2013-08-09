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
  arbName, genSetBasedOn, genDistinctSetOfN, genSmallDouble
}
import ODESystemGenerator.{
  genLinearODESystem
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
 *  
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

 property("testTheGenerator") = {
    val emptyProg = Prog(Nil)
    forAllNoShrink(genProg) { p =>
      println(pprint(p) ++ "\n")
      true
    }
  }

  implicit def arbProg: Arbitrary[Prog] = Arbitrary { genProg }
  
  //TODO Implement generation of models with more than one class.
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
                 , p: Prog
                 , classNameGen: => Gen[ClassName]
                 ): Gen[ClassDef] =
    for {
      name                <- classNameGen
      minVars             <- chooseNum(2,5)
      distinctNames       <- genDistinctSetOfN(minVars, arbitrary[Name])
      allNames            =  completeNames(distinctNames)
      numberOfFields      <- chooseNum[Int](0: Int, if (isMain || allNames.isEmpty) 0 else allNames.size)
      (fields, privNames) =  allNames.splitAt(numberOfFields)
      privs               <- genPrivateSection(privNames)
      equations           <- genLinearODESystem(distinctNames)
      actions             =  equations.toList.map(Continuously) // TODO Add discrete actions
    } yield ClassDef(name, if (isMain) List(Name("simulator",0)) else fields.toList, privs.toList, actions) 

  /**
   * Generates initialization statements for the input variable names.
   * NOTE: Assumes that the initial conditions for differential equations can be chosen arbitrarily!      
   */
  def genPrivateSection(varNames: Set[Name]): Gen[Set[Init]] =
    for { //TODO Generate more interesting initial conditions!
      rhss: List[Double] <- listOfN(varNames.size, genSmallDouble)
    } yield (varNames zip rhss).map { case (l, r) => Init(l, ExprRhs(Lit(GDouble(r)))) }
  
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