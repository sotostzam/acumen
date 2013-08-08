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
  arbName, arbClassName, arbIDoubleLit
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
object ProgGenerator extends Properties("ProgGenerator") {

  case class Conf( isLeaf: Boolean
                 , isPriv: Boolean
                 , banned: List[Name]
                 , p: Prog
                 , className: ClassName
                 , fields: List[Name]
                 , privs: List[Init]
                 )

  /**
   * TODO implement basic algorithm:
   * 1. Generate list of leaf classes and add these to Prog.
   * 2. Randomize a small number n, and do the following n times:
   * 3. Generate list of classes, based on Prog, and add these to Prog.
   */
  def genProg: Gen[Prog] = {
    val emptyProg = Prog(Nil)
    for {
      main <-
        genClassDef( isLeaf = true //false
                   , emptyProg //Prog(leaves) 
                   , oneOf(List(ClassName("Main")))
                   , listOfN(1, Name("simulator", 0))
                   )
    } yield Prog(List(main)) //+: leaves)
  }
  
  /**
   * Based on classes already defined in Prog, generate a ClassDef with a distinct
   * ClassName, possibly with children that are instances of the already
   * defined classes.
   * If isLeaf, do not generate constructor calls or Dots in Exprs.
   */
  def genClassDef( isLeaf: Boolean
                 , p: Prog
                 , classNameGen: => Gen[ClassName]
                 , fieldGen: => Gen[List[Name]]
                 ): Gen[ClassDef] =
    for {
      name       <- classNameGen
      fields     <- fieldGen
      numPrivs   <- chooseNum(0,4)
      privLhs_   <- listOfN(numPrivs, arbitrary[Name]) suchThat (!fields.contains(_)) 
      privLhss   =  completeNames(privLhs_) // Ensure that all names are initialized
      conf       =  Conf(isLeaf, true, Nil, p, name, fields, Nil)
      privRhss   <- listBasedOn(privLhss, genInitRhs(conf)_)
      privs      =  privLhss.zip(privRhss).map{ case (l,r) => Init(l,r) }
      numActions <- chooseNum(2,5) 
      actions    <- listOfN(numActions, genAction(conf.copy(privs = privs)))
    } yield ClassDef(name, fields, privs, actions) 
  
  /**
   * Generates a list of n elements using genElem, each time passing a new 
   * element of the input list to genElem.
   */
  def listBasedOn[E, T](params: List[T], gen: T => Gen[E]): Gen[List[E]] =
    if (params.isEmpty) Nil
    else for {
      head <- gen(params.head)
      tail <- listBasedOn(params.tail, gen)
    } yield head +: tail
    
  def genInitRhs(conf: Conf)(lhs: Name): Gen[InitRhs] = {
    val exprGen = genExpr(conf.copy(banned = List(lhs))).map(ExprRhs) //TODO banned OK?
    //TODO Consider randomly creating a ClassDef here if conf.p.defs.isEmpty
    if (conf.isLeaf || conf.p.defs.isEmpty) exprGen else oneOf(exprGen, genNewRhs(conf))
  }
  
  def genNewRhs(conf: Conf): Gen[InitRhs] =
    for {
      className <- oneOf(conf.p.defs.map(_.name))
      classFields <- listOf(genExpr(conf))
    } yield NewRhs(className, classFields)
             
  def genFreshClassName(p: Prog): Gen[ClassName] =
    for { 
      n <- arbitrary[ClassName] suchThat (!p.defs.map(_.name).contains(_)) 
    } yield n

  /**
   * If isPriv, generate an Expr that can be used to initialize a private variable.
   * Currently, this means that it is not possible for this Expr to contain
   * identifiers declared in the same private section, but can only consist 
   * of field names, operators and literals.
   */
  def genExpr(conf: Conf): Gen[Expr] = {
    val litGen      = chooseNum(-10000.0,10000.0).map(n => Lit(GDouble(n)))
    val bopGen      = genBinaryOp(conf)
    val identifiers = censor(conf.fields, conf) ++ 
                      (if (conf.isPriv) censor(conf.privs.map(_.x), conf) else Nil)
    val dottables   = conf.privs.filter(_.rhs.isInstanceOf[NewRhs])
    //FIXME Make neater using varargs conversion
    if (identifiers.nonEmpty) {
      val idGen = oneOf(identifiers.map(Var))
      if (conf.isLeaf || dottables.isEmpty)
        oneOf(litGen, bopGen, idGen)
      else
        oneOf(litGen, bopGen, idGen, genDot(conf))
    }
    else
      if (conf.isLeaf || dottables.isEmpty)
        oneOf(litGen, bopGen) 
      else
        oneOf(litGen, bopGen, genDot(conf))
  }

  /**
   * FIXME Incomplete version. This will only generate qualified names based
   * on objects created in the private section (privs).
   */
  def genDot(conf: Conf): Gen[Dot] = {
    val objectsInScope = conf.privs.filter(_.rhs.isInstanceOf[NewRhs])
                                   .map(p => (p.x, p.rhs.asInstanceOf[NewRhs].c))
    for {
      (objName, className) <- oneOf(objectsInScope)
      val classDef = conf.p.defs.find(_.name == className).head
      fieldName <- oneOf(classDef.fields)
    } yield Dot(Var(objName), fieldName)
  }
  /**
   * Given a list of Names, completes the list by adding names with lower primes.
   * E.g. the input:
   *   List(Name(2,"x"), Name(0,"y")) 
   * yields
   *   List(Name(1,"x"), Name(1,"x"), Name(2,"x"), Name(0,"y"))  
   */
  def completeNames(l: List[Name]): List[Name] =
    l.flatMap(n => if(n.primes == 0) List(n) else for(p <- 0 to n.primes) yield Name(n.x, p))
     .toSet.toList

  property("completeNames") =
    forAll { (l: List[Name]) =>
      val cn = completeNames(l)
      cn.forall{ n => (0 to n.primes).forall(p => cn.contains(Name(n.x, p))) }
    }

  /**
   * FIXME Incomplete version including only the most common statements.
   * Missing at least the following: ForEach, Discretely
   */
  def genAction(conf: Conf): Gen[Action] =
//    oneOf( genIfThenElse(conf)
//         , genSwitch(p, fields, privs) //TODO
          genContinuously(conf)
//         )

  def genIfThenElse(conf: Conf): Gen[IfThenElse] =
    for {
      cond       <- genPredicate(conf)
      thenClause <- listOf1(genAction(conf))
      elseClause <- listOf1(genAction(conf))
    } yield IfThenElse(cond, thenClause, elseClause)
  
  def genPredicate(conf: Conf): Gen[Expr] =
    oneOf( Lit(GBool(true))
         , Lit(GBool(false)) 
         , genBinaryRelation(conf)
         , genLogicalOp(conf)
         )

  def genOp(ops: List[String], argGen: => Gen[Expr]) =
    for {
      op   <- oneOf(ops)
      args <- listOfN(2, argGen)
    } yield Op(Name(op, 0), args)
    
  def genBinaryOp(conf: Conf): Gen[Op] =
    genOp(List("+", "-", "*", "/"), genExpr(conf)) //TODO Add support for vectors etc. 
    
  def genBinaryRelation(conf: Conf): Gen[Op] =
    genOp(List("<", ">", "<=", ">=", "==", "~="), genExpr(conf))

  def genLogicalOp(conf: Conf): Gen[Op] =
    genOp(List("||", "&&"), genPredicate(conf))

  /** 
   * Used to track which variables already occur as LHS in an equation to avoid duplicates.
   * NOTE: Not thread safe! This test should not be run in parallel.
   */
  val continuousBindings = scala.collection.mutable.Set[(ClassName, Name)]()
  
  def genContinuously(conf: Conf): Gen[Action] = {
    val names = conf.fields ++ conf.privs.map(_.x)
    val censoredNames = censor(names, conf)
    val maxPrimeNames = censoredNames.filter{ n => !censoredNames.exists(_.primes > n.primes) }
    val okNames = maxPrimeNames.filter{ n => !continuousBindings.contains((conf.className, n)) }
    for {
      lhs @ Name(name, primes) <- oneOf(okNames)
      val _ = continuousBindings.add(conf.className, lhs)
      bannedRhsNames = names.filter(n => n.x == name && n.primes >= primes)  
      rhs <- genExpr(conf.copy(banned = conf.banned ++ bannedRhsNames))
    } yield Continuously(Equation(Var(lhs), rhs))
  }
  
  def genSwitch(conf: Conf): Gen[Switch] =
    oneOf(Nil)
    
  /** Remove from idList variable names incompatible with the expression to be generated. */
  //FIXME Guide generation depending on the types of values associated with identifiers
  //      e.g. generate Dots for variable names corresponding to objects.
  def censor(idList: List[Name], c: Conf) =  
    idList.filter(i => i.x != "simulator" && !c.banned.contains(i))
}