package acumen
package testutil

import org.scalacheck._
import Gen._
import Arbitrary.arbitrary
import org.scalacheck.Properties
import org.scalacheck.Prop._
import acumen.Pretty.pprint
import acumen.testutil.Generators.{ 
  arbName, completeNames, genSetBasedOn, genDistinctSetOfN, genSmallDouble
}
import scala.Array.{
  ofDim
}
import acumen.interpreters.enclosure.TestingContext.{
  rnd
}
import acumen.interpreters.enclosure.Generators.{
  genSubInterval
}
import acumen.interpreters.enclosure.Box
import acumen.interpreters.enclosure.Interval
import acumen.interpreters.enclosure.Types
import acumen.interpreters.enclosure.Variable

/**
 * Class for generating systems of ODEs which are guaranteed to have (known) solutions.
 * 
 * This is useful when generating Acumen models which can be seen as encodings of piecewise 
 * systems of differential equations.
 */
object ODESystemGenerator extends acumen.interpreters.enclosure.Extract {

  /**
   * An equation system here means a set of equations which is guaranteed to have 
   * a solution, meaning the solution is finite over the whole simulation.
   * 
   * Precondition: The names must be distinct in the .x field.
   */
  def genLinearODESystem(lhss: Set[Name]): Gen[Set[Equation]] = {
    require(lhss.forall(n => lhss.filter(_.x == n.x).size == 1))
    val possibleTerms = completeNames(lhss)
    genSetBasedOn(lhss, genLinearODE(possibleTerms))
  } 
  
  /**
   * Generate a linear first-order ODE with terms from names and constant coefficients.
   * Terms will be chosen from names (lhs will be omitted).
   */
  def genLinearODE(names: Set[Name])(lhs: Name): Gen[Equation] = {
    val validNames = names - lhs // Will be used in RHS of equation
    for {
      terms <- someOf(validNames)
      coefficients <- listOfN(validNames.size, genSmallDouble)
    } yield Equation(lhs, univariatePolynomial(coefficients zip terms))
  }
    
  /* Event generator */
  
  /**
   * Generate a boolean expression (in timeVar) which will change its value 
   * at some point during the simulation.
   */
  def genEventfulTimeBasedPredicate(time: (Name, Interval)): Gen[Expr] =
  for { //FIXME Make sure this does not trigger at time 0
    subRange <- genSubInterval(time._2)
  } yield and(lessThan(subRange.loDouble, time._1), 
              lessThan(time._1, subRange.hiDouble))

  def genPredicate(varNames: Set[Name], time: (Name, Interval)): Gen[Expr] =
    frequency( 3 -> genEventfulTimeBasedPredicate(time)
             , 1 -> genBinaryRelation(varNames, time)
             , 2 -> genLogicalOp(varNames, time)
             )

  def genOp(ops: List[String], argGen: => Gen[Expr]) =
    for {
      op   <- oneOf(ops)
      args <- listOfN(2, argGen)
    } yield Op(Name(op, 0), args)
    
  def genBinaryOp(varNames: Set[Name], time: (Name, Interval)): Gen[Op] =
    genOp(List("+", "-", "*"), genExpr(varNames, time)) //TODO Add support for /, vectors etc. 
    
  def genBinaryRelation(varNames: Set[Name], time: (Name, Interval)): Gen[Op] =
    genOp(List("<", ">", "<=", ">="), genExpr(varNames, time)) // Skipping "=="

  def genLogicalOp(varNames: Set[Name], time: (Name, Interval)): Gen[Op] =
    genOp(List("||", "&&"), genPredicate(varNames, time))
    
  def genExpr(varNames: Set[Name], time: (Name, Interval)): Gen[Expr] =
    frequency( 1 -> arbGDouble
             , 2 -> Var(time._1)
             , 1 -> oneOf(varNames.toList).map(Var)
             , 1 -> genBinaryOp(varNames, time)
             )

  def arbGDouble = genSmallDouble.map(d => Lit(GDouble(d))) 
    
  /* Utilities */

  def linearCombination(terms: List[(Double, Expr)]): Expr =
    terms.foldLeft(0: Expr) { case (e, (c, t)) => sum(powerProduct(c, t), e) }

  def univariatePolynomial(terms: List[(Double, Name)]): Expr =
    linearCombination(terms.map { case (d, n) => (d, Var(n)) })
  
  def exp(e: Expr) = Op(Name("exp", 0), List(e))

  def powerProduct(d: Double, e: Expr) = op("*", d, e)
  
  def sum(l: Expr, r: Expr) = op("+", l, r)

  def pow(l: Expr, r: Expr) = op("^", l, r)
  
  def and(l: Expr, r: Expr) = op("&&", l, r)

  def or(l: Expr, r: Expr) = op("||", l, r)

  def lessThan(l: Expr, r: Expr) = op("<", l, r)

  def op(o: String, l: Expr, r: Expr) = Op(Name(o, 0), List(l, r))
  
  implicit def doubleToLit(d: Double): Expr = Lit(GDouble(d))

  implicit def nameToVar(n: Name): Expr = Var(n)
  
}