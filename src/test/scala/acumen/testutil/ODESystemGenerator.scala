package acumen
package testutil

import org.scalacheck._
import Gen._
import Arbitrary.arbitrary

import org.scalacheck.Properties
import org.scalacheck.Prop._

import acumen.Pretty.pprint

import acumen.testutil.Generators.{ 
  arbName, genSetBasedOn, genDistinctSetOfN, genSmallDouble
} 

import scala.Array.{ ofDim }

/**
 * Class for generating systems of ODEs which are guaranteed to have (known) solutions.
 * 
 * This is useful when generating Acumen models which can be seen as encodings of piecewise 
 * systems of differential equations.
 * 
 * 1. Generate complete set of n distinct variable names (e.g. x, x', y, with n = 3).
 * 2. For each variable with Name(n,primes) generate a linear ODE with constant coefficients:
 *    a. For all Name(m_i,p_i), with 0 <= i < primes, generate a Double coefficient c_i.
 *    b. Form an Equation with LHS equal to Name(,primes) and take as LHS the sum (linear 
 *       combination) of the coefficients with the corresponding Name(m_i,p_i), i.e. 
 *       
 *         Name(n,primes) = sum(c_i * Name(m_i, p_i))
 *
 * 3. The set of these equations is guaranteed to have a (real) solution, a linear combination
 *    of exponential functions with coefficients that are multiples of the initial conditions,
 *    which may be chosen arbitrarily.
 */
object ODESystemGenerator extends Properties("ODESystemGenerator") {

  /**
   * An equation system here means a set of equations which is guaranteed to have 
   * a solution, meaning the solution is finite over the whole simulation.
   * 
   * Precondition: The names must be distinct in the .x field. 
   */
  def genLinearODESystem(distinctNames: Set[Name]): Gen[Set[Equation]] = {
    require(distinctNames.forall(n => distinctNames.filter(_.x == n.x).size == 1))
    genSetBasedOn(distinctNames, genLinearODE)
  } 
  
  def genLinearODE(lhs: Name): Gen[Equation] =
    for {
      coefficients <- listOfN(lhs.primes, genSmallDouble)
      terms = (0 to lhs.primes).map(Name(lhs.x, _))
    } yield Equation(Var(lhs), linearCombination(coefficients zip terms))

  /* Utilities */

  def linearCombination(terms: List[(Double, Name)]): Expr =
    terms.foldLeft(Lit(GDouble(0)): Expr) { case (e, (c, t)) => sum(powerProduct(c, t), e) }
  
  def powerProduct(d: Double, n: Name) = Op(Name("*", 0), List(Lit(GDouble(d)), Var(n)))
  
  def sum(l: Expr, r: Expr) = Op(Name("+", 0), List(l, r))
  
}