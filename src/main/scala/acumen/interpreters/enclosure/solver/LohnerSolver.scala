package acumen.interpreters.enclosure.solver

import acumen.interpreters.enclosure.Box
import acumen.interpreters.enclosure.Field
import acumen.interpreters.enclosure.Interval
import acumen.interpreters.enclosure.Rounding
import acumen.interpreters.enclosure.Types.{ VarName }
import acumen.interpreters.enclosure.affine.UnivariateAffineEnclosure
import acumen.interpreters.enclosure.Expression
import acumen.interpreters.enclosure.Variable
import acumen.interpreters.enclosure.Constant

trait LohnerSolver extends SolveIVP {

  /**
   * TODO add description!
   */
  def solveVt(
    F: Field, // field
    T: Interval, // domain of t
    A: Box, // (A1,...,An), initial condition
    delta: Double, // padding 
    m: Int, // extra iterations after inclusion of iterates
    n: Int, // maximum number of iterations before inclusion of iterates
    degree: Int // number of pieces to split each initial condition interval
    )(implicit rnd: Rounding): (UnivariateAffineEnclosure, Box) = null // TODO unimplemented method

  // Taylor remainder of the given order and expansion for e around x
  def taylorRemainder(order: Int, e: Expression)(x: Box)(implicit rnd: Rounding): Expression = {
    val varNames = e.varNames
    def abs(multiIndex: Map[VarName, Int]): Int = multiIndex.values.sum
    // FIXME: replace this solution with one that scales
    def nextIndexes(indexes: Set[Map[VarName, Int]]): Set[Map[VarName, Int]] =
      indexes.flatMap(index => varNames.map(varName => index + (varName -> (index(varName) + 1))))
    var indexes: Set[Map[VarName, Int]] = Set(varNames.map((_ -> 0)).toMap)
    for (_ <- 0 until order) indexes = nextIndexes(indexes)
    var res: Expression = 0
    for (index <- indexes) res += taylorTerm(index, e)(x)
    res
  }

  // Taylor expansion of the given order for e around x
  def taylorPolynomial(order: Int, e: Expression)(x: Box)(implicit rnd: Rounding): Expression = {
    val varNames = e.varNames
    def abs(multiIndex: Map[VarName, Int]): Int = multiIndex.values.sum
    // FIXME: replace this solution with one that scales
    def nextIndexes(indexes: Set[Map[VarName, Int]]): Set[Map[VarName, Int]] =
      indexes.flatMap(index => varNames.map(varName => index + (varName -> (index(varName) + 1))))
    var indexes: Set[Map[VarName, Int]] = Set(varNames.map((_ -> 0)).toMap)
    var res: Expression = 0
    for (_ <- 0 to order) {
      for (index <- indexes) res += taylorTerm(index, e)(x)
      indexes = nextIndexes(indexes)
    }
    res
  }

  // Term in Taylor expansion for e around x
  def taylorTerm(multiIndex: Map[VarName, Int], e: Expression)(x: Box)(implicit rnd: Rounding): Expression = {
    var coeff = e
    var monic: Expression = 1
    for ((varName, order) <- multiIndex)
      for (o <- 1 to order) {
        coeff = coeff.dif(varName) / o
        monic *= Variable(varName) - x(varName)
      }
    coeff(x) * monic
  }

  def partialDerivative(multiIndex: Map[VarName, Int], e: Expression)(implicit rnd: Rounding): Expression = {
    var res = e
    for ((varName, order) <- multiIndex)
      for (_ <- 1 to order)
        res = res.dif(varName)
    res
  }

}

object LohnerSolverApp extends LohnerSolver with App {

  implicit val rnd = Rounding(10)
  val x = Variable("x")
  val y = Variable("y")
  val e = x * x
  val a = Map("x" -> Interval(1))
  val b = Map("x" -> Interval(0.9,1.1))
  val t = taylorPolynomial(1, e)(a)
  val r = taylorRemainder(2, e)(a)

  println(t)
  println(r)
  println(r(b))
  println(t+r(b))

}

