package acumen.interpreters.enclosure.solver

import acumen.interpreters.enclosure.Box
import acumen.interpreters.enclosure.Constant
import acumen.interpreters.enclosure.Expression
import acumen.interpreters.enclosure.Field
import acumen.interpreters.enclosure.Interval
import acumen.interpreters.enclosure.Multiply
import acumen.interpreters.enclosure.Negate
import acumen.interpreters.enclosure.Plus
import acumen.interpreters.enclosure.Rounding
import acumen.interpreters.enclosure.Variable
import acumen.interpreters.enclosure.affine.UnivariateAffineEnclosure
import scala.collection.mutable.ArrayBuffer

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

}

object LohnerSolverApp extends LohnerSolver with App {

  implicit val rnd = Rounding(10)

}

