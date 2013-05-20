package acumen.interpreters.enclosure.ivp

import acumen.interpreters.enclosure.Box
import acumen.interpreters.enclosure.Field
import acumen.interpreters.enclosure.Interval
import acumen.interpreters.enclosure.Rounding
import acumen.interpreters.enclosure.affine.UnivariateAffineEnclosure

trait VeroSolver extends SolveIVP {

  /**
   * TODO add description!
   */
  def solveIVP(
    F: Field, // field
    T: Interval, // domain of t
    A: Box, // (A1,...,An), initial condition
    delta: Double, // padding 
    m: Int, // extra iterations after inclusion of iterates
    n: Int, // maximum number of iterations before inclusion of iterates
    degree: Int // number of pieces to split each initial condition interval
    )(implicit rnd: Rounding): (UnivariateAffineEnclosure, Box) =

    null // TODO unimplemented method

}