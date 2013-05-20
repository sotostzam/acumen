package acumen.interpreters.enclosure.solver

import acumen.interpreters.enclosure.{Rounding,Interval,Field,Box}
import acumen.interpreters.enclosure.affine.UnivariateAffineEnclosure

trait SolveIVP {

  /* TODO add description! */
  def solveIVP(
    F: Field, // field
    T: Interval, // domain of the independent variable
    A: Box, // initial condition
    delta: Double, // padding 
    m: Int, // extra iterations after inclusion of iterates
    n: Int, // maximum number of iterations before inclusion of iterates
    degree: Int // number of pieces to split each initial condition interval
    )(implicit rnd: Rounding): (UnivariateAffineEnclosure, Box)

}