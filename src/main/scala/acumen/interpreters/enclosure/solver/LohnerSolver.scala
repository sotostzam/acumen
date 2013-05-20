package acumen.interpreters.enclosure.solver

import acumen.interpreters.enclosure.Box
import acumen.interpreters.enclosure.Constant
import acumen.interpreters.enclosure.Expression
import acumen.interpreters.enclosure.Field
import acumen.interpreters.enclosure.Types._
import acumen.interpreters.enclosure.Interval
import acumen.interpreters.enclosure.Multiply
import acumen.interpreters.enclosure.Negate
import acumen.interpreters.enclosure.Plus
import acumen.interpreters.enclosure.Rounding
import acumen.interpreters.enclosure.Variable
import acumen.interpreters.enclosure.affine.UnivariateAffineEnclosure
import scala.collection.mutable.ArrayBuffer
import acumen.interpreters.enclosure.affine.PaddedUnivariateAffineEnclosure
import acumen.interpreters.enclosure.affine.PaddedUnivariateAffineEnclosure

trait LohnerSolver extends PicardSolver {

  /**
   * TODO add description!
   */
  override def solveIVP(
    F: Field, // field
    T: Interval, // domain of t
    A: Box, // (A1,...,An), initial condition
    delta: Double, // padding 
    m: Int, // extra iterations after inclusion of iterates
    n: Int, // maximum number of iterations before inclusion of iterates
    degree: Int // number of pieces to split each initial condition interval
    )(implicit rnd: Rounding): (PaddedUnivariateAffineEnclosure, Box) = {

    val enclosure = super.solveIVP(F, T, A.midpoint, delta, m, n, degree)
    val (midpointEnclosure, midpointEndTimeBox) = enclosure
    val logNormBound = F.jacobianLogMaxNorm(A).high

    /**
     * Since we are taking the log norm w.r.t. the max norm, it is optimal
     * to pad each component with the full padding below.
     *
     * Note: for other norms it may be wasteful to scale with this padding
     * as the will account for the padding multiple times, e.g. in the case
     * of the L^1 norm, a weighted average of the padding can be used instead.
     */
    def padding(x: Interval): Interval = (logNormBound * x).exp * Interval(-0.5, 0.5) * A.maxNorm

    def paddedEnclosure = PaddedUnivariateAffineEnclosure(midpointEnclosure, padding)

    (paddedEnclosure, paddedEnclosure(T.high))
  }

}
