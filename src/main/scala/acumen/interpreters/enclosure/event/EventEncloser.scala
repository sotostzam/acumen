package acumen.interpreters.enclosure.event

import acumen.interpreters.enclosure.HybridSystem
import acumen.interpreters.enclosure.Interval
import acumen.interpreters.enclosure.Parameters
import acumen.interpreters.enclosure.Rounding
import acumen.interpreters.enclosure.StateEnclosure
import acumen.interpreters.enclosure.ivp.IVPSolver
import acumen.interpreters.enclosure.affine.UnivariateAffineEnclosure

trait EventEncloser {

  var ivpSolver: IVPSolver // IVP solver

  /**
   * Computes two StateEnclosures for the system `h`.
   *
   * The first encloses the system with all events accounted for over the entire interval `t`.
   * The second encloses the system at the end time t.high, generally providing a tighter
   * approximation than the first enclosure.
   */
  def handleEvents(
    ps: Parameters, // parameters for controlling event handling and IVP solving
    h: HybridSystem, // the system
    t: Interval, // the time interval
    s: StateEnclosure // initial states at t.low
    )(implicit rnd: Rounding): (StateEnclosure, StateEnclosure)

  def significantImprovement(eOld: UnivariateAffineEnclosure, eNew: UnivariateAffineEnclosure, x: Interval, minComputationImprovement: Double)(implicit rnd: Rounding) = {
    val normOld = eOld(x).l1Norm
    val normNew = eNew(x).l1Norm
    normOld - normNew greaterThan minComputationImprovement
  }

}