package acumen.interpreters.enclosure.strategy

import acumen.interpreters.enclosure.EnclosureInterpreterCallbacks
import acumen.interpreters.enclosure.HybridSystem
import acumen.interpreters.enclosure.Interval
import acumen.interpreters.enclosure.Parameters
import acumen.interpreters.enclosure.Rounding
import acumen.interpreters.enclosure.StateEnclosure
import acumen.interpreters.enclosure.affine.UnivariateAffineEnclosure
import acumen.interpreters.enclosure.event.EventEncloser

trait Strategy {

  val eventEncloser: EventEncloser

  /**
   * Sub-divides the simulation time interval `t` according to a given strategy
   * applying the IVP solver and event handler to produce valid enclosures for
   * each sub-interval.
   */
  def enclosePiecewise(
    ps: Parameters, // parameters for controlling event handling and IVP solving
    h: HybridSystem, // the system
    t: Interval, // the time interval
    s: StateEnclosure, // initial states at t.low
    cb: EnclosureInterpreterCallbacks // handles communication with GUI
    )(implicit rnd: Rounding): Seq[UnivariateAffineEnclosure]

}
