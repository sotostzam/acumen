package acumen.interpreters.enclosure.solver

import acumen.interpreters.enclosure._
import acumen.interpreters.enclosure.Types._

trait SolveVtE {

  // TODO add description
  def detectNextEvent(
    H: HybridSystem,
    T: Interval,
    q: Mode,
    Y: UnivariateAffineEnclosure)(implicit rnd: Rounding): Outcome = {
    val events = H.events.filter(e => e.sigma == q && H.guards(e)(Y.range) != Set(false))
    if (events.isEmpty) {
      MaybeOneOf(events)
    } else {
      if (H.domains(q)(Y(Y.domain.high)) == Set(false)) CertainlyOneOf(events)
      else MaybeOneOf(events)
    }
  }

  // TODO add description
  def solveVtE(
    H: HybridSystem,
    T: Interval,
    S: UncertainState,
    delta: Double,
    m: Int,
    n: Int,
    K: Int,
    output: String,
    log: String => Unit)(implicit rnd: Rounding): Option[(Set[UncertainState], Seq[UnivariateAffineEnclosure])] = {
    var res = EventTree.initialTree(T, H, S, delta, m, n, output)
    var tmp = res.addLayer
    while (res.size < K && tmp != res) {
      res = tmp
      tmp = tmp.addLayer
    }
    if (res != tmp)
      None
    else
      Some((res.endTimeStates, res.prunedEnclosures))
//      Some((res.endTimeStates, res.enclosures))
  }

}
