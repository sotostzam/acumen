package acumen.interpreters.enclosure.ivp.tree

import acumen.interpreters.enclosure.affine.UnivariateAffineEnclosure._
import acumen.interpreters.enclosure.Types._
import acumen.interpreters.enclosure._
import acumen.interpreters.enclosure.affine.UnivariateAffineEnclosure
import acumen.interpreters.enclosure.ivp.SolveIVP

trait SolveVtE {

  var ivpSolver: SolveIVP

  case class SolveVtEException(message: String) extends Exception

  // TODO add description
  def solveVtE(
    H: HybridSystem,
    T: Interval,
    S: UncertainState,
    delta: Double,
    m: Int,
    n: Int,
    degree: Int,
    K: Int,
    log: String => Unit)(implicit rnd: Rounding): Option[(Set[UncertainState], Seq[UnivariateAffineEnclosure])] = {
    var res = EventTree.initialTree(T, H, S, delta, m, n, degree, ivpSolver)
    var tmp = res.addLayer(ivpSolver)
    while (res.size < K && tmp != res) {
      res = tmp
      tmp = tmp.addLayer(ivpSolver)
    }
    if (res != tmp) {
      sys.error("gave up for maxEventTreeSize " + K + " at " + T)
      None
    }
    else
      Some((res.endTimeStates, res.prunedEnclosures))
  }

}
