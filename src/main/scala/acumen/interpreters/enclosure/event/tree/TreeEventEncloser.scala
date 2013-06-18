package acumen.interpreters.enclosure.event.tree

import acumen.interpreters.enclosure.affine.UnivariateAffineEnclosure._
import acumen.interpreters.enclosure.Types._
import acumen.interpreters.enclosure._
import acumen.interpreters.enclosure.affine.UnivariateAffineEnclosure
import acumen.interpreters.enclosure.ivp.IVPSolver
import acumen.interpreters.enclosure.HybridSystem
import acumen.interpreters.enclosure.event.EventEncloser

class TreeEventEncloser(override var ivpSolver: IVPSolver) extends EventEncloser {

  case class SolveVtEException(message: String) extends Exception

  /** FIXME add description */
  override def encloseEvents(
    ps: Parameters, // parameters for controlling event handling and IVP solving
    h: HybridSystem, // the system 
    t: Interval, // the time interval 
    s: StateEnclosure // initial states at t.low
    )(implicit rnd: Rounding): (StateEnclosure, StateEnclosure) = {
    val trees = s.uncertainStates.map {
      eventTree(
        h, t, _,
        ps.initialPicardPadding,
        ps.picardImprovements,
        ps.maxPicardIterations,
        ps.splittingDegree,
        ps.maxEventTreeSize)
    }
    val endTimeState = StateEnclosure.union(trees.map(_.endTimeStateEnclosure))
    val enclosure = StateEnclosure.union(trees.map(_.stateEnclosure))
    (enclosure, endTimeState)
  }

  /** FIXME add description */
  private def eventTree(
    H: HybridSystem,
    T: Interval,
    S: UncertainState,
    delta: Double,
    m: Int,
    n: Int,
    degree: Int,
    K: Int)(implicit rnd: Rounding): EventTree = {
    var res = EventTree.initialTree(T, H, S, delta, m, n, degree, ivpSolver)
    var tmp = res.addLayer(ivpSolver)
    while (res.size < K && tmp != res) {
      res = tmp
      tmp = tmp.addLayer(ivpSolver)
    }
    if (res != tmp) {
      sys.error("gave up for maxEventTreeSize " + K + " at " + T)
    }
    res
  }

  // old event encloser for using with simple recursive strategy 
  // deprecate?
  def solveVtE(
    H: HybridSystem,
    T: Interval,
    S: UncertainState,
    delta: Double,
    m: Int,
    n: Int,
    degree: Int,
    K: Int,
    log: String => Unit)(implicit rnd: Rounding): Option[(Set[UncertainState], Seq[UnivariateAffineEnclosure])] =
    try {
      val tree = eventTree(H, T, S, delta, m, n, degree, K)
      Some((tree.endTimeStates, tree.prunedEnclosures))
    }
    catch {
      case _ => None
    }

}
