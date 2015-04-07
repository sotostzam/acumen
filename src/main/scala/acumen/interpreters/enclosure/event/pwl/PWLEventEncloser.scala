package acumen.interpreters.enclosure.event.pwl

import acumen.interpreters.enclosure.Types._
import acumen.interpreters.enclosure.Types.Mode
import acumen.interpreters.enclosure.Field
import acumen.interpreters.enclosure.ivp.PicardSolver
import acumen.interpreters.enclosure.Box
import acumen.interpreters.enclosure.Interval
import acumen.interpreters.enclosure.Parameters
import acumen.interpreters.enclosure.affine.UnivariateAffineEnclosure
import acumen.interpreters.enclosure.ivp.IVPSolver
import acumen.interpreters.enclosure.HybridSystem
import acumen.interpreters.enclosure.StateEnclosure._
import acumen.interpreters.enclosure.StateEnclosure
import acumen.interpreters.enclosure.event.EventEncloser

/**
 * EncloseEvents
 * 
 * Mix in this trait in place of SolveVtE to get PWL rather than EventTree based event handling.
 */
case class PWLEventEncloser(override val ivpSolver: IVPSolver) extends EventEncloser {

  def withSolver(s: IVPSolver) = PWLEventEncloser(s)

  // main function

  /** Implements the event handler method in EventEncloser. */
  override def encloseEvents(ps: Parameters, h: HybridSystem, t: Interval, s: StateEnclosure): (StateEnclosure, StateEnclosure) = {
    val (s0, fin0) = encloseFlowNoEvents(ps, h, s, t)
    val sEvents = reachableStatesPWL(ps, h, t, ps.maxIterations, emptyState(h), s0)
    val sEnclosed = union(Set(s0, sEvents))
    val finEnclosed = union(Set(fin0, sEvents))
    (sEnclosed, finEnclosed)
  }

  // helper functions

  private def reachableStatesPWL(ps: Parameters, h: HybridSystem, t: Interval, maxIterations: Int, pIn: StateEnclosure, wIn: StateEnclosure): StateEnclosure =
    if (maxIterations == 0) sys.error("EncloseEventsFailure")
    else {
      if (wIn.isDefinitelyEmpty) pIn
      else {
        val oneEventEnclosed = encloseOneEvent(h, wIn)
        val (sEvolved, _) = encloseFlowNoEvents(ps, h, oneEventEnclosed, t)
        val p = union(Set(pIn, sEvolved))
        val w = sEvolved.minus(pIn)
        reachableStatesPWL(ps, h, t, maxIterations - 1, p, w)
      }
    }

  private def encloseOneEvent(h: HybridSystem, s: StateEnclosure): StateEnclosure = {
    val allPossibleResets = h.events.map(e => s.intersectGuard(h, e).reset(h, e))
    val unionOfResets = union(allPossibleResets)
    unionOfResets.intersectInv(h)
  }

  private def encloseFlowNoEvents(ps: Parameters, h: HybridSystem, sIn: StateEnclosure, t: Interval): (StateEnclosure, StateEnclosure) = {
    val sPreAndFinalPre = sIn map {
      case (q, None)      => (q, (None, None))
      case (q, Some(box)) => (q, encloseFlowRange(ps, h.fields(q), t, box))
    }
    val sPre = new StateEnclosure(sPreAndFinalPre.mapValues(_._1))
    val finalPre = new StateEnclosure(sPreAndFinalPre.mapValues(_._2))
    val s = sPre.intersectInv(h)
    val fin = finalPre.intersectInv(h)
    (s, fin)
  }

  private def encloseFlowRange(ps: Parameters, f: Field, t: Interval, init: Box): (Option[Box], Option[Box]) = {
    val flow = encloseFlow(ps, f, t, init)
    if (flow isEmpty) (None, None)
    else {
      val ranges = flow.map(_.range)
      (Some(ranges.tail.fold(ranges.head)(_ hull _)), Some(flow.last(t.high)))
    }
  }

  private def encloseFlow(ps: Parameters, f: Field, t: Interval, init: Box): Seq[UnivariateAffineEnclosure] = {
    if (t.width greaterThan ps.maxTimeStep)
      splitAndRepeatEncloseFlow(ps, f, t, init)
    else
      try {
        val (e, _) = encloseFlowStep(ps, f, t, init)
        if (t.width lessThan ps.minSolverStep * 2) Seq(e)
        else {
          val (eL, eR) = splitAndEncloseFlowStep(ps, f, t, init)
          if (significantImprovement(e, eR, t.high, ps.minComputationImprovement))
            try { splitAndRepeatEncloseFlow(ps, f, t, init) } catch { case _ => Seq(e) }
          else Seq(e)
        }
      }
      catch {
        case _ =>
          if (t.width lessThan ps.minSolverStep * 2) sys.error("EncloseFlowFailure")
          else splitAndRepeatEncloseFlow(ps, f, t, init)
      }
  }

  private def splitAndEncloseFlowStep(ps: Parameters, field: Field, t: Interval, init: Box): (UnivariateAffineEnclosure, UnivariateAffineEnclosure) = {
    val (tL, tR) = t.split
    val (eL, initR) = encloseFlowStep(ps, field, tL, init)
    //    val initR = eL(tL.high)
    val (eR, _) = encloseFlowStep(ps, field, tR, initR)
    (eL, eR)
  }

  private def splitAndRepeatEncloseFlow(ps: Parameters, field: Field, t: Interval, init: Box): Seq[UnivariateAffineEnclosure] = {
    val (tL, tR) = t.split
    val esL = encloseFlow(ps, field, tL, init)
    val initR = esL.filter(_.domain.contains(tL.high)).last(tL.high)
    val esR = encloseFlow(ps, field, tR, initR)
    esL ++ esR
  }

  private def encloseFlowStep(ps: Parameters, f: Field, t: Interval, b: Box): (UnivariateAffineEnclosure, Box) =
    ivpSolver.solveIVP(f, t, b,
      ps.initialPicardPadding,
      ps.picardImprovements,
      ps.maxPicardIterations,
      ps.splittingDegree)

}
