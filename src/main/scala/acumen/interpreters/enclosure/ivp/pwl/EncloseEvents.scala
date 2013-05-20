package acumen.interpreters.enclosure.ivp.pwl

import acumen.interpreters.enclosure.Types._
import acumen.interpreters.enclosure.Types.Mode
import acumen.interpreters.enclosure.Field
import acumen.interpreters.enclosure.ivp.PicardSolver
import acumen.interpreters.enclosure.Box
import acumen.interpreters.enclosure.Interval
import acumen.interpreters.enclosure.Parameters
import acumen.interpreters.enclosure.Rounding
import acumen.interpreters.enclosure.ivp.tree.HybridSystem
import acumen.interpreters.enclosure.affine.UnivariateAffineEnclosure
import acumen.interpreters.enclosure.ivp.SolveIVP
import acumen.interpreters.enclosure.event.EventHandler

/**
 * Mix in this trait in place of SolveVtE to get PWL
 * rather than EventTree based event handling.
 */
trait EncloseEvents extends EventHandler {

  def handleEvents(ps: Parameters, h: HybridSystem, t: Interval, s: StateEnclosure)(implicit rnd: Rounding) =
    encloseEvents(ps, h, t, s)

  // main function

  def encloseEvents(ps: Parameters, h: HybridSystem, t: Interval, s: StateEnclosure)(implicit rnd: Rounding): (StateEnclosure, StateEnclosure) = {
    val (s0, fin0) = encloseFlowNoEvents(ps, h, s, t)
    val sEvents = reachableStatesPWL(ps, h, t, ps.maxIterations, emptyState(h), s0)
    val sEnclosed = union(Set(s0, sEvents))
    val finEnclosed = union(Set(fin0, sEvents))
    (sEnclosed, finEnclosed)
  }

  // helper functions

  def reachableStatesPWL(ps: Parameters, h: HybridSystem, t: Interval, maxIterations: Int, pIn: StateEnclosure, wIn: StateEnclosure)(implicit rnd: Rounding): StateEnclosure =
    if (maxIterations == 0) sys.error("EncloseEventsFailure")
    else {
      if (isDefinitelyEmpty(wIn)) pIn
      else {
        val oneEventEnclosed = encloseOneEvent(h, wIn)
        val (sEvolved, _) = encloseFlowNoEvents(ps, h, oneEventEnclosed, t)
        val p = union(Set(pIn, sEvolved))
        val w = minus(sEvolved, pIn)
        reachableStatesPWL(ps, h, t, maxIterations - 1, p, w)
      }
    }

  def encloseOneEvent(h: HybridSystem, s: StateEnclosure)(implicit rnd: Rounding): StateEnclosure = {
    val allPossibleResets = h.events.map(e => reset(h, e, intersectGuard(h, e, s)))
    val unionOfResets = union(allPossibleResets)
    intersectInv(h, unionOfResets)
  }

  def encloseFlowNoEvents(ps: Parameters, h: HybridSystem, sIn: StateEnclosure, t: Interval)(implicit rnd: Rounding): (StateEnclosure, StateEnclosure) = {
    val sPreAndFinalPre = sIn map {
      case (q, None)      => (q, (None, None))
      case (q, Some(box)) => (q, encloseFlowRange(ps, h.fields(q), t, box))
    }
    val sPre = sPreAndFinalPre.mapValues(_._1)
    val finalPre = sPreAndFinalPre.mapValues(_._2)
    val s = intersectInv(h, sPre)
    val fin = intersectInv(h, finalPre)
    (s, fin)
  }

  def encloseFlowRange(ps: Parameters, f: Field, t: Interval, init: Box)(implicit rnd: Rounding): (OBox, OBox) = {
    val flow = encloseFlow(ps, f, t, init)
    if (flow isEmpty) (None, None)
    else {
      val ranges = flow.map(_.range)
      (Some(ranges.tail.fold(ranges.head)(_ hull _)), Some(flow.last(t.high)))
    }
  }

  def encloseFlow(ps: Parameters, f: Field, t: Interval, init: Box)(implicit rnd: Rounding): Seq[UnivariateAffineEnclosure] = {
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

  def significantImprovement(eOld: UnivariateAffineEnclosure, eNew: UnivariateAffineEnclosure, x: Interval, minComputationImprovement: Double)(implicit rnd: Rounding) = {
    val normOld = norm(eOld(x))
    val normNew = norm(eNew(x))
    normOld - normNew greaterThan minComputationImprovement
  }

  def splitAndEncloseFlowStep(ps: Parameters, field: Field, t: Interval, init: Box)(implicit rnd: Rounding): (UnivariateAffineEnclosure, UnivariateAffineEnclosure) = {
    val (tL, tR) = t.split
    val (eL, initR) = encloseFlowStep(ps, field, tL, init)
    //    val initR = eL(tL.high)
    val (eR, _) = encloseFlowStep(ps, field, tR, initR)
    (eL, eR)
  }

  def splitAndRepeatEncloseFlow(ps: Parameters, field: Field, t: Interval, init: Box)(implicit rnd: Rounding): Seq[UnivariateAffineEnclosure] = {
    val (tL, tR) = t.split
    val esL = encloseFlow(ps, field, tL, init)
    val initR = esL.filter(_.domain.contains(tL.high)).last(tL.high)
    val esR = encloseFlow(ps, field, tR, initR)
    esL ++ esR
  }

  def encloseFlowStep(ps: Parameters, f: Field, t: Interval, b: Box)(implicit rnd: Rounding): (UnivariateAffineEnclosure, Box) =
    ivpSolver.solveIVP(f, t, b,
      ps.initialPicardPadding,
      ps.picardImprovements,
      ps.maxPicardIterations,
      ps.splittingDegree)


}
