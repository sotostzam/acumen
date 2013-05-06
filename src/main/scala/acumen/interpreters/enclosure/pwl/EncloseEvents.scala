package acumen.interpreters.enclosure.pwl

import acumen.interpreters.enclosure.Types._
import acumen.interpreters.enclosure.Types.Mode
import acumen.interpreters.enclosure.Field
import acumen.interpreters.enclosure.SolveIVP
import acumen.interpreters.enclosure.Box
import acumen.interpreters.enclosure.Interval
import acumen.interpreters.enclosure.Parameters
import acumen.interpreters.enclosure.Rounding
import acumen.interpreters.enclosure.UnivariateAffineEnclosure
import acumen.interpreters.enclosure.tree.HybridSystem

/**
 * Mix in this trait in place of SolveVtE to get PWL
 * rather than EventTree based event handling.
 */
trait EncloseEvents extends SolveIVP {

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
      case (q, None) => (q, (None, None))
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
        val e = encloseFlowStep(ps, f, t, init)
        if (t.width lessThan ps.minSolverStep * 2) Seq(e)
        else {
          val (eL, eR) = splitAndEncloseFlowStep(ps, f, t, init)
          if (significantImprovement(e, eR, t.high, ps.minComputationImprovement))
            try { splitAndRepeatEncloseFlow(ps, f, t, init) } catch { case _ => Seq(e) }
          else Seq(e)
        }
      } catch {
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
    val eL = encloseFlowStep(ps, field, tL, init)
    val initR = eL(tL.high)
    val eR = encloseFlowStep(ps, field, tR, initR)
    (eL, eR)
  }

  def splitAndRepeatEncloseFlow(ps: Parameters, field: Field, t: Interval, init: Box)(implicit rnd: Rounding): Seq[UnivariateAffineEnclosure] = {
    val (tL, tR) = t.split
    val esL = encloseFlow(ps, field, tL, init)
    val initR = esL.filter(_.domain.contains(tL.high)).last(tL.high)
    val esR = encloseFlow(ps, field, tR, initR)
    esL ++ esR
  }

  def encloseFlowStep(ps: Parameters, f: Field, t: Interval, b: Box)(implicit rnd: Rounding): UnivariateAffineEnclosure =
    solveVt(f, t, b,
      ps.initialPicardPadding,
      ps.picardImprovements,
      ps.maxPicardIterations,
      ps.splittingDegree)

  // types for representing enclosures

  type OBox = Option[Box]

  type StateEnclosure = Map[Mode, OBox]

  // operations on enclosures

  /** mode-wise union */
  def union(ss: Set[StateEnclosure])(implicit rnd: Rounding): StateEnclosure = {
    def hull(obs: Seq[OBox]): OBox = {
      def ohull(l: OBox, r: OBox) =
        if (l isEmpty) r
        else if (r isEmpty) l
        else Some((l.get) hull (r.get))
      if (obs isEmpty) None
      else obs.tail.fold(obs.head)((res, ob) => ohull(res, ob))
    }
    // flatten state-enclosures to sequence of mode-obox pairs
    val modeAndOBoxs = ss.toSeq.flatMap(_.toSeq)
    // group oboxes by mode
    val oBoxsByMode = modeAndOBoxs.groupBy(_._1).mapValues(_.map(_._2))
    // take the hull of each obox group
    oBoxsByMode.mapValues(hull(_))
  }

  /** mode-wise set difference */
  def minus(l: StateEnclosure, r: StateEnclosure)(implicit rnd: Rounding): StateEnclosure = {
    require(l.keySet == r.keySet, "EncloseEvents.minus: cannot take the set difference of " + l + " and " + r)
    def ominus(l: OBox, r: OBox): OBox =
      if (l.isEmpty || r.isEmpty) l
      else l.get setminus r.get
    l.map { case (mode, obox) => mode -> ominus(obox, r(mode)) }
  }

  /** mode-wise intersection with invariant */
  // TODO solve this without try-catch
  def intersectInv(h: HybridSystem, s: StateEnclosure)(implicit rnd: Rounding): StateEnclosure =
    s.map {
      case (mode, obox) =>
        mode -> (try {
          val box = obox.get
          Some(h.domains(mode).support(box))
        } catch { case _ => None })
    }

  /** mode-wise intersection with the guard of `e` */
  // TODO solve this without try-catch
  def intersectGuard(h: HybridSystem, e: Event, s: StateEnclosure)(implicit rnd: Rounding): StateEnclosure =
    emptyState(h) + (e.sigma -> (try { Some(h.guards(e).support(s(e.sigma).get)) } catch { case _ => None }))

  /** "element-wise" application of reset for `e` */
  // TODO solve this without try-catch
  def reset(h: HybridSystem, e: Event, s: StateEnclosure)(implicit rnd: Rounding): StateEnclosure =
    emptyState(h) + (e.tau -> (try { Some(h.resets(e)(s(e.sigma).get)) } catch { case _ => None }))

  /** the state enclosure that is empty in each mode */
  def emptyState(h: HybridSystem): StateEnclosure = h.modes.map(_ -> None).toMap

  /** check that `s` is empty for each mode */
  def isDefinitelyEmpty(s: StateEnclosure): Boolean =
    s.forall(_._2 isEmpty)

  // plumbing to enable drop-in replacement for solveVtE 

  def solveVtE(
    h: HybridSystem,
    t: Interval,
    u: UncertainState,
    delta: Double,
    m: Int,
    n: Int,
    degree: Int,
    K: Int,
    log: String => Unit)(implicit rnd: Rounding): Option[(Set[UncertainState], Seq[UnivariateAffineEnclosure])] = {
    val noEventEnclosure = solveVt(h.fields(u.mode), t, u.initialCondition, delta, m, n, degree)
    val noPossibleEvents = !h.guards.exists {
      case (event, guard) => event.sigma == u.mode && (guard(noEventEnclosure.range) contains true)
    }
    if (noPossibleEvents)
      Some((Set(UncertainState(u.mode, noEventEnclosure(t.high))), Seq(noEventEnclosure)))
    else {
      val ps = Parameters(rnd.precision, t.loDouble, t.hiDouble,
        delta, m, n,
        K, 0, 0, 0, 0, 0, // these are not used - any value will do
        degree,
        0 // these are not used - any value will do
        )
      val init: StateEnclosure = emptyState(h) + (u.mode -> Some(u.initialCondition))
      val (s, fin) = encloseEvents(ps, h, t, init)
      val us = uncertainStates(fin)
      val es = enclosures(t, s)
      if (us.isEmpty || es.isEmpty) None
      else Some(us, es)
      //      else Some(us, noEventEnclosure +: es)
    }
  }

  def uncertainStates(s: StateEnclosure): Set[UncertainState] =
    for ((mode, obox) <- s.toSet if obox isDefined)
      yield UncertainState(mode, obox.get)

  def enclosures(t: Interval, s: StateEnclosure)(implicit rnd: Rounding): Seq[UnivariateAffineEnclosure] = {
    //    for ((_, obox) <- s.toSeq if obox isDefined)
    //      yield UnivariateAffineEnclosure(t, obox.get)
    val boxes = s.toSeq.map(_._2).filter(_.isDefined).map(_.get)
    require(!boxes.isEmpty)
    Seq(UnivariateAffineEnclosure(t, boxes.tail.fold(boxes.head)(_ hull _)))
  }

}
