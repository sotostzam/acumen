package acumen.interpreters.enclosure.pwl

import acumen.interpreters.enclosure.Box
import acumen.interpreters.enclosure.Types.Mode
import acumen.interpreters.enclosure.tree.HybridSystem
import acumen.interpreters.enclosure.Interval
import acumen.interpreters.enclosure.Types._
import acumen.interpreters.enclosure.tree.Field
import acumen.interpreters.enclosure.UnivariateAffineEnclosure
import acumen.interpreters.enclosure.Rounding

class EncloseEvents(implicit rnd: Rounding) extends App {

  type OBox = Option[Box]

  type StateEnclosure = Map[Mode, OBox]

  /** mode-wise union */
  def union(ss: Set[StateEnclosure]): StateEnclosure = {
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
  def minus(l: StateEnclosure, r: StateEnclosure): StateEnclosure = {
    require(l.keySet == r.keySet)
    def ominus(l: OBox, r: OBox): OBox =
      if (l.isEmpty || r.isEmpty) l
      else l.get \ r.get
    l.map { case (mode, obox) => mode -> ominus(obox, r(mode)) }
  }

  def intersectInv(s: StateEnclosure): StateEnclosure = null
  def intersectGuard(e: Event, s: StateEnclosure): StateEnclosure = null
  def reset(e: Event, r: StateEnclosure): StateEnclosure = null
  def emptyState(H: HybridSystem): StateEnclosure = H.modes.map(_ -> None).toMap
  def isDefinitelyEmpty(s: StateEnclosure): Boolean = s == None

  def encloseEvents(h: HybridSystem, t: Interval, s: StateEnclosure): (StateEnclosure, StateEnclosure) = {
    val (s0, fin0) = encloseFlowNoEvents(h, s, t)
    val sEvents = reachableStatesPWL(h, t, emptyState(h), s0)
    (union(Set(s0, sEvents)), union(Set(fin0, sEvents)))
  }

  def reachableStatesPWL(h: HybridSystem, t: Interval, pIn: StateEnclosure, wIn: StateEnclosure): StateEnclosure =
    if (isDefinitelyEmpty(wIn)) pIn
    else {
      val (sEvolved, _) = encloseFlowNoEvents(h, encloseOneEvent(h, wIn), t)
      val w = minus(sEvolved, pIn)
      val p = union(Set(pIn, sEvolved))
      reachableStatesPWL(h, t, p, w)
    }

  def encloseOneEvent(h: HybridSystem, s: StateEnclosure): StateEnclosure =
    union(h.events.map(e => reset(e, intersectGuard(e, s))))

  def encloseFlowNoEvents(H: HybridSystem, S_In: StateEnclosure, T: Interval): (StateEnclosure, StateEnclosure) = {
    val sPreAndFinalPre = S_In map {
      case (q, None) => (q, (None, None))
      case (q, Some(box)) => (q, encloseFlowRange(H.fields(q), T, box))
    }
    val s = intersectInv(sPreAndFinalPre.mapValues(_._1))
    val fin = intersectInv(sPreAndFinalPre.mapValues(_._2))
    (s, fin)
  }

  def encloseFlowRange(f: Field, t: Interval, b: Box): (OBox, OBox) = {
    val flow = encloseFlow(f, t, b)
    (Some(flow(t)), Some(flow(t.high)))
  }

  def encloseFlow(f: Field, t: Interval, b: Box): UnivariateAffineEnclosure = null

}
