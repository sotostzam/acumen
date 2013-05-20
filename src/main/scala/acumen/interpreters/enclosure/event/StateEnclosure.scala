package acumen.interpreters.enclosure.event

import acumen.interpreters.enclosure.Box
import acumen.interpreters.enclosure.Box.toBox
import acumen.interpreters.enclosure.Interval
import acumen.interpreters.enclosure.Rounding
import acumen.interpreters.enclosure.Types.Event
import acumen.interpreters.enclosure.Types.Mode
import acumen.interpreters.enclosure.Types.UncertainState
import acumen.interpreters.enclosure.affine.UnivariateAffineEnclosure
import acumen.interpreters.enclosure.HybridSystem

trait StateEnclosure {

  /**
   * Type for representing enclosures for hybrid systems over time segments with possible
   * event occurrences.
   *
   * A None value associated with a particular mode indicates that the system cannot be in
   * that mode over the given time segment.
   */
  type StateEnclosure = Map[Mode, OBox]

  type OBox = Option[Box]

  // operations on state enclosures

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
        }
        catch { case _ => None })
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

  /** TBA */
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