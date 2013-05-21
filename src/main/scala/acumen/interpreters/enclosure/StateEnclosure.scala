package acumen.interpreters.enclosure

import acumen.interpreters.enclosure.Types.Event
import acumen.interpreters.enclosure.Types.Mode
import acumen.interpreters.enclosure.Types.UncertainState
import acumen.interpreters.enclosure.affine.UnivariateAffineEnclosure
import scala.collection.immutable.MapProxy

/**
 * Type for representing enclosures for hybrid systems over time segments with possible
 * event occurrences.
 *
 * A None value associated with a particular mode indicates that the system cannot be in
 * that mode over the given time segment.
 */
class StateEnclosure(val self: Map[Mode, Option[Box]])(implicit rnd: Rounding) extends MapProxy[Mode, Option[Box]] {
  import rnd._
  import StateEnclosure._

  // operations on state enclosures

  /** mode-wise set difference */
  def minus(that: StateEnclosure): StateEnclosure = new StateEnclosure({
    require(this.keySet == that.keySet, "EncloseEvents.minus: cannot take the set difference of " + this + " and " + that)
    def ominus(l: Option[Box], r: Option[Box]): Option[Box] =
      if (l.isEmpty || r.isEmpty) l
      else l.get setminus r.get
    this.map { case (mode, obox) => mode -> ominus(obox, that(mode)) }
  })

  /** mode-wise intersection with invariant */
  // TODO solve this without try-catch
  def intersectInv(h: HybridSystem): StateEnclosure =
    new StateEnclosure(this.map {
      case (mode, obox) =>
        mode -> (try {
          val box = obox.get
          Some(h.domains(mode).support(box))
        }
        catch { case _ => None })
    })

  /** mode-wise intersection with the guard of `e` */
  // TODO solve this without try-catch
  def intersectGuard(h: HybridSystem, e: Event): StateEnclosure =
    new StateEnclosure(emptyState(h) + (e.sigma -> (try { Some(h.guards(e).support(this(e.sigma).get)) } catch { case _ => None })))

  /** "element-wise" application of reset for `e` */
  // TODO solve this without try-catch
  def reset(h: HybridSystem, e: Event): StateEnclosure =
    new StateEnclosure(emptyState(h) + (e.tau -> (try { Some(Box.toBox(h.resets(e)(this(e.sigma).get))) } catch { case _ => None })))

  /** check that `s` is empty for each mode */
  def isDefinitelyEmpty: Boolean =
    this.forall(_._2 isEmpty)

  /** TBA */
  def uncertainStates(s: StateEnclosure): Set[UncertainState] =
    for ((mode, obox) <- s.toSet if obox isDefined)
      yield UncertainState(mode, obox.get)

  def enclosures(t: Interval): Seq[UnivariateAffineEnclosure] = {
    val boxes = this.toSeq.map(_._2).filter(_.isDefined).map(_.get)
    require(!boxes.isEmpty)
    Seq(UnivariateAffineEnclosure(t, boxes.tail.fold(boxes.head)(_ hull _)))
  }

}
object StateEnclosure {

  /** mode-wise union */
  def union(ss: Set[StateEnclosure])(implicit rnd: Rounding): StateEnclosure =
    new StateEnclosure({
      def hull(obs: Seq[Option[Box]]): Option[Box] = {
        def ohull(l: Option[Box], r: Option[Box]) =
          if (l isEmpty) r
          else if (r isEmpty) l
          else Some((l.get) hull (r.get))
        if (obs isEmpty) None
        else obs.tail.fold(obs.head)((res, ob) => ohull(res, ob))
      }
      // flatten state-enclosures to sequence of mode-Option[Box] pairs
      val modeAndOBoxs = ss.toSeq.flatMap(_.toSeq)
      // group oboxes by mode
      val oBoxsByMode = modeAndOBoxs.groupBy(_._1).mapValues(_.map(_._2))
      // take the hull of each Option[Box] group
      oBoxsByMode.mapValues(hull(_))
    })

  /** the state enclosure that is empty in each mode */
  def emptyState(h: HybridSystem)(implicit rnd: Rounding): StateEnclosure =
    new StateEnclosure(h.modes.map(_ -> None).toMap)

}