package acumen.interpreters.enclosure

import Interval._
import Types._
import scala.collection.immutable.MapProxy

/** Interval boxes with named components. */
class Box(val self: Map[VarName, Interval]) extends MapProxy[VarName, Interval] {

  /** Look up an interval in the box. Return the [0,0] interval as default if the variable name not exist. */
  def apply(name: VarName)(implicit rnd: Rounding): Interval = self getOrElse (name, Interval(0, 0))

}
object Box {

  /** An empty box. */
  def empty: Box = Box()

  /** Build a box out of name-interval pairs. */
  def apply(domains: (VarName, Interval)*): Box = new Box(Map(domains: _*))

  /** Translate the domain so that each interval is of the form [0,_]. */
  def normalize(box: Box): Box = box.mapValues { i => val (lo, _) = i.bounds; i - lo }

  /** The corners of the box. */
  def corners(box: Box) = cornersHelper(box, Seq(Box.empty))
  private def cornersHelper(dom: Map[VarName, Interval], corners: Seq[Map[VarName, Interval]]): Seq[Map[VarName, Interval]] =
    dom.keys.toList match {
      case Nil => corners
      case k :: ks => cornersHelper(dom - k, corners.flatMap(corner => {
        val (lo, hi) = dom(k).bounds
        Seq(corner + (k -> lo), corner + (k -> hi))
      }))
    }

  /** Returns true if all intervals in "subBox" are contained in the corresponding interval in "box". */
  def isSubBoxOf(subBox: Box, box: Box)(implicit rnd: Rounding) = {
    require(box.keySet == subBox.keySet)
    box.keys.forall { k => box(k) contains subBox(k) }
  }

  implicit def toBox(m: Map[VarName, Interval]): Box = new Box(m)

}