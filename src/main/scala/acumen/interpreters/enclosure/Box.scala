package acumen.interpreters.enclosure

import Interval._
import Types._
import scala.collection.immutable.MapProxy

/** Interval boxes with named components. */
class Box(val self: Map[VarName, Interval]) extends MapProxy[VarName, Interval] {

  /** Look up an interval in the box. Return the [0,0] interval as default if the variable name not exist. */
  def apply(name: VarName)(implicit rnd: Rounding): Interval = self getOrElse (name, Interval(0, 0))

  /** Returns true if all intervals in "subBox" are contained in the corresponding interval in "box". */
  def contains(box: Box)(implicit rnd: Rounding) = {
    assert(box.keySet == this.keySet, "Containment is only defined for boxes with the same set of variables.")
    box.keys.forall { name => this(name) contains box(name) }
  }

  /** Component-wise union. */
  def union(that: Box)(implicit rnd: Rounding): Box = {
    require(keySet == that.keySet)
    map { case (k, v) => k -> that(k) /\ v }
  }

  def intersect(that: Box)(implicit rnd: Rounding): Box = {
    require(keySet == that.keySet)
    //    map { case (k, v) => k -> (if (v disjointFrom that(k)) v else that(k) /\ v) }
    map { case (k, v) => k -> that(k) \/ v }
  }

  /** Split the interval of the 'name' component. */
  def split(name: VarName)(implicit rnd: Rounding): Set[Box] = {
    require(keySet contains name)
    val (l, r) = this(name).split
    Set(this + (name -> l), this + (name -> r))
  }

  /** Split the interval of each component. */
  def split(implicit rnd: Rounding): Set[Box] =
    foldLeft(Set(this)) { case (res, (name, _)) => res flatMap (_ split name) }

  def almostEqualTo(that: Box)(implicit rnd: Rounding): Boolean = {
    require(keySet == that.keySet)
    this.forall { case (name, interval) => interval almostEqualTo that(name) }
  }

}
object Box {

  /** An empty box. */
  def empty: Box = Box()

  /** Build a box out of name-interval pairs. */
  def apply(domains: (VarName, Interval)*): Box = new Box(Map(domains: _*))

  /**
   * Translate the domain so that each interval is of the form [0,_].
   * For each interval i in "box", use i.width.high as the new end-point to give a safe
   * over-approximation of the end-point.
   */
  def normalize(box: Box)(implicit rnd: Rounding): Box = box.mapValues { i => 0 /\ i.width.high }

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

  implicit def toBox(m: Map[VarName, Interval]): Box = new Box(m)

}

object BoxApp extends App {
  implicit val rnd = Rounding(10)
  val b1 = Box("y" -> Interval(1), "x" -> Interval(1))
  val b2 = Box("x" -> Interval(1), "y" -> Interval(1))
  println(b1 == b2)
}