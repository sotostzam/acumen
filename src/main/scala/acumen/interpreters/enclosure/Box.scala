package acumen.interpreters.enclosure

import Interval._
import Types._
import scala.collection.immutable.MapProxy

/** Interval boxes with named components. */
class Box(val self: Map[VarName, Interval])(implicit rnd: Rounding) extends MapProxy[VarName, Interval] {
  import rnd._

  /** Look up an interval in the box. Return the [0,0] interval as default if the variable name not exist. */
  override def apply(name: VarName): Interval = self getOrElse (name, Interval(0, 0))

  /** Returns true if all intervals in "subBox" are contained in the corresponding interval in "box". */
  def contains(box: Box) = {
    assert(box.keySet == this.keySet, "Containment is only defined for boxes with the same set of variables.")
    box.keys.forall { name => this(name) contains box(name) }
  }

  def disjointFrom(that: Box): Boolean = {
    require(keySet == that.keySet)
    keySet.exists(name => this(name) disjointFrom that(name))
  }

  /** Component-wise union. */
  def hull(that: Box): Box = {
    require(keySet == that.keySet)
    map { case (k, v) => k -> that(k) /\ v }
  }

  /** Component-wise set difference. */
  def setminus(that: Box): Option[Box] = {
    require(keySet == that.keySet)
    if (exists { case (name, interval) => that(name) contains interval }) None
    else Some(map { case (name, interval) => name -> (interval setminus that(name)).get })
  }

  /** Component-wise set difference. */
  def \(that: Box)(implicit rnd: Rounding): Box = {
    require(keySet == that.keySet)
    map { case (name, interval) => name -> (interval setminus that(name)).get }
  }

  /**
   * Component-wise intersection.
   *
   * Note: yields None whenever intersect yields None for a component.
   */
  def intersect(that: Box): Option[Box] = {
    require(this.keySet == that.keySet)
    if (this.exists { case (name, interval) => that(name) disjointFrom interval }) None
    else Some(this \/ that)
  }

  /**
   * Component-wise intersection.
   *
   * Note: partial operation, fails whenever \/ fails for a component.
   */
  def \/(that: Box): Box = {
    require(keySet == that.keySet)
    map { case (k, v) => k -> that(k) \/ v }
  }

  /** Split the interval of the 'name' component. */
  def split(name: VarName): Set[Box] = {
    require(keySet contains name)
    val (l, r) = this(name).split
    Set(this + (name -> l), this + (name -> r))
  }

  /** Split the interval of each 'names' component. */
  def split(names: VarName*): Set[Box] = {
    require(names.toSet subsetOf keySet)
    names.foldLeft(Set(this)) { case (res, name) => res flatMap (_ split name) }
  }

  /** Split the interval of each component. */
  def split: Set[Box] =
    foldLeft(Set(this)) { case (res, (name, _)) => res flatMap (_ split name) }

  /** Refine the interval of the 'name' component. */
  def refine(pieces: Int, name: VarName): Set[Box] = {
    require(keySet contains name)
    (this(name) refine (pieces) map ((i: Interval) =>
      (this + (name -> i)): Box)).toSet
  }

  /** Refine the interval of each 'names' component. */
  def refine(pieces: Int, names: VarName*): Set[Box] = {
    require(names.toSet subsetOf keySet)
    names.foldLeft(Set(this)) { case (res, name) => res flatMap (_ refine (pieces, name)) }
  }

  def equalTo(that: Box): Boolean =
    (keySet == that.keySet) && forall { case (name, interval) => that(name) equalTo interval }

  def almostEqualTo(that: Box): Boolean = {
    require(keySet == that.keySet)
    this.forall { case (name, interval) => interval almostEqualTo that(name) }
  }

  /** The corners of the box. */
  def corners = cornersHelper(this, Seq(Box.empty))
  private def cornersHelper(dom: Map[VarName, Interval], corners: Seq[Map[VarName, Interval]]): Seq[Map[VarName, Interval]] =
    dom.keys.toList match {
      case Nil => corners
      case k :: ks => cornersHelper(dom - k, corners.flatMap(corner => {
        val (lo, hi) = dom(k).bounds
        Seq(corner + (k -> lo), corner + (k -> hi))
      }))
    }

  /** The midpoint of this box. */
  def midpoint(implicit rnd: Rounding): Box = Box.toBox(mapValues(_.midpoint))

  /**
   * Approximation of the max norm for interval vectors.
   *
   * @precondition this.nonEmpty
   */
  def maxNorm = {
    require(this.nonEmpty, "Cannot take the norm of empty box " + this)
    val vs = values.map(_.width.high)
    vs.tail.fold(vs.head)(Interval.max)
  }

  /**
   * Approximation of the L^1 norm for interval vectors.
   *
   * @precondition this.nonEmpty
   */
  def l1Norm = {
    require(this.nonEmpty, "Cannot take the norm of empty box " + this)
    val vs = values.map(_.width.high)
    vs.tail.fold(vs.head)(_ + _)
  }

}
object Box {

  /** An empty box. */
  def empty(implicit rnd: Rounding): Box = Box()

  /** Build a box out of name-interval pairs. */
  def apply(domains: (VarName, Interval)*)(implicit rnd: Rounding): Box = new Box(Map(domains: _*))

  /**
   * Translate the domain so that each interval is of the form [0,_].
   * For each interval i in "box", use i.width.high as the new end-point to give a safe
   * over-approximation of the end-point.
   */
  def normalize(box: Box)(implicit rnd: Rounding): Box = box.mapValues { i => 0 /\ i.width.high }

  /** The corners of the box. */
  def corners(box: Box) = box.corners

  implicit def toBox(m: Map[VarName, Interval])(implicit rnd: Rounding): Box = new Box(m)

}
