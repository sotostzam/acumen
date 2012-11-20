package acumen.interpreters.enclosure

/**
 * A class for outward rounded intervals with BigDeciaml end-points.
 * @define lo and hi fields holding the lower and upper bound of the
 * interval.
 */
import Interval._
import java.math.BigDecimal

/**
 * Intervals with outward-rounded operations.
 *
 * Outward-rounded means that the low end-point is rounded downwards
 * and the high end-point is rounded upwards.
 *
 * The end-points are parameterized by precision, i.e. the number of
 * digits in the decimal expansion of the end-point value.
 *
 * Precondition: lo <= hi.
 *
 * property (monotonicity w.r.t. precision): given doubles a < b and
 * positive integers i < j and intervals X = [a,b] initialized with
 * precision i and Y = [a,b] initialized with precision j it should
 * hold that the set defined by X contains the set defined by Y.
 *
 * property (monotonicity of functions): for each interval function f
 * it should hold that given intervals {A_1,...,A_n} and {B_1,...,B_n}
 * such that A_i is contained in B_i for each i, f(A_1,...,A_n) is
 * contained in f(B_1,...,B_n).
 */
case class Interval private (
  private val lo: Real,
  private val hi: Real)(implicit val rnd: Rounding) {
  import rnd._

  def low = Interval(lo)

  def high = Interval(hi)

  def bounds = (low, high)

  private def midpoint = hi.subtract(lo, dn).divide(Interval(2).lo, dn).add(lo, dn)

  def left = Interval(lo, midpoint)

  def right = Interval(midpoint, hi)

  def \(that: Interval): Option[Interval] =
    if (that contains this) None
    else if (that contains lo) Some(Interval(that.hi, hi))
    else if (that contains hi) Some(Interval(lo, that.lo))
    else Some(this)

  def split = {
    val mid = midpoint
    (Interval(lo, mid), Interval(mid, hi))
  }

  def refine(pieces: Int) = {
    val mesh = width.lo.divide(Interval(pieces).hi, dn)
    (0 until pieces - 1).map {
      i =>
        Interval(
          lo.add(mesh.multiply(Interval(i).lo, dn), dn), lo.add(mesh.multiply(Interval(i + 1).lo, dn), dn))
    } :+ Interval(lo.add(mesh.multiply(Interval(pieces - 1).lo, dn), dn), hi)
  }

  /** Interval of absolute values of elements in this interval. */
  def abs = Interval.max(this /\ -this, Interval(0))

  /** Interval of possible square roots elements in this interval. */
  def sqrt(implicit rnd: Rounding) = {
    def sqrt(dir: java.math.MathContext)(x: Real) = {
      val half = new Real(0.5, dir)
      val xinit = x.round(dir)
      var res = xinit
      var tmp = xinit.subtract(xinit, dir) // == 0
      while (res != tmp) {
        tmp = res
        res = half.multiply(res.add(xinit.divide(res, dir), dir), dir)
      }
      res
    }
    if (this lessThan Interval(0)) sys.error("sqrt is undefined on " + this)
    else Interval(sqrt(dn)(max(lo, lo.subtract(lo))), sqrt(up)(hi))
  }

  /**
   * Adds that interval to this one.
   *  @param that the interval to add.
   *  @return an interval which is the subset-least upper bound
   *  of the sum of this and that and representable in the given
   *  precision.
   */
  def +(that: Interval) = Interval(lo.add(that.lo, dn), hi.add(that.hi, up))

  /**
   * Subtracts that interval from this one.
   *  @param that the interval to subtract.
   *  @return an interval which is the subset-least upper bound
   *  of the difference of this and that and representable in the
   *  given precision.
   */
  def -(that: Interval) = Interval(lo.subtract(that.hi, dn), hi.subtract(that.lo, up))

  /**
   * Negation of this interval.
   *  @return an interval which is the negation of this.
   */
  def unary_- = Interval(hi.negate, lo.negate)

  /**
   * Multiplies that interval with this one.
   *  @param that the interval to multiply with.
   *  @return an interval which is the subset-least upper bound
   *  of the product of this and that and representable in the
   *  given precision.
   */
  def *(that: Interval) = {
    val prodsDN: List[Real] = List(lo.multiply(that.hi, dn), hi.multiply(that.lo, dn), hi.multiply(that.hi, dn))
    val prodsUP: List[Real] = List(lo.multiply(that.hi, up), hi.multiply(that.lo, up), hi.multiply(that.hi, up))
    Interval(prodsDN.foldLeft(lo.multiply(that.lo, dn))(min(_, _)), prodsUP.foldLeft(lo.multiply(that.lo, up))(max(_, _)))
  }

  def *(that: Double): Interval = this * Interval(that)

  /** Squares this interval. */
  def square = max(Interval(0), this * this)

  /**
   * Divides this interval by that one.
   *  @param that the divisor.
   *  @return an interval which is the subset-least upper bound
   *  of the quotient of this and that and representable in the
   *  given precision.
   */
  def /(that: Interval) = {
    require(!that.contains(0), "division by 0")
    val divsDN: List[Real] = List(lo.divide(that.hi, dn), hi.divide(that.lo, dn), hi.divide(that.hi, dn))
    val divsUP: List[Real] = List(lo.divide(that.hi, up), hi.divide(that.lo, up), hi.divide(that.hi, up))
    Interval(divsDN.foldLeft(lo.divide(that.lo, dn))(min(_, _)), divsUP.foldLeft(lo.divide(that.lo, up))(max(_, _)))
  }

  def /(that: Double): Interval = this / Interval(that)

  /**
   * Take the meet, or g.l.b., of this and that interval.
   *  @param that the interval to take the the meet with.
   *  @return an interval which is the interval-wrapped set
   *  union of this and that.
   *
   *  property: for any intervals A and B it holds that A /\ B
   *  contains both A and B.
   */
  def /\(that: Interval) = Interval(min(lo, that.lo), max(hi, that.hi))

  /**
   * Take the join, or l.u.b., of this and that interval.
   * The intersection of this and that must be non-empty!
   *  @param that the interval to take the the join with.
   *  @return an interval which is the intersection of this
   *  and that.
   */
  def \/(that: Interval) = {
    require(!(this disjointFrom that), "cannot intersect disjoint intervals " + this + " and " + that)
    Interval(max(lo, that.lo), min(hi, that.hi))
  }

  def disjointFrom(that: Interval) =
    that.hi.compareTo(this.lo) < 0 || this.hi.compareTo(that.lo) < 0

  /**
   * The width of this interval.
   * @return the least interval containing this.hi-this.lo
   *
   * property: for any interval X it holds that X.width >= 0.
   *
   * property: for any interval X it holds that (-X).width == X.width.
   */
  def width = Interval(hi.subtract(lo, dn), hi.subtract(lo, up))

  /**
   * Comparison operations on intervals. WARNING!
   * They are not each other's negations. E.g.
   * [0,2] lessThan [1,3] == false but
   * [1,3] greaterThanOrEqualTo [0,2] == false as well!
   * Do not form expressions by negating values returned by these
   * methods, they will likely not produce to the intended results.
   *
   * property (positive monotonicity): for any intervals A,B,C,D such
   * that A contains B and C contains D it holds that if A lessThan C
   * is true then B lessThan D is true.
   */
  def lessThan(that: Interval) = hi.compareTo(that.lo) < 0

  /**
   * property (positive monotonicity): for any intervals A,B,C,D such
   * that A contains B and C contains D it holds that if A lessThanOrEqualTo C
   * is true then B lessThanOrEqualTo D is true.
   */
  def lessThanOrEqualTo(that: Interval) = hi.compareTo(that.lo) <= 0

  /**
   * property (positive monotonicity): for any intervals A,B,C,D such
   * that A contains B and C contains D it holds that if A greaterThanOrEqualTo C
   * is true then B greaterThanOrEqualTo D is true.
   */
  def equalTo(that: Interval) = (this.lo compareTo that.lo) == 0 && (this.hi compareTo that.hi) == 0

  def greaterThanOrEqualTo(that: Interval) = lo.compareTo(that.hi) >= 0

  /**
   * property (positive monotonicity): for any intervals A,B,C,D such
   * that A contains B and C contains D it holds that if A greaterThan C
   * is true then B greaterThan D is true.
   */
  def greaterThan(that: Interval) = lo.compareTo(that.hi) > 0

  /**
   * Determine if this interval contains that interval.
   * @param that the interval to test containment of.
   *  @return a boolean that is true if and only if the underlying
   *  set of this contains the underlying set of that and false
   *  otherwise. Note that a false result does not imply the opposite
   *  relation holds!
   */
  def contains(that: Interval) = lo.compareTo(that.lo) <= 0 && that.hi.compareTo(hi) <= 0

  def contains(x: Real) = lo.compareTo(x) <= 0 && x.compareTo(hi) <= 0

  def contains(x: Double) = {
    val it = Interval(x)
    lo.compareTo(it.lo) <= 0 && it.hi.compareTo(hi) <= 0
  }

  def isThin = (lo compareTo hi) == 0

  def isZero = equalTo(Interval(0))

  def almostEqualTo(that: Interval) = {
    //    println("almostEqualTo: " + epsilon + " contains " + (this.low - that.low) + " is " + (epsilon contains (this.low - that.low)))
    //    println("almostEqualTo: " + epsilon + " contains " + (this.high - that.high) + " is " + (epsilon contains (this.high - that.high)))
    (epsilon contains (this.low - that.low)) &&
      (epsilon contains (this.high - that.high))
  }

  /**
   * @return a string representation of the interval in the usual
   * notation for closed intervals.
   */
  override def toString = "[" + lo + "," + hi + "]"

  // TODO improve description
  /** UNSAFE only to be used for plotting */
  def loDouble: Double = lo.doubleValue()

  // TODO improve description
  /** UNSAFE only to be used for plotting */
  def hiDouble: Double = hi.doubleValue()
}

object Interval {
  import java.math.BigDecimal
  type Real = java.math.BigDecimal
  def apply(lo: Double, hi: Double)(implicit rnd: Rounding): Interval =
    Interval(new BigDecimal(lo, rnd.dn), new BigDecimal(hi, rnd.up))
  def apply(x: Int)(implicit rnd: Rounding): Interval = Interval(x, x)
  def apply(x: Double)(implicit rnd: Rounding): Interval = Interval(x, x)
  def apply(x: Real)(implicit rnd: Rounding): Interval = Interval(x, x)
  def min(left: Interval, right: Interval)(implicit rnd: Rounding): Interval =
    Interval(min(left.lo, right.lo), min(left.hi, right.hi))
  def max(left: Interval, right: Interval)(implicit rnd: Rounding): Interval =
    Interval(max(left.lo, right.lo), max(left.hi, right.hi))
  def min(left: Real, right: Real) = if (left.compareTo(right) < 0) left else right
  def max(left: Real, right: Real) = if (left.compareTo(right) > 0) left else right

  implicit def toInterval(x: Real)(implicit rnd: Rounding) = Interval(x)
  implicit def toInterval(x: Double)(implicit rnd: Rounding) = Interval(x)
  implicit def toInterval(x: Int)(implicit rnd: Rounding) = Interval(x)
  def epsilon(implicit rnd: Rounding) = {
    val eps = new Real(0.1).pow(rnd.precision - 1, rnd.up)
    Interval(eps.negate, eps)
  }
}

object IntervalApp extends App {
  implicit val rnd = Rounding(10)
  println(Interval(5, 6) \ Interval(2, 4))
}
