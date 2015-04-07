package acumen.interpreters.enclosure

import scala.annotation.unchecked
import Interval._
import net.java.jinterval.rational._
import net.java.jinterval.interval.set._

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
case class Interval(val i: SetInterval) extends AnyVal {
   
  def lo: Real = i.inf
  def hi: Real = i.sup
  def midpoint: Real = i.mid

  def low: Interval = Interval(lo)
  def high: Interval = Interval(hi)
  def bounds: (Interval, Interval) = (low, high)

  def left: Interval = Interval(lo, midpoint)
  def right: Interval = Interval(midpoint, hi)

  /**
   * Set difference.
   *
   * Note: yields None for empty intersections.
   */
  def setminus(that: Interval): Option[Interval] =
    if (that contains this) None
    else Some(this \ that)

  /**
   * Set difference.
   *
   * Note: partial operation, fails for empty intersections.
   */
  def \(that: Interval): Interval = {
    require(!(that contains this), "Interval.\\: cannot produce empty set difference!")
    if (that contains lo) Interval(that.hi, hi)
    else if (that contains hi) Interval(lo, that.lo)
    else this
  }

  def intersect(that: Interval): Option[Interval] =
    if (this disjointFrom that) None
    else Some(this \/ that)

  def split = {
    val mid = midpoint
    (Interval(lo, mid), Interval(mid, hi))
  }

  /** Interval of absolute values of elements in this interval. */
  def abs = Interval(ic.abs(this.i))

  /**
   * Interval of n:th power values of elements in this interval.
   *
   * @precondition n >= 0.
   */
  def pow(n: Int): Interval = {
    require(n >= 0)
    Interval(ic.pown(this.i, n))
  }

  def pow(that: Interval) = Interval(ic.pow(this.i, that.i))
  
  /** Sinus */
  def sin = Interval(ic.sin(this.i))

  /** Cosinus */
  def cos = Interval(ic.cos(this.i))

  /** Tangens */
  def tan = Interval(ic.tan(this.i))

  /** Arcus sinus */
  def asin = Interval(ic.asin(this.i))

  /** Arcus cosinus */
  def acos = Interval(ic.acos(this.i))

  /** Arcus tangens */
  def atan = Interval(ic.atan(this.i))
  
  /** Natural logarithm */
  def log = Interval(ic.log(this.i))

  /** Base 10 logarithm */
  def log10 = Interval(ic.log10(this.i))

  /** Square root */
  def sqrt = Interval(ic.sqrt(this.i))

  /** Cubic root */
  def cbrt = Interval(ic.rootn(this.i, 3)) 

  /** Ceiling */
  def ceil = Interval(ic.ceil(this.i))
  
  /** Floor */
  def floor = Interval(ic.floor(this.i))

  /** Sinus hyperbolicus */
  def sinh = Interval(ic.sinh(this.i))

  /** Cosinus hyperbolicus */
  def cosh = Interval(ic.cosh(this.i))
  
  /** Tangens hyperbolicus */
  def tanh = Interval(ic.tanh(this.i))
  
  /** Signum */
  def signum = Interval(ic.sign(this.i))
  
  /**
   * Interval of possible values of the exponential function over this interval.
   *
   * FIXME: need to check that truncation is accounted for!
   */
  def exp = Interval(ic.exp(this.i))

  /**
   * Adds that interval to this one.
   *  @param that the interval to add.
   *  @return an interval which is the subset-least upper bound
   *  of the sum of this and that and representable in the given
   *  precision.
   */
  def +(that: Interval) = Interval(ic.add(this.i, that.i))

  /**
   * Subtracts that interval from this one.
   *  @param that the interval to subtract.
   *  @return an interval which is the subset-least upper bound
   *  of the difference of this and that and representable in the
   *  given precision.
   */
  def -(that: Interval) = Interval(ic.sub(this.i, that.i))

  /**
   * Negation of this interval.
   *  @return an interval which is the negation of this.
   */
  def unary_- = Interval(ic.neg(this.i))

  /**
   * Multiplies that interval with this one.
   *  @param that the interval to multiply with.
   *  @return an interval which is the subset-least upper bound
   *  of the product of this and that and representable in the
   *  given precision.
   */
  def *(that: Interval) = Interval(ic.mul(this.i, that.i))

  def *(that: Double): Interval = this * Interval(that)

  /** Squares this interval. */
  def square = max(Interval.zero, this * this)

  /**
   * Divides this interval by that one.
   *  @param that the divisor.
   *  @return an interval which is the subset-least upper bound
   *  of the quotient of this and that and representable in the
   *  given precision.
   */
  def /(that: Interval) = Interval(ic.div(this.i, that.i))

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
  def /\(that: Interval) = Interval(ic.convexHull(this.i, that.i))

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
  def width = if (i isEmpty) Interval.zero else Interval(i.wid)

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

  def properlyContains(that: Interval): Boolean = 
    that.i containedInInterior this.i

  def isThin = (lo compareTo hi) == 0

  def isZero = equalTo(Interval.zero)
  
  def isNonnegative = greaterThanOrEqualTo(Interval.zero)

  def almostEqualTo(that: Interval) =
    epsilon.contains(this.low - that.low) &&
      epsilon.contains(this.high - that.high)

  /** @return a string representation of the interval in the usual
   *  notation for closed intervals. */
  override def toString =
    s"[${lo.doubleValueDown}..${hi.doubleValueUp}]"
    
  /** @return a string representation of the interval with the 
   *  rational end-points represented exactly. */
  def toRationalString =
    s"[(${lo.getNumerator}/${lo.getDenominator})..(${hi.getNumerator}/${hi.getDenominator})]"

  // TODO improve description
  /** UNSAFE only to be used for plotting */
  def loDouble: Double = lo.doubleValue

  // TODO improve description
  /** UNSAFE only to be used for plotting */
  def hiDouble: Double = hi.doubleValue
}

object Interval {

  type Real = ExtendedRational

  /** Use outward rounded floating-point number operations in interval library. */
  val ic = SetIntervalContexts.getInfSup(BinaryValueSet.BINARY64)
  val rc = ExtendedRationalContexts.exact()

  def apply(lo: Real, hi: Real): Interval =
    Interval(ic.numsToInterval(lo, hi))
  def apply(lo: Double, hi: Double): Interval =
    Interval(ic.numsToInterval(lo, hi))
  def apply(x: Int): Interval = Interval(x, x)
  def apply(x: Double): Interval = Interval(x, x)
  def apply(x: Real): Interval = Interval(x, x)
  def min(left: Interval, right: Interval): Interval =
    Interval(ic.min(left.i, right.i))
  def max(left: Interval, right: Interval): Interval =
    Interval(ic.max(left.i, right.i))
  def min(left: Real, right: Real) = rc.min(left, right)
  def max(left: Real, right: Real) = rc.max(left, right)

  implicit def toInterval(x: Real) = Interval(x)
  implicit def toInterval(x: Double) = Interval(x)
  implicit def toInterval(x: Int) = Interval(x)

  /* Constants */

  /** Neighborhood with twice the width of the smallest representable Double. */
  val epsilon = Interval(-Double.MinPositiveValue, Double.MinPositiveValue)

  /** Approximation based on first 1000 digits. */
  val pi = Interval(ic.textToInterval(
    "3.1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170679" +
    "821480865132823066470938446095505822317253594081284811174502841027019385211055596446229489549303819644" +
    "288109756659334461284756482337867831652712019091456485669234603486104543266482133936072602491412737245" +
    "870066063155881748815209209628292540917153643678925903600113305305488204665213841469519415116094330572" +
    "703657595919530921861173819326117931051185480744623799627495673518857527248912279381830119491298336733" +
    "624406566430860213949463952247371907021798609437027705392171762931767523846748184676694051320005681271" +
    "452635608277857713427577896091736371787214684409012249534301465495853710507922796892589235420199561121" +
    "290219608640344181598136297747713099605187072113499999983729780499510597317328160963185950244594553469" +
    "083026425223082533446850352619311881710100031378387528865875332083814206171776691473035982534904287554" +
    "687311595628638823537875937519577818577805321712268066130019278766111959092164201989380952572010654858" +
    "632788659361533818279682303019520353018529689957736225994138912497217752834791315155748572424541506959")) 

  val zero: Interval = Interval(0)

  val one: Interval = Interval(1)
      
  def union(is: Seq[Interval]): Interval =
    if (is.isEmpty) sys.error("Interval.union: empty union")
    else is.tail.fold(is.head)(_ /\ _)

  def max(is: Iterable[Interval]): Interval = {
    require(is.nonEmpty)
    is.tail.fold(is.head)(max(_, _))
  }
  
  /** Converts rectangular coordinates (x, y) to polar (r, theta) */
  def atan2(x: Interval, y: Interval) = Interval(ic.atan2(x.i, y.i))

}
