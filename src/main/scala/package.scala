import acumen.interpreters.enclosure.Interval
import acumen.TAD._
import acumen.FAD._
import breeze.math._
import breeze.storage.Zero

/**
 * Global definitions. Will be automatically available in all other classes.
 * NOTE: Type aliases have to be declared in a package *object* in scala
 */
package object acumen {

 /* canonical (reference) representation of (maybe filtered) store/values 
     'C' is for canonical */

  // Notes on the use of CValue/GValue from commit 87bcf66 (Jul 10 2013)
  //
  //   A closely related change is the conversion from using the CStore in
  //   the u.i. and pretty printer to using the new GStore.  A GStore is
  //   similar to a CStore with the following two difference: 1) The
  //   container type is a collection.Iterable instead of immutable.Map and
  //   2) the value type is a generic Value[_] instead of a Value[CId].
  //
  //   With the conversion to using a GStore the results of the imperative
  //   interpreter, with minimal conversion, can be passed around in most
  //   places that expected a CStore.  This change, combined with the default
  //   filtering options, allow the FinalTournamentWithPlayers model to run
  //   again with the imperative interpreters.  By avoiding the unnecessary
  //   conversion to a CStore after each step the imperative interpreter now
  //   runs this model over 4 times faster than the reference interpreter (7s
  //   vs 30s).

  type CValue = Value[CId]
  type CObject = Map[Name, CValue]
  type CStore = Map[CId, CObject]

  // 'G' is for generic
  type GValue = Value[_]
  type GObject = collection.Iterable[(Name, GValue)]
  type GStore = collection.Iterable[(CId, GObject)]
  
  /**
   * Type class with operations that have sensible
   * implementations for Int as well as all the
   * other numeric types.
   */
  trait Integral[V] extends PartialOrdering[V] with Semiring[V] with Zero[V] {
    def add(l: V, r: V): V
    def sub(l: V, r: V): V
    def mul(l: V, r: V): V
    def neg(x: V): V
    def fromInt(i: Int): V
    def toInt(x: V): Int
    def toDouble(x: V): Double
    def zero: V
    def one: V
    def isZero(x: V): Boolean = x == zero
    def isValidInt(x: V): Boolean
    def isValidDouble(x: V): Boolean
    def groundValue(v: V): GroundValue
    /* Semiring */
    def +(l: V, r: V): V = add(l,r)
    def *(l: V, r: V): V = mul(l,r)
  }

  /**
   * Type class with operations that have sensible
   * implementations for numeric types that represent
   * real numbers. 
   */
  trait Real[V] extends Integral[V] {
    def div(l: V, r: V): V
    def pow(l: V, r: V): V
    def sin(x: V): V
    def cos(x: V): V
    def tan(x: V): V
    def acos(x: V): V
    def asin(x: V): V
    def atan(x: V): V
    def exp(x: V): V
    def log(x: V): V
    def square(x: V): V
    def sqrt(x: V): V
    def fromDouble(i: Double): V
  }
  
  /** Syntactic sugar for Integral operations */
  implicit class IntegralOps[V](val l: V)(implicit ev: Integral[V]) {
    def +(r: V): V = ev.add(l, r)
    def -(r: V): V = ev.sub(l, r)
    def *(r: V): V = ev.mul(l, r)
    def unary_- = ev.neg(l)
    def < (r: V): Boolean = ev.lt(l, r)
    def > (r: V): Boolean = ev.gt(l, r)
    def <= (r: V): Boolean = ev.lteq(l, r)
    def >= (r: V): Boolean = ev.gteq(l, r)
    def zero: V = ev.zero
    def one: V = ev.one
    def isZero: Boolean = ev.isZero(l)
    def isValidInt: Boolean = ev.isValidInt(l)
    def toInt: Int = ev.toInt(l)
    def isValidDouble: Boolean = ev.isValidDouble(l)
    def toDouble: Double = ev.toDouble(l)
  }

  /** Syntactic sugar for Real operations */
  implicit class RealOps[V](val l: V)(implicit ev: Real[V]) {
    def /(r: V): V = ev.div(l, r)
    def ^(r: V): V = ev.pow(l, r)
    def ^(r: Int): V = ev.pow(l, ev.fromInt(r))
    def sin: V = ev.sin(l)
    def cos: V = ev.cos(l)
    def tan: V = ev.tan(l)
    def acos: V = ev.acos(l)
    def asin: V = ev.asin(l)
    def atan: V = ev.atan(l)
    def exp: V = ev.exp(l)
    def log: V = ev.log(l)
    def square: V = ev.square(l)
    def sqrt: V = ev.sqrt(l)
  }
  
  /** Integral instance for Int */
  implicit object intIsIntegral extends Integral[Int] {
    def add(l: Int, r: Int): Int = l + r
    def sub(l: Int, r: Int): Int = l - r
    def mul(l: Int, r: Int): Int = l * r
    def neg(x: Int): Int = -x
    def fromInt(x: Int): Int = x
    def zero: Int = 0
    def one: Int = 1
    def ==(a: Int, b: Int): Boolean = a == b
    def !=(a: Int, b: Int): Boolean = a != b
    def tryCompare(l: Int, r: Int): Option[Int] = Some(l compareTo r)
    def lteq(l: Int, r: Int): Boolean = l <= r
    def isValidInt(x: Int): Boolean = true
    def toInt(x: Int): Int = x
    def isValidDouble(x: Int): Boolean = true
    def toDouble(x: Int): Double = x.toDouble
    def groundValue(v: Int): GroundValue = GInt(v)
  }

  /** Real instance for Double */
  implicit object doubleIsReal extends Real[Double] {
    def add(l: Double, r: Double): Double = l + r
    def sub(l: Double, r: Double): Double = l - r
    def mul(l: Double, r: Double): Double = l * r
    def div(l: Double, r: Double): Double = l / r
    def pow(l: Double, r: Double): Double = Math.pow(l, r)
    def neg(x: Double): Double = -x
    def sin(x: Double): Double = Math.sin(x)
    def cos(x: Double): Double = Math.cos(x)
    def tan(x: Double): Double = Math.tan(x)
    def acos(x: Double): Double = Math.acos(x)
    def asin(x: Double): Double = Math.asin(x)
    def atan(x: Double): Double = Math.atan(x)
    def exp(x: Double): Double = Math.exp(x)
    def log(x: Double): Double = Math.log(x)
    def square(x: Double): Double = x * x
    def sqrt(x: Double): Double = Math.sqrt(x)
    def fromInt(x: Int): Double = x
    def fromDouble(x: Double): Double = x
    def zero: Double = 0
    def one: Double = 1
    def ==(a: Double, b: Double): Boolean = a == b
    def !=(a: Double, b: Double): Boolean = a != b
    def tryCompare(l: Double, r: Double): Option[Int] = Some(l compareTo r)
    def lteq(l: Double, r: Double): Boolean = l <= r
    def isValidInt(x: Double): Boolean =
      Math.floor(x) == Math.ceil(x) && Integer.MIN_VALUE <= x && x <= Integer.MAX_VALUE
    def toInt(x: Double): Int = x.toInt
    def isValidDouble(x: Double): Boolean = true
    def toDouble(x: Double): Double = x
    def groundValue(v: Double): GroundValue = GDouble(v)
  }

  /** Real instance for Interval */
  implicit object intervalIsReal extends Real[Interval] {
    def add(l: Interval, r: Interval): Interval = l + r
    def sub(l: Interval, r: Interval): Interval = l - r
    def mul(l: Interval, r: Interval): Interval = l * r
    def div(l: Interval, r: Interval): Interval = l / r
    def pow(l: Interval, r: Interval): Interval = l pow r
    def neg(x: Interval): Interval = -x
    def sin(x: Interval): Interval = x.sin
    def cos(x: Interval): Interval = x.cos
    def tan(x: Interval): Interval = x.tan
    def acos(x: Interval): Interval = x.acos
    def asin(x: Interval): Interval = x.asin
    def atan(x: Interval): Interval = x.atan
    def exp(x: Interval): Interval = x.exp
    def log(x: Interval): Interval = x.log
    def square(x: Interval): Interval = x.square
    def sqrt(x: Interval): Interval = x.sqrt
    def fromInt(x: Int): Interval = Interval(x)
    def fromDouble(x: Double): Interval = Interval(x)
    def zero: Interval = Interval.zero
    def one: Interval = Interval.one
    def ==(a: Interval, b: Interval): Boolean = a == b
    def !=(a: Interval, b: Interval): Boolean = a != b
    def isValidInt(x: Interval): Boolean = x.lo.doubleValueFloor == x.hi.doubleValueCeiling
    def toInt(x: Interval): Int = x.lo.doubleValueFloor.toInt 
    def isValidDouble(x: Interval): Boolean = (x.lo == x.hi)
    def toDouble(x: Interval): Double = (x.lo).doubleValueFloor
    def lteq(l: Interval, r: Interval): Boolean = l lessThanOrEqualTo r
    def tryCompare(l: Interval, r: Interval): Option[Int] =
      if (l == r)         Some(0)
      else if (lteq(l,r)) Some(1) 
      else if (lteq(r,l)) Some(-1)
      else                None
    def groundValue(v: Interval) = GInterval(v)
  }
  
  /** Representation of a number and its derivatives, indexed by the type I. */
  abstract class Dif[V : Integral, Id] {
    /** Returns the element of the Dif corresponding to i when 
     *  i < length and otherwise the zero of V. */
    def apply(i: Id): V
    /** Returns an integer > 0. */
    def length: Int
    /** Returns the leading coefficient of the Dif. */
    def head: V
    /** Apply m to every element. */
    def map[W: Integral](m: V => W): Dif[W,Id] 
  }
  
  abstract class DifAsIntegral[V: Integral, Id, D <: Dif[V,Id]] {
    /* Constants */
    val evVIsIntegral = implicitly[Integral[V]]
    val zeroOfV = evVIsIntegral.zero
    val oneOfV = evVIsIntegral.one
    /* Integral instance */
    protected def combinedLength(l: D, r: D) = Math.max(l.length, r.length)
    def toInt(x: D): Int = x.head.toInt
    def toDouble(x: D): Double = x.head.toDouble
    def isValidInt(x: D): Boolean = evVIsIntegral.isValidInt(x.head) && isConstant(x)
    def isValidDouble(x: D): Boolean = evVIsIntegral.isValidDouble(x.head) && isConstant(x)
    def isConstant(x: D): Boolean
    def tryCompare(l: D, r: D): Option[Int] = evVIsIntegral.tryCompare(l.head, r.head)
    def lteq(l: D, r: D): Boolean = evVIsIntegral.lteq(l.head, r.head)
  }
  
}