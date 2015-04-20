package acumen.interpreters

import enclosure.Interval

/**
 * Taylor series arithmetic.
 *
 * Based on description from book "Validated Numerics" by
 * Warrick Tucker, pages 121 and 122.
 */
case class Taylor(dim: Int) extends App {

  case class Dif[A](coeff: Vector[A]) {
    require(coeff.size == dim, "Tried to create Dif with incompatible dimension.")
    def apply(i: Int) = coeff(i)
    def size: Int = coeff.size
  }
  object Dif {
    /** Lift a constant value of type A to a Dif. */
    def constant[A: Num](a: A) = new Dif[A](Vector(a, implicitly[Num[A]].zero).padTo(dim, implicitly[Num[A]].zero))
    /** Lift a variable of type A to a Dif. The value a is the current value of the variable. */
    def variable[A: Num](a: A) = new Dif(Vector(a, implicitly[Num[A]].one).padTo(dim, implicitly[Num[A]].zero))
    /**
     * Use a sequence of values of type A as the coefficients of a Dif.
     *  The sequence is padded to dim with the zero of A.
     */
    def apply[A: Num](as: A*) = new Dif(as.toVector.padTo(dim, implicitly[Num[A]].zero))
    /** Use a repeated sequence of the value a as the coefficients of a Dif. */
    def fill[A: Num](a: A) = new Dif(Vector.fill(dim)(a))
  }

  trait Num[A] {
    def plus(l: A, r: A): A
    def minus(l: A, r: A): A
    def mul(l: A, r: A): A
    def div(l: A, r: A): A
    def zero: A
    def one: A
  }
  implicit class NumOps[A](val lhs: A)(implicit ev: Num[A]) {
    def +(rhs: A): A = ev.plus(lhs, rhs)
    def -(rhs: A): A = ev.minus(lhs, rhs)
    def *(rhs: A): A = ev.mul(lhs, rhs)
    def /(rhs: A): A = ev.div(lhs, rhs)
    def zero: A = ev.zero
    def one: A = ev.one
  }

  implicit object DoubleIsNum extends Num[Double] {
    def plus(l: Double, r: Double): Double = l + r
    def minus(l: Double, r: Double): Double = l - r
    def mul(l: Double, r: Double): Double = l * r
    def div(l: Double, r: Double): Double = l / r
    def zero: Double = 0
    def one: Double = 1
  }
  implicit object IntervalIsNum extends Num[Interval] {
    def plus(l: Interval, r: Interval): Interval = l + r
    def minus(l: Interval, r: Interval): Interval = l - r
    def mul(l: Interval, r: Interval): Interval = l * r
    def div(l: Interval, r: Interval): Interval = l / r
    def zero: Interval = Interval.zero
    def one: Interval = Interval.one
  }
  class DifAsNum[A: Num] extends Num[Dif[A]] {
    /* Caches */
    val mulCache = collection.mutable.HashMap[(Dif[A], Dif[A]), Dif[A]]()
    val divCache = collection.mutable.HashMap[(Dif[A], Dif[A]), Dif[A]]()
    /* Constants */
    val zeroOfA = implicitly[Num[A]].zero
    val oneOfA = implicitly[Num[A]].one
    /* Num instance */
    def plus(l: Dif[A], r: Dif[A]): Dif[A] = Dif((l.coeff, r.coeff).zipped.map(_ + _))
    def minus(l: Dif[A], r: Dif[A]): Dif[A] = Dif((l.coeff, r.coeff).zipped.map(_ - _))
    def mul(l: Dif[A], r: Dif[A]): Dif[A] =
      // FIXME Implement using mutable data, but keep the immutable interface
      mulCache.getOrElseUpdate((l, r),
        Dif((for (k <- 0 until l.size) yield ((0 to k).foldLeft(zeroOfA) {
          case (sum, i) => sum + (l(i) * r(k - i))
        })).toVector))
    def div(l: Dif[A], r: Dif[A]): Dif[A] =
      divCache.getOrElseUpdate((l, r), Dif {
        val n = l.size
        val coeff = new collection.mutable.ArraySeq[A](n)
        coeff(0) = l(0) / r(0)
        for (k <- 1 until n)
          coeff(k) = (l(k) - (0 to k - 1).foldLeft(zeroOfA) {
            case (sum, i) => sum - (coeff(i) * r(k - i))
          }) / r(0)
        coeff.toVector
      })
    // FIXME Test these definitions
    def zero: Dif[A] = Dif.fill(zeroOfA)
    def one: Dif[A] = Dif.constant(oneOfA)
  }
  implicit object DoubleDifIsNum extends DifAsNum[Double]
  implicit object IntervalDifIsNum extends DifAsNum[Interval]

}
