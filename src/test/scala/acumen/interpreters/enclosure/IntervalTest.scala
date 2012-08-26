package acumen.interpreters.enclosure

import org.scalacheck.Gen
import org.scalacheck.Gen.posNum
import org.scalacheck.Gen.sized
import org.scalacheck.Gen.choose
import org.scalacheck.Properties
import org.scalacheck.Prop._
import org.scalacheck.Arbitrary.arbitrary
import Interval.min
import Interval.max
import java.math.RoundingMode
import scala.collection.immutable.List
import scala.math.abs
import Generators._
import Interval._
import org.scalacheck.Prop

object IntervalTest extends Properties("Interval") {

  //TODO Test min and max

  import TestingContext._

  /* Properties */

  /**
   * Property (monotonicity w.r.t. precision): given doubles a < b and
   * positive integers i < j and intervals X = [a,b] initialized with
   * precision i and Y = [a,b] initialized with precision j it should
   * hold that the set defined by X contains the set defined by Y.
   */
  property("monotonicity w.r.t. precision") =
    forAll(arbitrary[Double], arbitrary[Double], posNum[Int], posNum[Int]) {
      (a, b, i, j) =>
        val (lo, hi) = if (a < b) (a, b) else (b, a)
        val (lop, hip) = if (i < j) (i, j) else (j, i)
        val x = Interval(lo, hi)(Rounding(lop))
        val y = Interval(lo, hi)(Rounding(hip))
        x contains y
    }

  /**
   * Property (monotonicity of functions): for each interval function f
   * it should hold that given intervals {A_1,...,A_n} and {B_1,...,B_n}
   * such that A_i is contained in B_i for each i, f(A_1,...,A_n) is
   * contained in f(B_1,...,B_n).
   */
  property("monotonicity of unary functions") =
    forAll(genInterval, posNum[Double]) {
      (a, p) =>
        (p >= 0) ==> {
          // Unary negation
          -pad(a, p, p) contains -a
        }
    }

  property("monotonicity of binary functions (+)") = monoOfBinaryFun(_ + _)
  property("monotonicity of binary functions (-)") = monoOfBinaryFun(_ - _)
  property("monotonicity of binary functions (*)") = monoOfBinaryFun(_ * _)
  property("monotonicity of binary functions (/)") = monoOfBinaryFun(_ / _, arbitrary[Interval], genNonZeroInterval)
  property("monotonicity of binary functions (/\\)") = monoOfBinaryFun(_ /\ _)
  property("monotonicity of binary functions (\\/)") =
    forAll(
      arbitrary[Double], arbitrary[Double], arbitrary[Double], arbitrary[Double],
      posNum[Double], posNum[Double], posNum[Double], posNum[Double]) {
        case (p1, p2, p3, p4, lLoPad, lHiPad, rLoPad, rHiPad) =>
          val lst = List(p1, p2, p3, p4).sorted
          val al = Interval(lst(0), lst(2))
          val ar = Interval(lst(1), lst(3))
          val bl = pad(al, lLoPad, lHiPad)
          val br = pad(ar, rLoPad, rHiPad)
          (bl \/ br) contains (al \/ ar)
      }

  /**
   * /\ is an approximation of the superset-meet in the interval lattice. The meet
   * itself may not be exactly representable, so we get some lesser lower bound
   * instead.
   *
   * property: for any intervals A and B it holds that A /\ B contains both A and B.
   */
  property("/\\ on Intervals is the interval union") =
    forAll { (a: Interval, b: Interval) =>
      (a /\ b contains a) && (a /\ b contains b)
    }

  /* Utilities */

  /** Checks that the binary function "f" is monotonic, using randomly chosen input intervals. */
  def monoOfBinaryFun(f: (Interval, Interval) => Interval): Prop = monoOfBinaryFun(f, arbitrary[Interval], arbitrary[Interval])

  /** Checks that the binary function "f" is monotonic, i.e. that if A contains B then f(A) contains f(B). */
  def monoOfBinaryFun(f: (Interval, Interval) => Interval, genL: Gen[Interval], genR: Gen[Interval]) =
    forAll(genL, genR) { (l, r) =>
      forAll(genSubInterval(l), genSubInterval(r)) { (subl, subr) =>
        f(l, r) contains f(subl, subr)
      }
    }

}

object IntervalUnitTest extends Properties("Interval.UnitTest") {

  //TODO Test min and max

  import TestingContext._

  /* Properties */
  property("scala interval expressions - sums") = {
    Interval(0, 1) + Interval(-1, 0) == Interval(-1, 1) &&
      Interval(0, 1) + Interval(-2, 2) == Interval(-2, 3) &&
      Interval(-1, 0) + Interval(-3, -2) == Interval(-4, -2) &&
      Interval(-2, 2) + Interval(1) == Interval(-1, 3) &&
      Interval(0, 1) + Interval(-3, -2) == Interval(-3, -1)
  }

  property("scala interval expressions - products") = {
    Interval(-3, -1) * Interval(-3, -1) == Interval(1, 9) &&
      Interval(-3, -1) * Interval(-3, 0) == Interval(0, 9) &&
      Interval(4) * Interval(-1) == Interval(-4) &&
      Interval(-3, -1) * Interval(-3) == Interval(3, 9)
  }

  property("scala interval expressions - differences") = {
    Interval(-3, -1) - Interval(-3, -1) == Interval(-2, 2) &&
      Interval(-3, -1) - Interval(-3, 0) == Interval(-3, 2) &&
      Interval(4) - Interval(-1) == Interval(5) &&
      Interval(-3, -1) - Interval(-3) == Interval(0, 2)
  }

  property("scala interval expressions - quotients") = {
    Interval(-4, -1) / Interval(-4, -1) == Interval(0.25, 4) &&
      Interval(-1, 3) / Interval(-3, -1) == Interval(-3, 1) &&
      Interval(4) / Interval(-1) == Interval(-4) &&
      Interval(-3, 3) / Interval(-3) == Interval(-1, 1)
  }

}