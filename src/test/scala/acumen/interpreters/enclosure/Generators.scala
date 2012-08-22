package acumen.interpreters.enclosure

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalacheck.Gen.alphaChar
import org.scalacheck.Gen.alphaStr
import org.scalacheck.Gen.posNum
import org.scalacheck.Gen.listOfN
import org.scalacheck.Gen.sized
import org.scalacheck.Gen.choose
import org.scalacheck.Gen.oneOf

import acumen.interpreters.enclosure.Interval.Real

object Generators {

  import TestingContext._

  /* Double */

  /** Generates Doubles larger than the parameter d. */
  def genLargerDouble(d: Double): Gen[Double] = {
    // TODO Verify that this works correctly 
    val rest = Double.MaxValue + (if (d < 0) d else -d)
    for { val delta <- choose(0, if (rest < 0) -rest else rest) } yield d + delta
  }

  /* Interval */

  /** Generates a random interval. */
  def intervalGen(implicit r: Rounding): Gen[Interval] = for {
    val lo <- arbitrary[Double]
    val hi <- genLargerDouble(lo)
  } yield Interval(lo, hi)
  implicit val arbitraryInterval = Arbitrary(intervalGen)

  def intervalFilterGen(implicit r: Rounding): Gen[Interval] = for {
    val lo <- arbitrary[Double]
    val hi <- genLargerDouble(lo)
  } yield Interval(lo, hi)

  /* --- Utilities --- */

  /** Returns an version of the input Interval i, padded below and above by loPad and hiPad respectively. */
  def pad(i: Interval, loPad: Double, hiPad: Double): Interval = {
    i + Interval(-loPad, hiPad)
  }

}