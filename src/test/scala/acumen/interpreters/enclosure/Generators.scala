package acumen.interpreters.enclosure

import scala.math.min
import scala.math.max

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

import acumen.interpreters.enclosure.Interval._
import acumen.interpreters.enclosure.Types._

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
  def genInterval(implicit r: Rounding): Gen[Interval] = for {
    val lo <- arbitrary[Double]
    val hi <- arbitrary[Double]
  } yield Interval(min(lo,hi), max(lo,hi))
  implicit val arbitraryInterval = Arbitrary(genInterval)

  def genIntervalFilter(implicit r: Rounding): Gen[Interval] = for {
    val lo <- arbitrary[Double]
    val hi <- genLargerDouble(lo)
  } yield Interval(lo, hi)

  /* Box */

  /** Generates a plain box. */
  def genBox(implicit r: Rounding): Gen[Box] = for {
    dim <- posNum[Int]
    names <- listOfN(dim, alphaChar)
    intervals <- listOfN(dim, arbitrary[Interval])
  } yield (names.map(_.toString) zip intervals).toMap.asInstanceOf[Box]

   /** Generates a box of dimension dim. */
  def genDimBox(dim:Int)(implicit r: Rounding): Gen[Box] = for {
    names <- listOfN(dim, alphaChar)
    intervals <- listOfN(dim, arbitrary[Interval])
  } yield (names.map(_.toString) zip intervals).toMap.asInstanceOf[Box]

  /* --- Utilities --- */

  /** Returns an version of the input Interval i, padded below and above by loPad and hiPad respectively. */
  def pad(i: Interval, loPad: Double, hiPad: Double): Interval = {
    i + Interval(-loPad, hiPad)
  }

}