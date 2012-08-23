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
  } yield Interval(min(lo, hi), max(lo, hi))
  implicit val arbitraryInterval = Arbitrary(genInterval)

  /** Generates a positive interval */
  def genPosInterval(implicit r: Rounding): Gen[Interval] = for {
    val lo <- posNum[Double]
    val hi <- posNum[Double]
  } yield Interval(min(lo, hi), max(lo, hi))

  /** Generates a non-zero interval */
  def genNonZeroInterval(implicit r: Rounding): Gen[Interval] = for {
    i <- genPosInterval
    b <- arbitrary[Boolean]
  } yield if (b) i else -i

  /** Generates a sub-interval of the interval. */
  def genSubInterval(i: Interval)(implicit r: Rounding): Gen[Interval] = for {
    s <- posNum[Double]
  } yield i / (Interval(s) + Interval(1))

  /** Generates a super-interval of the interval. */
  def genSupInterval(i: Interval)(implicit r: Rounding): Gen[Interval] = for {
    s <- posNum[Double]
  } yield i * (Interval(s) + Interval(1))

  def genIntervalFilter(implicit r: Rounding): Gen[Interval] = for {
    val lo <- arbitrary[Double]
    val hi <- genLargerDouble(lo)
  } yield Interval(lo, hi)

  /* VarName */

  /** Generates a random VarName */
  def genVarName: Gen[VarName] = for {
    c <- alphaChar
  } yield c.toString.asInstanceOf[VarName]

  /* Box */

  /** Generates a plain box. */
  def genBox(implicit r: Rounding): Gen[Box] = for {
    dim <- posNum[Int]
    intervals <- listOfN(dim, arbitrary[Interval])
  } yield (listOfNames(dim) zip intervals).toMap.asInstanceOf[Box]

  /** Generates a box of dimension dim. */
  def genDimBox(dim: Int)(implicit r: Rounding): Gen[Box] = for {
    intervals <- listOfN(dim, arbitrary[Interval])
  } yield (listOfNames(dim) zip intervals).toMap.asInstanceOf[Box]

  /* AffineScalarEnclosure */

  /** Generates a plain enclosure */
  def genAffineScalarEnclosure(implicit r: Rounding): Gen[AffineScalarEnclosure] = for {
    dim <- posNum[Int]
    names <- listOfN(dim, genVarName)
    domains <- listOfN(dim, arbitrary[Interval])
    constant <- arbitrary[Interval]
    coeffs <- listOfN(dim, arbitrary[Interval])
    val domain = (names zip domains).toMap.asInstanceOf[Box]
    val coefficients = (names zip coeffs).toMap.asInstanceOf[Box]
  } yield AffineScalarEnclosure(domain, constant, coefficients)

  /* --- Utilities --- */

  /** Returns an version of the input Interval i, padded below and above by loPad and hiPad respectively. */
  def pad(i: Interval, loPad: Double, hiPad: Double): Interval = {
    i + Interval(-loPad, hiPad)
  }

  /** Returns a list of strings of the form t_i, where 0 <= i <= dim. For dim <= 0 an empty list is generated. */
  def listOfNames(dim: Int): Seq[VarName] = for { i <- 0 to dim } yield "t_" + i

}