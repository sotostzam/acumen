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
    s <- choose(0.0, 1.0)
    val r = ((i.width.low / 2).low * s).low
  } yield (i.low + r) /\ (i.high - r)

  def genIntervalFilter(implicit r: Rounding): Gen[Interval] = for {
    val lo <- arbitrary[Double]
    val hi <- genLargerDouble(lo)
  } yield Interval(lo, hi)

  /* VarName */

  /** Generates a random VarName */
  def genVarName: Gen[VarName] = for {
    c <- alphaChar
  } yield c.toString.asInstanceOf[VarName]
  implicit val arbitraryVarName = Arbitrary(genVarName)

  /* Box */

  /** Generates a plain box. */
  def genBox(implicit r: Rounding): Gen[Box] = for {
    dim <- posNum[Int]
    names <- listOfN(dim, arbitrary[VarName])
    intervals <- listOfN(dim, arbitrary[Interval])
  } yield (names zip intervals).toMap.asInstanceOf[Box]
  implicit val arbitraryBox = Arbitrary(genBox)

  /** Generates a dim-dimensional box. */
  def genDimBox(dim: Int)(implicit r: Rounding): Gen[Box] = for {
    names <- listOfN(dim, arbitrary[VarName])
    intervals <- listOfN(dim, arbitrary[Interval])
  } yield (names zip intervals).toMap.asInstanceOf[Box]

  /** Generates a sub-box of the box. */
  def genSubBox(box: Box)(implicit r: Rounding): Gen[Box] = for {
    subintervals <- Gen.sequence[List, Interval](box.values.map(genSubInterval(_)))
  } yield (box.keys zip subintervals).toMap

  /* AffineScalarEnclosure */

  /** Generates a plain enclosure. */
  def genAffineScalarEnclosure(implicit r: Rounding): Gen[AffineScalarEnclosure] = for {
    dim <- posNum[Int]
    names <- listOfN(dim, arbitrary[VarName])
    domains <- listOfN(dim, arbitrary[Interval])
    constant <- arbitrary[Interval]
    coeffs <- listOfN(dim, arbitrary[Interval])
    val domain = (names zip domains).toMap.asInstanceOf[Box]
    val coefficients = (names zip coeffs).toMap.asInstanceOf[Box]
  } yield AffineScalarEnclosure(domain, constant, coefficients)
  implicit val arbitraryAffineScalarEnclosure = Arbitrary(genAffineScalarEnclosure)

  /** Generates a dim-dimensional enclosure. */
  def genDimAffineScalarEnclosure(dim: Int)(implicit r: Rounding): Gen[AffineScalarEnclosure] = for {
    names <- listOfN(dim, arbitrary[VarName])
    domains <- listOfN(dim, arbitrary[Interval])
    constant <- arbitrary[Interval]
    coeffs <- listOfN(dim, arbitrary[Interval])
    val domain = (names zip domains).toMap.asInstanceOf[Box]
    val coefficients = (names zip coeffs).toMap.asInstanceOf[Box]
  } yield AffineScalarEnclosure(domain, constant, coefficients)

  /** Generates an enclosure over the box. */
  def genBoxAffineScalarEnclosure(box: Box)(implicit r: Rounding): Gen[AffineScalarEnclosure] = for {
    constant <- arbitrary[Interval]
    coeffs <- listOfN(box.size, arbitrary[Interval])
    val coefficients = (box.keys zip coeffs).toMap.asInstanceOf[Box]
  } yield AffineScalarEnclosure(box, constant, coefficients)

  /* --- Utilities --- */

  /** Returns an version of the input Interval i, padded below and above by loPad and hiPad respectively. */
  def pad(i: Interval, loPad: Double, hiPad: Double): Interval = {
    i + Interval(-loPad, hiPad)
  }

}