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

import AffineScalarEnclosure._

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
  def genInterval(implicit rnd: Rounding): Gen[Interval] = for {
    val lo <- arbitrary[Double]
    val hi <- arbitrary[Double]
  } yield Interval(min(lo, hi), max(lo, hi))
  implicit val arbitraryInterval = Arbitrary(genInterval)

  /** Generates a positive interval */
  def genPosInterval(implicit rnd: Rounding): Gen[Interval] = for {
    val lo <- posNum[Double]
    val hi <- posNum[Double]
  } yield Interval(min(lo, hi), max(lo, hi))

  /** Generates a non-zero interval */
  def genNonZeroInterval(implicit rnd: Rounding): Gen[Interval] = for {
    i <- genPosInterval
    b <- arbitrary[Boolean]
  } yield if (b) i else -i

  /** Generates a sub-interval of the interval. */
  def genSubInterval(i: Interval)(implicit rnd: Rounding): Gen[Interval] = for {
    s <- choose(0.0, 1.0)
    val r = ((i.width.low / 2).low * s).low
  } yield (i.low + r) /\ (i.high - r)

  /** Generates a point in the interval. */
  def genThinSubInterval(i: Interval)(implicit rnd: Rounding) = for {
    s <- choose(0.0, 1.0)
  } yield i.low + i.width.low * s

  def genIntervalFilter(implicit rnd: Rounding): Gen[Interval] = for {
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
  def genBox(implicit rnd: Rounding): Gen[Box] = for {
    dim <- posNum[Int]
    intervals <- listOfN(dim, arbitrary[Interval])
  } yield (listOfNames(dim) zip intervals).toMap.asInstanceOf[Box]

  /** Generates a dim-dimensional box. */
  def genDimBox(dim: Int)(implicit rnd: Rounding): Gen[Box] = for {
    intervals <- listOfN(dim, arbitrary[Interval])
  } yield (listOfNames(dim) zip intervals).toMap.asInstanceOf[Box]

  /** Generates a sub-box of the box. */
  def genSubBox(box: Box)(implicit rnd: Rounding): Gen[Box] = for {
    subIntervals <- Gen.sequence[List, Interval](box.values.map(genSubInterval(_)))
  } yield (box.keys zip subIntervals).toMap

  /** Generates a point in the box. */
  def genThinSubBox(box: Box)(implicit rnd: Rounding): Gen[Box] = for {
    thinSubIntervals <- Gen.sequence[List, Interval](box.values.map(genThinSubInterval(_)))
  } yield (box.keys zip thinSubIntervals).toMap

  /* AffineScalarEnclosure */

  /** Generates a plain enclosure. */
  def genAffineScalarEnclosure(implicit rnd: Rounding): Gen[AffineScalarEnclosure] = for {
    dim <- posNum[Int]
    names <- listOfN(dim, arbitrary[VarName])
    domains <- listOfN(dim, arbitrary[Interval])
    constant <- arbitrary[Interval]
    coeffs <- listOfN(dim, arbitrary[Interval])
    val domain = (names zip domains).toMap.asInstanceOf[Box]
    val coefficients = (names zip coeffs).toMap.asInstanceOf[Box]
  } yield AffineScalarEnclosure(domain, Box.normalize(domain), constant, coefficients)
  implicit val arbitraryAffineScalarEnclosure = Arbitrary(genAffineScalarEnclosure)

  /** Generates a dim-dimensional enclosure. */
  def genDimAffineScalarEnclosure(dim: Int)(implicit rnd: Rounding): Gen[AffineScalarEnclosure] = for {
    names <- listOfN(dim, arbitrary[VarName])
    domains <- listOfN(dim, arbitrary[Interval])
    constant <- arbitrary[Interval]
    coeffs <- listOfN(dim, arbitrary[Interval])
    val domain = (names zip domains).toMap.asInstanceOf[Box]
    val coefficients = (names zip coeffs).toMap.asInstanceOf[Box]
  } yield AffineScalarEnclosure(domain, Box.normalize(domain), constant, coefficients)

  /** Generates an enclosure over the box. */
  def genBoxAffineScalarEnclosure(box: Box)(implicit rnd: Rounding): Gen[AffineScalarEnclosure] = for {
    constant <- arbitrary[Interval]
    coeffs <- listOfN(box.size, arbitrary[Interval])
    val coefficients = (box.keys zip coeffs).toMap.asInstanceOf[Box]
  } yield AffineScalarEnclosure(box, Box.normalize(box), constant, coefficients)

  /** Generates a sub-enclosure of the enclosure. */
  def genSubAffineScalarEnclosure(f: AffineScalarEnclosure)(implicit rnd: Rounding): Gen[AffineScalarEnclosure] = { 
    for {
      subconst <- genSubInterval(f.constant)
      subcoeffs <- Gen.sequence[List, Interval](f.coefficients.values.map(genSubInterval(_)))
    } yield AffineScalarEnclosure(f.domain, f.normalizedDomain, subconst, (f.domain.keys zip subcoeffs).toMap)
  }

  /* --- Utilities --- */

  /** Returns an version of the input Interval i, padded below and above by loPad and hiPad respectively. */
  def pad(i: Interval, loPad: Double, hiPad: Double): Interval = {
    i + Interval(-loPad, hiPad)
  }

  /** Returns a list of strings of the form t_i, where 0 <= i <= dim. For dim <= 0 an empty list is generated. */
  def listOfNames(dim: Int): Seq[VarName] = for { i <- 0 to dim } yield "t_" + i

}