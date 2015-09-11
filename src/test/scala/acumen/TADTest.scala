package acumen

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.Gen
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck.Properties
import testutil.Generators._
import interpreters.enclosure.Interval
import interpreters.enclosure.Generators._
import TAD._
import acumen.interpreters.reference2015.Interpreter.FieldImpl

object ADTest extends Properties("TAD") {

  val maxLength = 10 // Maximum number of coefficients used in generators
  val smallValue = 10 // Bound used for generating "small" values
  
  /* Utilities */
  
  val evDoubleTDifIsIntegral = implicitly[Integral[TDif[Double]]]
  val evDoubleTDifIsReal = implicitly[Real[TDif[Double]]]
      
  val evIntervalTDifIsIntegral = implicitly[Integral[TDif[Interval]]]
  val evIntervalTDifIsReal = implicitly[Real[TDif[Interval]]]
              
  def fromIntD(i: Int) = evDoubleTDifIsIntegral.fromInt(i)
  val zeroD = evDoubleTDifIsIntegral.zero
  val oneD = evDoubleTDifIsIntegral.one
  val piD = evDoubleTDifIsReal.fromDouble(Math.PI)
  
  def fromIntI(i: Int) = evIntervalTDifIsIntegral.fromInt(i)
  val zeroI = evIntervalTDifIsIntegral.zero
  val oneI = evIntervalTDifIsIntegral.one
  val piI = evIntervalTDifIsReal.fromDouble(Math.PI)
  
  trait Similar[T] {
    def close(l: T, r: T): Boolean
    def in(l: T, r: T): Boolean
  }
  implicit object DoubleIsSimilar extends Similar[Double] {
    def close(l: Double, r: Double): Boolean = 
      Math.abs(l - r) <= 10e-6 * Math.min(Math abs l, Math abs r)
    def in(l: Double, r: Double): Boolean =
      l == r
  }
  implicit object IntervalIsSimilar extends Similar[Interval] {
    def close(l: Interval, r: Interval): Boolean = 
      (l intersect r).isDefined
    def in(l: Interval, r: Interval): Boolean =
      r contains l
  }
  implicit class SimilarOps[V](val l: V)(implicit ev: Similar[V]) {
    def ~=(r: V): Boolean = ev.close(l, r)
    def in(r: V): Boolean = ev.in(l, r)
  }
  
  implicit class TDifIsSimilar[T : Similar](l: TDif[T]) {
    /** Compare two TDifs coefficient-wise using comp, ignoring first n coefficients */
    def similar(r: TDif[T], comp: (T,T) => Boolean, msg: String, n: Int): Boolean =
      (0 to Math.max(l.length, r.length)).forall(i => 
          if (comp(l(i), r(i))) true
          else sys.error(s"\n\nFound $msg at index $i:\n\n" +
              s"Left coefficient:\n${l(i)}\n\nRight coefficient:\n${r(i)}\n\n" +
              s"Full left TDif:\n$l\n\nFull right TDif:\n$r\n\n"))
    def ~=(r: TDif[T]): Boolean = similar(r, (lc:T,rc:T) => lc ~= rc, "dissimilarity", 0)
    /** Ignore first n coefficients */
    def ~=(n: Int)(r: TDif[T]): Boolean = similar(r, (lc:T,rc:T) => lc ~= rc, "dissimilarity", n)
    def in(r: TDif[T]): Boolean = similar(r, (lc:T,rc:T) => lc in rc, "non-inclusion", 0)
  }
  
  /* Properties of TDif[Double] as Integral */

  property("TDif[Double].add: x+x ~= 2*x") = 
    forAll(genSmallDoubleTDif()) { (x: TDif[Double]) =>
      (x + x) ~= (fromIntD(2) * x)
    }
  
  property("TDif[Double].add: 1+x > x") = 
    forAll(genSmallDoubleTDif()) { (x: TDif[Double]) =>
      (oneD + x) > x
    }
  
  property("TDif[Double].mul: x*x >= x") = 
    forAll { (x: TDif[Double]) =>
      (x * x) >= x  
    }  

  property("TDif[Double].mul: x*1 ~= x && 1*x ~= x") = 
    forAll { (x: TDif[Double]) =>
      ((x * oneD) ~= x) && ((oneD * x) ~= x) 
    }  
  
  property("TDif[Double].div: x/1 ~= x") = 
    forAll { (x: TDif[Double]) =>
      (x / oneD) ~= x
    }

  property("TDif[Double].div: x/x ~= 1") = 
    forAll { (x: TDif[Double]) =>
      (x(0) != 0d) ==> ((x / x) ~= oneD)  
    }
  
  /* Properties of TDif[Double] as Real */
  
  property("TDif[Double]: sin(x) ~= cos(pi/2 - x)") = 
    forAll(genSmallDoubleTDif()) { (x: TDif[Double]) =>
      x.sin ~= ((piD / fromIntD(2)) - x).cos
    }

  property("TDif[Double]: cos(x) ~= sin(pi/2 - x)") = 
    forAll(genSmallDoubleTDif()) { (x: TDif[Double]) =>
      x.cos ~= ((piD / fromIntD(2)) - x).sin
    }

  property("TDif[Double]: x != 0 => tan(x) ~= sin(x) / cos(x)") = 
    forAll(genSmallDoubleTDif()) { (x: TDif[Double]) =>
      (x != zeroD) ==> (x.tan ~= x.sin / x.cos)
    }

  property("TDif[Double]: tan(x) ~= tan(x + pi)") = 
    forAll(genSmallDoubleTDif()) { (x: TDif[Double]) =>
      x.tan ~= (x + piD).tan
    }
  
  property("TDif[Double]: tan(atan(x)) ~= x") = 
    forAll(genSmallDoubleTDif()) { (x: TDif[Double]) =>
      x.atan.tan ~= x
    }

  property("TDif[Double]: exp(x) > 0") =
    // Generate x such that exp(x) is representable as a Double
    forAll(genBoundedDoubleTDif(Math.log(Double.MaxValue))) { (x: TDif[Double]) =>
      x.exp > zeroD
    }
  
  property("TDif[Double]: for small x, log(exp(x)) ~= x") = 
    forAll(genSmallDoubleTDif()) { (x: TDif[Double]) =>
      x.exp.log ~= x
    }

  property("TDif[Double]: x > 1 => sqrt(x) < x") =
    forAll(genSmallDoubleTDif()) { (x: TDif[Double]) =>
      (x > oneD) ==> x.sqrt < x
    }
  
  property("TDif[Double]: x >= 1 => square(x) >= x") =
    forAll(genSmallDoubleTDif()) { (x: TDif[Double]) =>
      (x >= oneD) ==> x.square >= x
    }
  
  property("TDif[Double]: square(x) ~= x*x") =
    forAll(genSmallDoubleTDif()) { (x: TDif[Double]) =>
      x.square ~= x*x
    }
  
  property("TDif[Double]: pow(0) ~= 1") = 
    forAll(genSmallDoubleTDif()) { (x: TDif[Double]) =>
      (x ^ zeroD) ~= oneD
    }
  
  property("TDif[Double]: 0 < x < 1 && y > 1 => x^y < x") = 
    forAll(genTDif(choose(0.01,0.99)), genTDif(choose(1.0,10))) { 
      (x: TDif[Double], y: TDif[Double]) =>
        (x ^ y) < x
    }
  
  /* Properties of TDif[Interval] as Real */
  
  property("TDif[Interval]: -1 <= x <= 1 => x in cos(acos(x))") = 
    forAll(genBoundedThinIntervalTDif(-1,1)) { (x: TDif[Interval]) =>
      x in x.acos.cos
    }

  property("TDif[Interval]: -1 <= x <= 1 => x in sin(asin(x))") = 
    forAll(genBoundedThinIntervalTDif(-1,1)) { (x: TDif[Interval]) =>
      x in x.asin.sin
    }
      
  property("TDif[Interval]: x >= 0 => x in sqrt(x*x)") =
    forAll(genBoundedThinIntervalTDifEvenLeadingZeros) { (x: TDif[Interval]) =>
      x in (x*x).sqrt
    }

  property("TDif[Interval]: x >= 0 => x in square(sqrt(x))") =
    forAll(genBoundedThinIntervalTDifEvenLeadingZeros) { (x: TDif[Interval]) =>
      x in x.sqrt.square
    }
  
  property("TDif[Interval]: x >= 0 => x in (sqrt(x))^2") =
    forAll(genBoundedThinIntervalTDifEvenLeadingZeros) { (x: TDif[Interval]) =>
      x in x.sqrt^2
    }

  property("TDif[Interval]: x >= 0 => x in sqrt(x^2)") =
    forAll(genBoundedThinIntervalTDifEvenLeadingZeros) { (x: TDif[Interval]) =>
      x in (x^2).sqrt
    }
  
  property("TDif[Interval]: 1 in x^0") =
    forAll(genSmallThinIntervalTDif()) { (x: TDif[Interval]) =>
      IntervalDifIsReal.fromInt(1) in x^0 
    }
  
  property("TDif[Interval]: x in x^1") =
    forAll(genSmallThinIntervalTDif()) { (x: TDif[Interval]) =>
      x in x^1
    }
  
  property("TDif[Interval]: x in (x^3 / x^2)") =
    forAll(genSmallThinIntervalTDif()) { (x: TDif[Interval]) =>
      x in ((x^3) / (x^2))
    }
  
  property("TDif[Interval]: a > 0 => x^a ~= x*x*...*x (a times)") =
    forAll(genBoundedThinIntervalTDif(0,1), chooseNum[Int](1, 20)) { (x: TDif[Interval], a: Int) =>
      val xa = List.fill(a)(x)
      xa.nonEmpty ==> ((x^a) ~= xa.reduce(_*_))
    }

  property("TDif[Interval]: n > 0 => 1^n = 1") =
    forAll (posNum[Int]) { (n: Int) =>
      (IntervalDifIsReal.one ^ n) ~= IntervalDifIsReal.one 
    }

  property("TDif[Interval]: x^0 = 1") =
    forAll(genSmallThinIntervalTDif()) { (x: TDif[Interval]) =>
      (x^0) ~= IntervalDifIsReal.one
    }
  
  property("TDif[Interval]: x > 1 && n < 0 => x^n <= x") =
    forAll(genBoundedThinIntervalTDif(1,10), negNum[Int]) { (x: TDif[Interval], n: Int) =>
      (x^n) <= x
    }
  
  /* Other properties */
  
  property("lift leaves no base number types") = forAll { (gv: GroundValue) =>
    lift(Lit(gv)) match {
      case Lit(_: GDouble | _: GInt | _: GInterval) => false
      case _ => true
    }
  }

  /* Generic generators */

  def genTDif[N: Integral](genN: Gen[N], leadingZeros: Int = 0): Gen[TDif[N]] =
    genCoeffs(genN, leadingZeros).map(c => TDif(c, c.length))
    
  def genCoeffs[N: Integral](genN: Gen[N], leadingZeros: Int): Gen[Vector[N]] =
    if (leadingZeros > maxLength)
      sys.error("Too many leading zeros. Choose a number less than " + maxLength)
    else for {
      coeffs <- listOfN(maxLength - leadingZeros, genN)
    } yield (Vector.fill(leadingZeros)(implicitly[Integral[N]].zero) ++ coeffs.to[Vector])
  
  def genSmallDoubleTDif(leadingZeros: Int = 0): Gen[TDif[Double]] = 
    genBoundedDoubleTDif(smallValue)
  def genSmallIntervalTDif(leadingZeros: Int = 0): Gen[TDif[Interval]] = 
    genBoundedIntervalTDif(-smallValue,smallValue)
  def genSmallThinIntervalTDif(leadingZeros: Int = 0): Gen[TDif[Interval]] = 
    genBoundedThinIntervalTDif(-smallValue,smallValue)

  def genBoundedDoubleTDif(magnitude: Double, leadingZeros: Int = 0): Gen[TDif[Double]] = {
    val abs = Math.abs(magnitude)       
    genTDif(choose(-abs, abs))
  }
  def genBoundedIntervalTDif(lo: Double, hi: Double, leadingZeros: Int = 0): Gen[TDif[Interval]] =
    genTDif(for {
      a <- choose(lo, hi)
      b <- choose(lo, hi)
    } yield Interval(Math.min(a,b), Math.max(a,b)))
  def genBoundedThinIntervalTDif(lo: Double, hi: Double, leadingZeros: Int = 0): Gen[TDif[Interval]] =
    genTDif(choose(lo, hi) map (b => Interval(b,b)), leadingZeros)
    
  implicit def arbitraryIntTDif: Arbitrary[TDif[Int]] =
    Arbitrary(genTDif(arbitrary[Int]))
  implicit def arbitraryDoubleTDif: Arbitrary[TDif[Double]] =
    Arbitrary(genTDif(arbDouble.arbitrary))
  implicit def arbitraryIntervalTDif: Arbitrary[TDif[Interval]] =
    Arbitrary(genTDif(arbitrary[Interval]))

  /* Requirement-specific generators */

  /** Generate TDif[Interval] with a number of leading zeros that satisfies 
   *  even-power requirement of e.g. the sqrt function. */
  def genBoundedThinIntervalTDifEvenLeadingZeros: Gen[TDif[Interval]] =
    for {
      halfLeadingZeros <- choose(0, maxLength / 2)
      tdif <- genBoundedThinIntervalTDif(0, 10, 2 * halfLeadingZeros)
    } yield tdif
    
}
