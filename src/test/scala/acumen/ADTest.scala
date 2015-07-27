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
import AD._

object ADTest extends Properties("AD") {

  /* Utilities */
  
  val evDoubleDifIsIntegral = implicitly[Integral[Dif[Double]]]
  val evDoubleDifIsReal = implicitly[Real[Dif[Double]]]
      
  val evIntervalDifIsIntegral = implicitly[Integral[Dif[Interval]]]
  val evIntervalDifIsReal = implicitly[Real[Dif[Interval]]]
              
  def fromIntD(i: Int) = evDoubleDifIsIntegral.fromInt(i)
  val zeroD = evDoubleDifIsIntegral.zero
  val oneD = evDoubleDifIsIntegral.one
  val piD = evDoubleDifIsReal.fromDouble(Math.PI)
  
  def fromIntI(i: Int) = evIntervalDifIsIntegral.fromInt(i)
  val zeroI = evIntervalDifIsIntegral.zero
  val oneI = evIntervalDifIsIntegral.one
  val piI = evIntervalDifIsReal.fromDouble(Math.PI)
  
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
  
  implicit class DifIsSimilar[T : Similar](l: Dif[T]) {
    /** Compare two Difs coefficient-wise using comp, ignoring first n coefficients */
    def similar(r: Dif[T], comp: (T,T) => Boolean, msg: String, n: Int): Boolean =
      (l.coeff zip r.coeff).zipWithIndex.forall {
        case ((lc, rc), i) =>
          if (i < n || comp(lc, rc)) true
          else sys.error(s"\n\nFound $msg at index $i:\n\n" +
              s"Left coefficient:\n$lc\n\nRight coefficient:\n$rc\n\n" +
              s"Full left Dif:\n$l\n\nFull right Dif:\n$r\n\n")
    }
    def ~=(r: Dif[T]): Boolean = similar(r, (lc:T,rc:T) => lc ~= rc, "dissimilarity", 0)
    /** Ignore first n coefficients */
    def ~=(n: Int)(r: Dif[T]): Boolean = similar(r, (lc:T,rc:T) => lc ~= rc, "dissimilarity", n)
    def in(r: Dif[T]): Boolean = similar(r, (lc:T,rc:T) => lc in rc, "non-inclusion", 0)
  }
  
  /* Properties of Dif[Double] as Integral */

  property("Dif[Double].add: x+x ~= 2*x") = 
    forAll(genSmallDoubleDif) { (x: Dif[Double]) =>
      (x + x) ~= (fromIntD(2) * x)
    }
  
  property("Dif[Double].add: 1+x > x") = 
    forAll(genSmallDoubleDif) { (x: Dif[Double]) =>
      (oneD + x) > x
    }
  
  property("Dif[Double].mul: x*x >= x") = 
    forAll { (x: Dif[Double]) =>
      (x * x) >= x  
    }  

  property("Dif[Double].mul: x*1 ~= x && 1*x ~= x") = 
    forAll { (x: Dif[Double]) =>
      ((x * oneD) ~= x) && ((oneD * x) ~= x) 
    }  
  
  property("Dif[Double].div: x/1 ~= x") = 
    forAll { (x: Dif[Double]) =>
      (x / oneD) ~= x
    }

  property("Dif[Double].div: x/x ~= 1") = 
    forAll { (x: Dif[Double]) =>
      (x(0) != 0d) ==> ((x / x) ~= oneD)  
    }
  
  /* Properties of Dif[Double] as Real */
  
  property("Dif[Double]: sin(x) ~= cos(pi/2 - x)") = 
    forAll(genSmallDoubleDif) { (x: Dif[Double]) =>
      x.sin ~= ((piD / fromIntD(2)) - x).cos
    }

  property("Dif[Double]: cos(x) ~= sin(pi/2 - x)") = 
    forAll(genSmallDoubleDif) { (x: Dif[Double]) =>
      x.cos ~= ((piD / fromIntD(2)) - x).sin
    }

  property("Dif[Double]: x != 0 => tan(x) ~= sin(x) / cos(x)") = 
    forAll(genSmallDoubleDif) { (x: Dif[Double]) =>
      (x != zeroD) ==> (x.tan ~= x.sin / x.cos)
    }

  property("Dif[Double]: tan(x) ~= tan(x + pi)") = 
    forAll(genSmallDoubleDif) { (x: Dif[Double]) =>
      x.tan ~= (x + piD).tan
    }
  
  property("Dif[Double]: tan(atan(x)) ~= x") = 
    forAll(genSmallDoubleDif) { (x: Dif[Double]) =>
      x.atan.tan ~= x
    }

  property("Dif[Double]: exp(x) > 0") =
    // Generate x such that exp(x) is representable as a Double
    forAll(genBoundedDoubleDif(Math.log(Double.MaxValue))) { (x: Dif[Double]) =>
      x.exp > zeroD
    }
  
  property("Dif[Double]: for small x, log(exp(x)) ~= x") = 
    forAll(genSmallDoubleDif) { (x: Dif[Double]) =>
      x.exp.log ~= x
    }

  property("Dif[Double]: x > 1 => sqrt(x) < x") =
    forAll(genSmallDoubleDif) { (x: Dif[Double]) =>
      (x > oneD) ==> x.sqrt < x
    }
  
  property("Dif[Double]: x >= 1 => square(x) >= x") =
    forAll(genSmallDoubleDif) { (x: Dif[Double]) =>
      (x >= oneD) ==> x.square >= x
    }
  
  property("Dif[Double]: square(x) ~= x*x") =
    forAll(genSmallDoubleDif) { (x: Dif[Double]) =>
      x.square ~= x*x
    }
  
  property("Dif[Double]: pow(0) ~= 1") = 
    forAll(genSmallDoubleDif) { (x: Dif[Double]) =>
      (x ^ zeroD) ~= oneD
    }
  
  property("Dif[Double]: 0 < x < 1 && y > 1 => x^y < x") = 
    forAll(genRealDif(choose(0.01,0.99)), genRealDif(choose(1.0,10))) { 
      (x: Dif[Double], y: Dif[Double]) =>
        (x ^ y) < x
    }
  
  /* Properties of Dif[Interval] as Real */
  
  property("Dif[Interval]: -1 <= x <= 1 => x in cos(acos(x))") = 
    forAll(genBoundedThinIntervalDif(-1,1)) { (x: Dif[Interval]) =>
      x in x.acos.cos
    }

  property("Dif[Interval]: -1 <= x <= 1 => x in sin(asin(x))") = 
    forAll(genBoundedThinIntervalDif(-1,1)) { (x: Dif[Interval]) =>
      x in x.asin.sin
    }
      
  property("Dif[Interval]: x >= 0 => x in sqrt(x*x)") = 
    forAll(genBoundedThinIntervalDif(0,10)) { (x: Dif[Interval]) =>
      x in (x*x).sqrt
    }

  property("Dif[Interval]: x >= 0 => x in square(sqrt(x))") =
    forAll(genBoundedThinIntervalDif(0,10)) { (x: Dif[Interval]) =>
      x in x.sqrt.square
    }
  
  property("Dif[Interval]: x >= 0 => x in (sqrt(x))^2") =
    forAll(genBoundedThinIntervalDif(0,10)) { (x: Dif[Interval]) =>
      x in x.sqrt^2
    }

  property("Dif[Interval]: x >= 0 => x in sqrt(x^2)") =
    forAll(genBoundedThinIntervalDif(0,10)) { (x: Dif[Interval]) =>
      x in (x^2).sqrt
    }
  
  property("Dif[Interval]: 1 in x^0") =
    forAll(genSmallThinIntervalDif) { (x: Dif[Interval]) =>
      IntervalDifIsReal.fromInt(1) in x^0 
    }
  
  property("Dif[Interval]: x in x^1") =
    forAll(genSmallThinIntervalDif) { (x: Dif[Interval]) =>
      x in x^1
    }
  
  property("Dif[Interval]: x in (x^3 / x^2)") =
    forAll(genSmallThinIntervalDif) { (x: Dif[Interval]) =>
      x in ((x^3) / (x^2))
    }

  property("Dif[Interval]: n > 0 => 1^n = 1") =
    forAll (posNum[Int]) { (n: Int) =>
      (IntervalDifIsReal.one ^ n) == IntervalDifIsReal.one 
    }

  property("Dif[Interval]: x^0 = 1") =
    forAll(genSmallThinIntervalDif) { (x: Dif[Interval]) =>
      (x^0) == IntervalDifIsReal.one
    }
  
  property("Dif[Interval]: x > 1 && n < 0 => x^n <= x") =
    forAll(genBoundedThinIntervalDif(1,10), negNum[Int]) { (x: Dif[Interval], n: Int) =>
      (x^n) <= x
    }
  
  /* Other properties */

  property("lower is inverse of lift") = forAll { (st: CStore) =>
    lower(lift(st)) == st
  }

  /* Generators */
  
  def genIntegralDif[N: Integral](genN: Gen[N]): Gen[Dif[N]] =
    for {
      coeffs <- listOfN(10, genN)
    } yield Dif(coeffs.to[Vector])

  def genRealDif[N: Real](genN: Gen[N]): Gen[Dif[N]] =
    for {
      coeffs <- listOfN(10, genN)
    } yield Dif(coeffs.to[Vector])
  
  def genSmallDoubleDif: Gen[Dif[Double]] = genBoundedDoubleDif(10)
  def genSmallIntervalDif: Gen[Dif[Interval]] = genBoundedIntervalDif(-10,10)
  def genSmallThinIntervalDif: Gen[Dif[Interval]] = genBoundedThinIntervalDif(-10,10)

  def genBoundedDoubleDif(magnitude: Double): Gen[Dif[Double]] = {
    val abs = Math.abs(magnitude)       
    genRealDif(choose(-abs, abs))
  }
  def genBoundedIntervalDif(lo: Double, hi: Double): Gen[Dif[Interval]] =
    genRealDif(for {
      a <- choose(lo, hi)
      b <- choose(lo, hi)
    } yield Interval(Math.min(a,b), Math.max(a,b)))
  def genBoundedThinIntervalDif(lo: Double, hi: Double): Gen[Dif[Interval]] =
    genRealDif(choose(lo, hi) map (b => Interval(b,b)))

    
  implicit def arbitraryIntDif: Arbitrary[Dif[Int]] =
    Arbitrary(genIntegralDif(arbitrary[Int]))
  implicit def arbitraryDoubleDif: Arbitrary[Dif[Double]] =
    Arbitrary(genRealDif(arbDouble.arbitrary))
  implicit def arbitraryIntervalDif: Arbitrary[Dif[Interval]] =
    Arbitrary(genRealDif(arbitrary[Interval]))
  
}
