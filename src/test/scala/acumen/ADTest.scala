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
  
  def fromInt(i: Int) = evDoubleDifIsIntegral.fromInt(i)
  val one = evDoubleDifIsIntegral.one
  val pi = evDoubleDifIsReal.fromDouble(Math.PI)
  
  implicit class DoubleOps(l: Double) {
    def ~=(r: Double): Boolean = 
      Math.abs(l - r) <= Math.abs(Math.min(Math.abs(l), Math.abs(r))) / 1000
  }
  
  implicit class DoubleDifOps(l: Dif[Double]) {
     def ~=(r: Dif[Double]): Boolean =
       (l.coeff zip r.coeff).forall{ case (lc, rc) => 
       if (!(lc ~= rc))
         sys.error("NO: " + (lc,rc) + "\nNO:\n" + l + "\n" + r)
         else
       lc ~= rc }
  }
  
  /* Properties of Integral */

  property("Dif[Double].add: x+x ~= 2*x") = 
    forAll(genSmallDoubleDif) { (x: Dif[Double]) =>
      (x + x) ~= (fromInt(2) * x)
    }
  
  property("Dif[Double].add: 1+x > x") = 
    forAll(genSmallDoubleDif) { (x: Dif[Double]) =>
      (one + x) > x
    }
  
  property("Dif[Double].mul: x*x >= x") = 
    forAll { (x: Dif[Double]) =>
      (x * x) >= x  
    }  

  property("Dif[Double].mul: x*1 ~= x && 1*x ~= x") = 
    forAll { (x: Dif[Double]) =>
      ((x * one) ~= x) && ((one * x) ~= x) 
    }  
  
  property("Dif[Double].div: x/1 ~= x") = 
    forAll { (x: Dif[Double]) =>
      (x / one) ~= x
    }

  property("Dif[Double].div: x/x ~= 1") = 
    forAll { (x: Dif[Double]) =>
      (x(0) != 0d) ==> ((x / x) ~= one)  
    }
  
  /* Properties of Real */
  property("Dif[Double]: tan(x) ~= tan(x + pi)") = 
    forAll(genSmallDoubleDif) { (x: Dif[Double]) =>
      x.tan ~= (x + pi).tan
    }

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
  
  def genSmallDoubleDif: Gen[Dif[Double]] = genBoundedDoubleDif(1000000)
    
  def genBoundedDoubleDif(magnitude: Double): Gen[Dif[Double]] = {
    val abs = Math.abs(magnitude)       
    genRealDif(choose(-abs, abs))
  }
    
  implicit def arbitraryIntDif: Arbitrary[Dif[Int]] =
    Arbitrary(genIntegralDif(arbitrary[Int]))
  implicit def arbitraryDoubleDif: Arbitrary[Dif[Double]] =
    Arbitrary(genRealDif(arbDouble.arbitrary))
  implicit def arbitraryIntervalDif: Arbitrary[Dif[Interval]] =
    Arbitrary(genRealDif(arbitrary[Interval]))
  
}
