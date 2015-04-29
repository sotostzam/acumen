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
  
  val evDoubleDifIsNum = implicitly[Num[Dif[Double]]]
  
  def liftInt(i: Int) = evDoubleDifIsNum.lift(i)
  val one = evDoubleDifIsNum.one
  
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
  
  /* Properties */

  property("Dif[Double].add: x+x ~= 2*x") = forAll(genSmallDoubleDif) { (x: Dif[Double]) =>
    (x + x) ~= (liftInt(2) * x)
  }
  
  property("Dif[Double].add: 1+x > x") = forAll(genSmallDoubleDif) { (x: Dif[Double]) =>
    (one + x) > x
  }
  
  property("Dif[Double].mul: x*x >= x") = forAll { (x: Dif[Double]) =>
    (x * x) >= x  
  }  

  property("Dif[Double].mul: x*1 ~= x && 1*x ~= x") = forAll { (x: Dif[Double]) =>
    ((x * one) ~= x) && ((one * x) ~= x) 
  }  
  
  property("Dif[Double].div: x/1 ~= x") = forAll { (x: Dif[Double]) =>
    (x / one) ~= x
  }

  property("Dif[Double].div: x/x ~= 1") = forAll { (x: Dif[Double]) =>
    (x(0) != 0d) ==> ((x / x) ~= one)  
  }
  
  property("Dif[Double]: tan(x) ~= tan(x+pi/2)") = forAll(genSmallDoubleDif) { (x: Dif[Double]) =>
    x.tan ~= (x + evDoubleDifIsNum.lift(Math.PI)).tan
  }

  property("lower is inverse of lift") = forAll { (st: CStore) =>
    lower(lift(st)) == st
  }
  
  /* Generators */
  
  def genDif[N: Num](genN: Gen[N]): Gen[Dif[N]] =
    for {
      coeffs <- listOfN(10, genN)
    } yield Dif(coeffs.to[Vector])
  
  def genSmallDoubleDif: Gen[Dif[Double]] = genBoundedDoubleDif(1000000)
    
  def genBoundedDoubleDif(magnitude: Double): Gen[Dif[Double]] = {
    val abs = Math.abs(magnitude)       
    genDif(choose(-abs, abs))
  }
    
  implicit def arbitraryIntDif: Arbitrary[Dif[Int]] =
    Arbitrary(genDif(arbitrary[Int]))
  implicit def arbitraryDoubleDif: Arbitrary[Dif[Double]] =
    Arbitrary(genDif(arbDouble.arbitrary))
  implicit def arbitraryIntervalDif: Arbitrary[Dif[Interval]] =
    Arbitrary(genDif(arbitrary[Interval]))
  
}
