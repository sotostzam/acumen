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

  property("Dif[Interval].sin") = forAll { (d: Dif[Interval]) =>
    d.sin(0) == d(0).sin
  }
  
  property("Dif[Interval].cos") = forAll { (d: Dif[Interval]) =>
    d.cos(0) == d(0).cos
  }

  property("lower is inverse of lift") = forAll { (st: CStore) =>
    lower(lift(st)) == st
  }
  
  /* Generators */
  
  def genDif[N: Num](genN: Gen[N]): Gen[Dif[N]] =
    for {
      coeffs <- listOfN(10, genN)
    } yield Dif(coeffs.to[Vector])
  
  implicit def arbitraryIntDif: Arbitrary[Dif[Int]] =
    Arbitrary(genDif(arbitrary[Int]))
  implicit def arbitraryDoubleDif: Arbitrary[Dif[Double]] =
    Arbitrary(genDif(arbDouble.arbitrary))
  implicit def arbitraryIntervalDif: Arbitrary[Dif[Interval]] =
    Arbitrary(genDif(arbitrary[Interval]))
  
}
