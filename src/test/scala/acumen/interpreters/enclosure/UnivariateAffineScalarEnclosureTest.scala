package acumen.interpreters.enclosure

import org.scalacheck.Arbitrary._
import org.scalacheck.Gen
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck.Properties

import UnivariateAffineScalarEnclosure._
import Box._
import Generators._
import Interval._
import Types._

object UnivariateAffineScalarEnclosureTest extends Properties("UnivariateAffineScalarEnclosure") {

  import TestingContext._

  // TODO add properties, e.g. that f union g conatins f and g.

  property("union") =
    forAll { (dom: Interval) =>
      forAll(
        genDomUnivariateAffineScalarEnclosure(dom),
        genDomUnivariateAffineScalarEnclosure(dom)) { (f, g) =>
          val u = f union g
          println("f: " + f)
          println("g: " + g)
          println("u: " + u)
          (u contains f) && (u contains g)
        }
    }

//  property("union") = {
//    val dom = Interval(-8.988465675E+307, 4.708293845E+307)
//    val ndom = Interval(0, 1.369675952E+308)
//    val f = UnivariateAffineScalarEnclosure(
//      dom,
//      ndom,
//      Interval(-6.587391532E+307, 8.988465675E+307),
//      Interval(0, 1.352122010E+307))
//    val g = UnivariateAffineScalarEnclosure(
//      dom,
//      ndom,
//      Interval(-8.721856061E+307, -5.011888087E+307),
//      Interval(5.698642743E+306, 8.988465675E+307))
//    val u = f union g
//    println("f: " + f)
//    println("g: " + g)
//    println("u: " + u)
//    (u contains f) && (u contains g)
//  }

}