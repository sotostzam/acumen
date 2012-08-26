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

}