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

  property("union") =
    forAllNoShrink(genInterval) { dom =>
      forAllNoShrink(
        genBoxUnivariateAffineScalarEnclosure(dom),
        genBoxUnivariateAffineScalarEnclosure(dom)) { (f, g) =>
          val u = f union g
          (u contains f) && (u contains g)
        }
    }

  property("Building a UASE from an ASE preserves the constant and coefficient") =
    forAllNoShrink(genDimBox(1)) { box =>
      forAllNoShrink(genBoxAffineScalarEnclosure(box)) { ase =>
        val uase = UnivariateAffineScalarEnclosure(ase)
        val varName = box.keySet.toList(0)
        (uase.constant == ase.constant) && (uase.coefficient == ase.coefficients(varName))
      }
    }

  property("ASE/UASE point-wise evaluation consistency") =
    forAllNoShrink(genDimBox(1)) { box =>
      forAllNoShrink(
        genSubBox(box),
        genBoxAffineScalarEnclosure(box)) { (subBox, ase) =>
          val uase = UnivariateAffineScalarEnclosure(ase)
          val subBoxAsInterval = subBox(subBox.keySet.toList(0))
          ase(subBox) == uase(subBoxAsInterval)
        }
    }

}

object UnivariateAffineScalarEnclosureUnitTest extends Properties("UnivariateAffineScalarEnclosureUnitTest") {

  import TestingContext._

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
  //
  //  property("union 2") = {
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
