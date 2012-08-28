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

  property("union monotnonicity") =
    forAllNoShrink(genInterval) { dom =>
      forAllNoShrink(
        genBoxUnivariateAffineScalarEnclosure(dom),
        genBoxUnivariateAffineScalarEnclosure(dom)) { (f, g) =>
          val u = f union g
          (u contains f) && (u contains g)
        }
    }

  property("building a UASE from an ASE preserves the constant and coefficient") =
    forAllNoShrink(genDimBox(1)) { box =>
      forAllNoShrink(genBoxAffineScalarEnclosure(box)) { ase =>
        val uase = UnivariateAffineScalarEnclosure(ase)
        val varName = box.keys.head
        (uase.constant == ase.constant) && (uase.coefficient == ase.coefficients(varName))
      }
    }

  /* Delegate to the tests of AffineScalarEnclosure. */
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

  property("union monotonicity - failing test case") = {
    val dom = Interval(3.590613572, 5.224065866)
    val f = UnivariateAffineScalarEnclosure(
      Interval(3.590613572, 5.224065866),
      Interval(0, 1.633452294),
      Interval(-5.581185179, -2.043696493),
      Interval(-3.473291402, 3.714148680))
    val g = UnivariateAffineScalarEnclosure(
      Interval(3.590613572, 5.224065866),
      Interval(0, 1.633452294),
      Interval(-7.174274390, 3.882178544),
      Interval(-5.706812312, -1.248947209))
    val u = f union g
    println("f: " + f)
    println("g: " + g)
    println("u: " + u)
    (u contains f) && (u contains g)
  }

}
