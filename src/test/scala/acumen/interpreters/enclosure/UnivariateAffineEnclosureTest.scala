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

object UnivariateAffineEnclosureTest extends Properties("UnivariateAffineEnclosure") {

  import TestingContext._

  property("union soundness") =
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
