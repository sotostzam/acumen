package acumen.interpreters.enclosure

import org.scalacheck.Gen._
import org.scalacheck.Properties
import org.scalacheck.Prop._

import Box._
import Generators._
import Interval._
import Types._

object AffineScalarEnclosureTest extends Properties("AffineScalarEnclosure") {

  import TestingContext._

  /* Generator tests */

  /* Properties */

  property("constant AffineScalarEnclosure has airty 0") =
    forAll(genBox, genInterval) { (box, interval) =>
      AffineScalarEnclosure(box, interval).arity == 0
    }

  property("projection AffineScalarEnclosure has arity 1") =
    forAll(genBox) { box =>
      box.size > 0 ==>
        forAll(choose(0, box.size - 1)) { index =>
          AffineScalarEnclosure(box, box.keys.toList(index)).arity == 1
        }
    }

  property("monotonicity of enclosure evaluation") =
    forAll(choose(1, 10)) { dim =>
      forAll(genDimBox(dim)) { dom =>
        forAll(genSubBox(dom), genBoxAffineScalarEnclosure(dom)) { (box, f) =>
          forAll(genSubBox(box)) { subbox =>
            f(box) contains f(subbox)
          }
        }
      }
    }

  property("sanity test of enclosure evaluation") =
    {
      val dom = Box("t" -> Interval(0, 1))
      val t = AffineScalarEnclosure(dom, "t")
      t(dom) == dom("t")
    }

  /* Arithmetic operations */

  property("monotonicity of binary addition") =
    forAll { (a: AffineScalarEnclosure, b: AffineScalarEnclosure) =>
      forAll () {
        val sum = a + b
        (sum contains a) && (sum contains b)
      }
    }
  
}
