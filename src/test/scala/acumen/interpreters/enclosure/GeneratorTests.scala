package acumen.interpreters.enclosure

import org.scalacheck.Gen
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck.Properties

import Generators._

object GeneratorTests extends Properties("Generators") {

  import TestingContext._

  /* Interval */

  property("genSubInterval generates a sub-interval") =
    forAll(genInterval) { interval =>
      forAll(genSubInterval(interval)) { subinterval =>
        interval contains subinterval
      }
    }

  property("genThinSubInterval generates thin subintervals") =
    forAll { (dom: Interval) =>
      forAllNoShrink(genThinSubInterval(dom)) { subDom =>
        (dom contains subDom) && (subDom.low == subDom.high)
      }
    }

  /* Box */

  property("genDimBox(n) is n-dimensional") =
    forAll(posNum[Int]) { dim =>
      forAll(genDimBox(dim)) { box =>
        box.size == dim
      }
    }

  /* AffineScalarEnclosure */

  property("genSubAffineScalarEnclosure generates a sub-enclosure") =
    forAllNoShrink(choose(1, 10)) { dim =>
      forAllNoShrink(genDimBox(dim)) { dom =>
        forAllNoShrink(genBoxAffineScalarEnclosure(dom)) { f =>
          forAllNoShrink(genSubAffineScalarEnclosure(f)) { subf =>
            f contains subf
          }
        }
      }
    }

  property("genSubAffineScalarEnclosure generates a point-wise sub-enclosure") =
    forAllNoShrink(choose(1, 10)) { dim =>
      forAllNoShrink(genDimBox(dim)) { dom =>
        forAllNoShrink(genSubBox(dom), genBoxAffineScalarEnclosure(dom)) { (box, f) =>
          forAllNoShrink(genSubAffineScalarEnclosure(f)) { subf =>
            f(box) contains subf(box)
          }
        }
      }
    }

}