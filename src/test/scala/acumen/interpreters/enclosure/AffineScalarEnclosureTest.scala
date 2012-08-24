package acumen.interpreters.enclosure

import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck.Properties

import Box._
import Generators._
import Interval._
import Types._

object AffineScalarEnclosureTest extends Properties("AffineScalarEnclosure") {

  import TestingContext._

  /* Generator tests */

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

  /* Properties */

  property("constant AffineScalarEnclosure has airty 0") =
    forAll(genBox, genInterval) { (box, interval) =>
      AffineScalarEnclosure(box, interval).arity == 0
    }

  property("projection AffineScalarEnclosure has arity 1") =
    forAll(posNum[Int]) { dim =>
      forAll(genDimBox(dim)) { box =>
        forAll(choose(0, box.size - 1)) { index =>
          AffineScalarEnclosure(box, box.keys.toList(index)).arity == 1
        }
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

  property("range contains values at points") =
    forAll(choose(1, 10)) { dim =>
      forAll(genDimBox(dim)) { dom =>
        forAll(genThinSubBox(dom), genBoxAffineScalarEnclosure(dom)) { (x, f) =>
          f.range contains f(x)
        }
      }
    }

  property("sanity test for enclosure containment") =
    {
      val dom = Box("t" -> Interval(0, 1))
      val t = AffineScalarEnclosure(dom, "t")
      t contains t
    }

  property("enclosure containment implies point-wise containment") =
    forAllNoShrink(choose(1, 10)) { dim =>
      forAllNoShrink(genDimBox(dim)) { dom =>
        forAllNoShrink(genSubBox(dom), genBoxAffineScalarEnclosure(dom)) { (box, f) =>
          forAllNoShrink(genSubAffineScalarEnclosure(f)) { subf =>
            (f contains subf) ==> (f(box) contains subf(box))
          }
        }
      }
    }

  property("numeric operations monotonicity") =
    false

  property("affine enclosure of quadratic terms") =
    false

  property("affine enclosure of mixed terms") =
    false

  property("affine enclosure of primitive function") =
    false

  property("collapsing removes the collapsed variable") =
    forAll(choose(1, 10)) { dim =>
      forAll(genDimBox(dim)) { dom =>
        forAll(genBoxAffineScalarEnclosure(dom), oneOf(dom.keys.toSeq)) { (f, name) =>
          val collapsed = f.collapse(name)
          !collapsed.domain.contains(name) && !collapsed.coefficients.contains(name)
        }
      }
    }

  property("safety of enclosure collapsing") =
    forAll(choose(1, 10)) { dim =>
      forAll(genDimBox(dim)) { dom =>
        forAll(genBoxAffineScalarEnclosure(dom), oneOf(dom.keys.toSeq)) { (f, name) =>
          val fnoname = f.collapse(name)
          val collapsed = AffineScalarEnclosure(f.domain, f.normalizedDomain, fnoname.constant, fnoname.coefficients)
          collapsed contains f
        }
      }
    }

}
