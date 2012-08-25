package acumen.interpreters.enclosure

import org.scalacheck.Arbitrary._
import org.scalacheck.Gen
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck.Properties

import Box._
import Generators._
import Interval._
import Types._
import AffineScalarEnclosure._

object AffineScalarEnclosureTest extends Properties("AffineScalarEnclosure") {

  import TestingContext._

  /* Type synonyms */

  type BinaryOp = (AffineScalarEnclosure, AffineScalarEnclosure) => AffineScalarEnclosure

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

  property("an enclsoure is the sum of its constantTerm and linearTerms") =
    forAll { (f: AffineScalarEnclosure) =>
      f.constantTerm + f.linearTerms == f
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
      val dom = Box("t" -> Interval(1, 2))
      val t = AffineScalarEnclosure(dom, "t")
      t(dom) == dom("t")
    }

  property("sanity test of enclosure range") =
    {
      val dom = Box("t" -> Interval(1, 2))
      val t = AffineScalarEnclosure(dom, "t")
      t.range == dom("t")
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
    //TODO Add testing of division and operation variants (intervals, scalars)
    forAll(choose(1, 10),
      oneOf(Seq((_ + _), (_ - _), (_ * _))): Gen[BinaryOp]) { (dim, bop) =>
        forAllNoShrink(genDimBox(dim)) { dom =>
          forAllNoShrink(
            genBoxAffineScalarEnclosure(dom),
            genBoxAffineScalarEnclosure(dom)) { (x, y) =>
              forAllNoShrink(
                genSubAffineScalarEnclosure(x),
                genSubAffineScalarEnclosure(y)) { (subx, suby) =>
                  bop(x, y) contains bop(subx, suby)
                }
            }
        }
      }

  property("monotonicity of range w.r.t. restriction") =
    forAllNoShrink(choose[Int](1, 10)) { dim =>
      forAllNoShrink(genDimBox(dim)) { dom =>
        forAllNoShrink(
          genSubBox(dom),
          genBoxAffineScalarEnclosure(dom)) { (subDom, e) =>
            e.range contains e.restrictTo(subDom).range
          }
      }
    }

  property("upper bound monotonicity of approximations of quadratics on normalized domains") =
    forAllNoShrink(choose[Int](1, 10)) { dim =>
      forAllNoShrink(genDimBox(dim)) { dom =>
        forAllNoShrink(
          genSubBox(dom),
          oneOf(dom.keys.toList)) { (subDom, name) =>
            quadratic(dom, name).restrictTo(subDom) contains quadratic(subDom, name).high
          }
      }
    }

  property("affine enclosure of mixed terms") =
    false // TODO Implement property

  property("affine enclosure of primitive function") =
    false // TODO Implement property

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
