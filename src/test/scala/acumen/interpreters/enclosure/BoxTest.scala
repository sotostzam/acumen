package acumen.interpreters.enclosure

import org.scalacheck.Properties
import org.scalacheck.Prop._
import org.scalacheck.Gen._
import Types._
import Generators._
import acumen.interpreters.enclosure.affine.AffineScalarEnclosure
import acumen.interpreters.enclosure.affine.AffineEnclosure

object BoxTest extends Properties("Box") {

  import TestingContext._

  /* Properties */

  // AffineEnclosure before and after collapsing "x" 
  val (ae, aenox) = {
    val dom = Box("x" -> Interval(0, 1), "y" -> Interval(0, 1))
    val ndom = Box.normalize(dom)
    val ase = AffineScalarEnclosure(dom, ndom, Interval(0), Box("x" -> Interval(1)))
    val ae = AffineEnclosure(dom, ndom, Map("a" -> ase))
    val domnox = dom - "x"
    val ndomnox = ndom - "x"
    val asenox = ase.collapse("x")
    val aenox = AffineEnclosure(domnox, ndomnox, Map("a" -> asenox))
    (ae, aenox)
  }

  property("collapsing") = {
    ae.collapse("x") == aenox
  } 
  
  property("fast collapsing") = {
    ae.collapseFast("x" :: Nil) == aenox
  }

  property("box normalization") =
    forAll(genBox) { box =>
      Box.normalize(box).forall {
        case (_, i) =>
          i greaterThanOrEqualTo Interval(0)
      }
    }

  property("monotonicity of box normalization") =
    forAll(genBox) { box =>
      forAll(genSubBox(box)) { subbox =>
        val nbox = Box.normalize(box)
        val nsubbox = Box.normalize(subbox)
        box.keys.forall { name =>
          nbox(name) contains nsubbox(name)
        }
      }
    }

  property("box corners consist of box edge endpoints") =
    forAll(choose[Int](1, 10)) { dim =>
      forAll(genDimBox(dim)) { (b: Box) =>
        Box.corners(b).forall { c =>
          c.forall {
            case (name, value) =>
              value == b(name).low || value == b(name).high
          }
        }
      }
    }

  property("there are 2^n corners of a n-dimensional box.") =
    forAll(choose[Int](1, 10)) { dim =>
      forAll(genDimBox(dim)) { box =>
        Box.corners(box).size == scala.math.pow(2, dim)
      }
    }

  property("removing a variable from a box") =
    forAllNoShrink(choose(1, 10)) { dim =>
      forAllNoShrink(genDimBox(dim)) { box =>
        forAllNoShrink(oneOf(box.keySet.toList)) { varName =>
          (box contains varName) &&
            !((box - varName) contains varName)
        }
      }
    }

  property("contains soundness") =
    forAll { box: Box =>
      forAll(genSubBox(box)) { sbox =>
        box contains sbox
      }
    }

  property("hull is isotonic") =
    forAll { b1: Box =>
      forAll(genNamesBox(b1.keySet)) { b2 =>
        val h = b1 hull b2
        (h contains b1) && (h contains b2)
      }
    }

  property("\\ is isotonic in left argument") =
    forAll(genBox) { box =>
      forAll(genSubBox(box)) { sbox =>
        !(box equalTo sbox) ==> (box contains (box \ sbox))
      }
    }

  property("\\ is antitonic in right argument") =
    forAll(genBox) { box =>
      forAll(genSubBox(box)) { sbox =>
        forAll(genSubBox(sbox)) { ssbox =>
          !((box equalTo sbox) || (box equalTo ssbox)) ==>
            ((box \ ssbox) contains (box \ sbox))
        }
      }
    }

  //  property("intersection of disjoint boxes is empty") =
  //    forAll(genBox, genBox) { (b1, b2) =>
  //      (b1 disjointFrom b2) ==> ((b1 /\ b2) isEmpty)
  //    }

  //  TODO: implement porperty!
  //  property("intersection of non-disjoint boxes is non-empty") =
  //    forAll(genBox, genBox) { (b1, b2) =>
  //      false
  //    }

}
