package acumen.interpreters.enclosure

import org.scalacheck.Properties
import org.scalacheck.Prop._
import org.scalacheck.Gen._

import Types._

import Generators._

object BoxTest extends Properties("Box") {

  import TestingContext._

  /* Properties */

  property("collapsing") = {
    val dom = Box("x" -> Interval(0, 1), "y" -> Interval(0, 1))
    val ndom = Box.normalize(dom)
    val ase = AffineScalarEnclosure(dom, ndom, Interval(0), Box("x" -> Interval(1)))
    val ae = AffineEnclosure(dom, ndom, Map("a" -> ase))
    val domnox = dom - "x"
    val ndomnox = ndom - "x"
    val asenox = ase.collapse("x")
    val aenox = AffineEnclosure(domnox, ndomnox, Map("a" -> asenox))
    ae.collapse("x") == aenox
  }

//  property("box normalization") =
//    forAll(genBox) { box =>
//      Box.normalize(box).forall {
//        case (_, i) =>
//          i greaterThanOrEqualTo Interval(0)
//      }
//    }
//
//  property("monotonicity of box normalization") =
//    forAll(genBox) { box =>
//      forAll(genSubBox(box)) { subbox =>
//        val nbox = Box.normalize(box)
//        val nsubbox = Box.normalize(subbox)
//        box.keys.forall { name =>
//          nbox(name) contains nsubbox(name)
//        }
//      }
//    }
//
//  property("box corners consist of box edge endpoints") =
//    forAll(choose[Int](1, 10)) { dim =>
//      forAll(genDimBox(dim)) { (b: Box) =>
//        Box.corners(b).forall { c =>
//          c.forall {
//            case (name, value) =>
//              value == b(name).low || value == b(name).high
//          }
//        }
//      }
//    }
//
//  property("there are 2^n corners of a n-dimensional box.") =
//    forAll(choose[Int](1, 10)) { dim =>
//      forAll(genDimBox(dim)) { box =>
//        Box.corners(box).size == scala.math.pow(2, dim)
//      }
//    }

}