package acumen.interpreters.enclosure

import org.scalacheck.Arbitrary._
import org.scalacheck.Gen
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck.Properties

import AffineEnclosure._
import AffineScalarEnclosure._
import Box._
import Generators._
import Interval._
import Types._

object AffineEnclosureTest extends Properties("AffineEnclosure") {

  import TestingContext._

  /* Type synonyms */

  type BinaryAEOp = (AffineEnclosure, AffineEnclosure) => AffineEnclosure

  /* Properties */

  property("constant AffineEnclosure has components with arity 0") =
    forAll { (domain: Box, constant: Box) =>
      val components = AffineEnclosure(domain, constant).components
      components.keys.forall { components(_).arity == 0 }
    }

  property("projection AffineEnclosure has components with arity 1") =
    forAll(posNum[Int]) { dim =>
      forAll(genDimBox(dim)) { dom =>
        forAll(listOfN(dim, oneOf(dom.keySet.toList))) { names =>
          val components = AffineEnclosure(dom, names: _*).components
          components.keys.forall { components(_).arity == 1 }
        }
      }
    }

  //  property("projections act as identity functions on normalized boxes") =
  //    forAll { (dom: Box) =>
  //      val names = dom.keySet.toList
  //      val ndom = Box.normalize(dom)
  //      val e = AffineEnclosure(ndom, names.map { n => (n, AffineScalarEnclosure.apply(ndom, n)) }.toMap)
  //      forAllNoShrink(genThinSubBox(ndom)) { subnDom =>
  //        e(subnDom) == subnDom(name)
  //      }
  //    }

  //    property("projections safely approximate identity functions") =

  property("an enclosure is the sum of its constantTerm and linearTerms") =
    forAll { (f: AffineEnclosure) =>
      f.constantTerm + f.linearTerms == f
    }

//  property("monotonicity of enclosure evaluation") =
//    //TODO Try this with a larger number of state variables
//    forAll(choose(1, 10), choose(1, 10)) { (stateDim, dim) =>
//      forAll(listOfN(stateDim, genDimBox(dim))) { doms =>
//        forAll(genSubBox(dom), genBoxAffineScalarEnclosure(dom)) { (box, f) =>
//          forAll(genSubBox(box)) { subbox =>
//            f(box) contains f(subbox)
//          }
//        }
//      }
//    }

  //  property("sanity test of enclosure evaluation") =

  //  property("sanity test of enclosure range") =

  //  property("range monotonicity") =

  //  property("sanity test for enclosure containment") =

  //  property("enclosure containment implies point-wise containment") =

  //  property("numeric operations monotonicity") =

  //  property("monotonicity of range w.r.t. restriction") =

  //  property("upper bound monotonicity of approximations of quadratics on normalized domains") =

  //  property("affine enclosures safely approximate mixed terms") =

  //  property("affine enclosure of primitive function") =

  //  property("collapsing removes the collapsed variable") =

  //  property("safety of enclosure collapsing") =

}