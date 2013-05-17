package acumen.interpreters.enclosure.affine

import org.scalacheck.Arbitrary._
import org.scalacheck.Gen
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck.Properties
import AffineEnclosure._
import AffineScalarEnclosure._
import acumen.interpreters.enclosure.Box._
import acumen.interpreters.enclosure.Generators._
import acumen.interpreters.enclosure.Interval._
import acumen.interpreters.enclosure.Types._
import acumen.interpreters.enclosure.Box

object AffineEnclosureTest extends Properties("AffineEnclosure") {

  import acumen.interpreters.enclosure.TestingContext._

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

  property("collapsing reduces domain dimension") =
    forAll(oneOf(2 to 10)) { dim =>
      forAll(genDimAffineEnclosure(dim)) { e =>
        forAll(oneOf(e.domain.keys.toList)) { name =>
          e.domain.keys.size == e.collapse(name).domain.size + 1
        }
      }
    }

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

object AffineEnclosureUnitTest extends Properties("AffineEnclosureUnitTest") {

  import acumen.interpreters.enclosure.TestingContext._

}