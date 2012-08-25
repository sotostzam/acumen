package acumen.interpreters.enclosure

import org.scalacheck.Arbitrary._
import org.scalacheck.Gen._
import org.scalacheck.Properties
import org.scalacheck.Prop._
import scala.collection.immutable.Set

import Expression._
import Generators._
import Interval._
import Types._

object ExpressionTest extends Properties("Expression") {

  import TestingContext._

  /* Properties */

  property("variables correspond to identity functions / projections on normalized boxes") =
    forAllNoShrink(genDimBox(1)) { dom =>
      val name = dom.keySet.toList(0)
      val ndom = Box.normalize(dom)
      val proj = AffineScalarEnclosure.apply(ndom, name)
      forAllNoShrink(genThinSubBox(ndom)) { subnDom =>
        proj(subnDom) == subnDom(name)
      }
    }

}