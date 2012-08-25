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

  property("monotonicity of expression evaluation using intervals") = {
    forAllNoShrink(genExpression) { e =>
      forAllNoShrink(genNamesBox(e.varNames)) { box =>
        forAllNoShrink(genThinSubBox(box)) { subbox =>
          e(box) contains e(subbox)
        }
      }
    }
  }

}
