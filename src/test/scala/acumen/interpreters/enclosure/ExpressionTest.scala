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

  // Note: This property relies on the \/ operator throwing an exception when its result is empty. 
  property("enclosure/interval affine expression evaluation consistency") =
    forAllNoShrink(genAffineExpression) { e =>
      forAllNoShrink(genNamesBox(e.varNames)) { box =>
        try {
          e.enclosureEval(box) \/ e(box)
          true
        } catch {
          case e => false
        }
      }
    }

}

object ExpressionUnitTest extends Properties("Expression.UnitTest") {

  import TestingContext._

  property("enclosure/interval expression evaluation consistency") = {
    val l: Expression = "l"
    val e = l * l
    val box = Box("l" -> Interval(4, 5))
    try {
//      println("e.enclosureEval(box)" + e.enclosureEval(box))
//      println("e(box)" + e(box))
      e.enclosureEval(box) \/ e(box)
      true
    } catch {
      case e => false
    }
  }

  property("enclosure expression evaluation failing test case") = {
    // Should result in [15.75,25]
    val l: Expression = "l"
    val e = l * l
    val box = Box("l" -> Interval(4, 5))
//    println("e.enclosureEval(box)" + e.enclosureEval(box))
    Interval(15.75, 25) contains e.enclosureEval(box)
  }

}
