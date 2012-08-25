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

  /* Generator tests */

  /* Properties */

  //  property("variables correspond to identity functions / projections on normalized boxes") =
  //    false
  //
  //  property("eval is lifting's inverse on constants") =
  //    false

  //  property("monotonicity (w.r.t subdomains) of evaluation") =
  //    forAllNoShrink(arbitrary[Expression]) { e =>
  //      val names = e.varNames
  //      names.nonEmpty ==>
  //        forAllNoShrink(genNamesBox(names)) { box =>
  //          forAllNoShrink(genSubBox(box)) { subBox =>
  //            e(box) contains e(subBox)
  //          }
  //        }
  //    }

  //  property("monotonicity (w.r.t subdomains) of evaluation - unit test 2") = {
  //    val e = Variable("w") * Interval(-1,7)
  //    val box = Box("w" -> Interval(-1,6))
  //    val subBox = Box("w" -> Interval(3,4))
  //    println("e(box)" + e(box))
  //    println("e(subBox)" + e(subBox))
  //    e(box) contains e(subBox)
  //  }

  property("monotonicity (w.r.t subdomains) of evaluation - unit test 3 (mini)") = {
    val e = Variable("w") * Interval(0, 1)
    val box = Box("w" -> Interval(-1, 0))
    println("e(box)" + e(box))
    Interval(0,3) contains e(box)
  }

  //  property("monotonicity (w.r.t subdomains) of evaluation - unit test") = {
  //    val e = (Variable("Z") * "k") * "q"
  //    val box = Box("Z" -> Interval(-8.969294966E+307, 0), "k" -> 1, "q" -> Interval(0, 8.763940222E+307))
  //    val subBox = Box("Z" -> Interval(-5.903191316E+307, -3.066103650E+307), "k" -> Interval(1.000000000, 1.000000000), "q" -> Interval(1.278638373E+307, 7.485301849E+307))
  //    e(box) contains e(subBox)
  //  }

  //  property("safe approximation of floating point evaluation") =
  //    false

  /* Utilities */

  //  property("varNames unit test") = {
  //    (Variable("x") + 1).varNames == Set("x") &&
  //      (Variable("z") + Variable("x")).varNames == Set("x", "z")
  //  }

}