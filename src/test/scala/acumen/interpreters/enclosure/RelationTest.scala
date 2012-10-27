package acumen.interpreters.enclosure

import org.scalacheck.Properties

import Relation._

object RelationTest extends Properties("Relation") {

  import TestingContext._

  /* Generator tests */

  /* Properties */

}

object RelationUnitTest extends Properties("Relation.UnitTest") {

  import TestingContext._

  property("support") = {
    val dom = Box("x" -> Interval(0, 1), "y" -> Interval(1, 2))
    val x: Expression = "x"
    val y: Expression = "y"
    dom contains equalToZero(x - y).support(dom)
    dom contains nonNegative(x - y).support(dom)
  }

  property("newSupport") = {
    val dom = Box("x" -> Interval(0, 1), "y" -> Interval(1, 2))
    val x: Expression = "x"
    val y: Expression = "y"
    dom contains equalToZero(x - y).support(dom)
    dom contains nonNegative(x - y).support(dom)
  }

}
