package acumen.interpreters.enclosure

import org.scalacheck.Prop._
import org.scalacheck.Properties
import Interval._
import Types._
import tree.WalidSolver

object WalidSolverTest extends Properties("WalidSolver") with WalidSolver {

  import TestingContext._

  property("goodEnough true for thin enclosure") = {
    val x = UnivariateAffineEnclosure(Interval(0, 0.5), Map("x" -> Interval(1)))
    goodEnough("x", 0, Interval(0, 1), x)
  }

  property("goodEnough for quadratic") = {
    def quadratic(dom: Interval, varname: VarName) = {
      val x = AffineEnclosure(Box(varname -> dom), varname)
      UnivariateAffineEnclosure(x * x)
    }
    goodEnough("x", 0, Interval(0, 1), quadratic(Interval(0, 0.25), "x"))
    goodEnough("x", 0, Interval(0, 1), quadratic(Interval(0.25, 0.5), "x"))
    goodEnough("x", 0, Interval(0, 1), quadratic(Interval(0.5, 0.75), "x"))
    goodEnough("x", 0, Interval(0, 1), quadratic(Interval(0.75, 1), "x"))
  }

}