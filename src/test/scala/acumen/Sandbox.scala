package acumen

import acumen.interpreters.Common._
import acumen.interpreters.enclosure.Types._
import acumen.interpreters.enclosure.Extract
import acumen.interpreters.enclosure.Rounding
import acumen.interpreters.enclosure.Interval
import acumen.interpreters.enclosure.Interval._
import acumen.interpreters.enclosure.Types._
import acumen.interpreters.enclosure.solver._
import acumen.interpreters.enclosure.solver.Solver._
import acumen.interpreters.enclosure.UnivariateAffineScalarEnclosure
import acumen.interpreters.enclosure.UnivariateAffineEnclosure
import TransformTest._
import acumen.ui.EnclosureTraceModel
import acumen.ui.PlotEnclosure
import java.awt.Color

object Sandbox extends App with Extract with HybridSolver {

  val prog = Parser.run(Parser.prog, Models.bbfu)
  val des = Desugarer.run(prog)
  val main = classDef(ClassName("Main"), des)

  val ps = parameters(main)
  implicit val rnd = Rounding(ps.precision)
  val (h, us) = extract(main)

  val start = System.currentTimeMillis
  val res = solver(
    h,
    ps.simulationTime,
    Set(us),
    ps.solveVtInitialConditionPadding,
    ps.extraPicardIterations,
    ps.maxPicardIterations,
    ps.maxEventTreeSize,
    ps.minTimeStep,
    ps.maxTimeStep,
    ps.minImprovement,
    "output",
    defaultCallback)
  val end = System.currentTimeMillis
  val time = end - start
  println("computed " + res.size + " enclosures in " + time / 1000.0 + " seconds")
  UnivariateAffineScalarEnclosure.plot(
//    res.map(e => (Color.BLUE, e("x"))) ++ res.map(e => (new Color(0, 127, 0), e("x'"))) ++ res.map(e => (Color.RED, e("r"))): _*)
    res.map(e => (Color.BLUE, e("x1"))) ++ res.map(e => (new Color(0, 127, 0), e("x2"))): _*)

}
