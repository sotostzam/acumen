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

object Sandbox extends App with Extract with Solver {

  val prog = Parser.run(Parser.prog, Models.two_tanks_sum)
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
    res.map(e => (
        Color.getHSBColor(0.25f,1.0f,0.5f), e("x1"))) ++ 
        res.map(e => (Color.getHSBColor(0.5f,1.0f,0.5f), e("x2"))) ++ 
        res.map(e => (Color.getHSBColor(0.75f,1.0f,0.5f), e("x12"))): _*)
}
