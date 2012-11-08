package acumen

import acumen.interpreters.Common.classDef
import acumen.interpreters.enclosure.UnivariateAffineEnclosure.plot
import acumen.interpreters.enclosure.solver.Solver.defaultCallback
import acumen.interpreters.enclosure.solver.FixedStepSolver
import acumen.interpreters.enclosure.solver.HybridSystem
import acumen.interpreters.enclosure.Extract
import acumen.interpreters.enclosure.Rounding
import acumen.interpreters.enclosure.solver.Solver
import acumen.interpreters.enclosure.solver.SolveVt
import acumen.interpreters.enclosure.Variable
import acumen.interpreters.enclosure.solver.Field
import acumen.interpreters.enclosure.Interval
import acumen.interpreters.enclosure.Box
import acumen.interpreters.enclosure.UnivariateAffineEnclosure
import TransformTest._
import java.awt.Color

object Sandbox extends App with Extract with Solver with SolveVt {

  val prog = Parser.run(Parser.prog, Models.damped_spring)
  val des = Desugarer.run(prog)
  val main = classDef(ClassName("Main"), des)

  val ps = parameters(main)
  implicit val rnd = Rounding(ps.precision)
  val (h: HybridSystem, us) = extract(main)

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
  //  println("computed " + resPreBounce.size + " enclosures in " + time / 1000.0 + " seconds")

  plot("Plotter mockup")(res)(new Rounding(10))
  //  plot("Plotter mockup")(resCons)(new Rounding(10))

}
