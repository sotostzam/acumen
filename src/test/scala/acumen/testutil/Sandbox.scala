package acumen
package testutil

//import acumen.interpreters.enclosure.Extract
import acumen.interpreters.enclosure.Interval
import acumen.interpreters.enclosure.affine.UnivariateAffineEnclosure
import acumen.interpreters.enclosure.affine.UnivariateAffineScalarEnclosure
import acumen.interpreters.enclosure.Parameters

object Sandbox extends App with acumen.interpreters.enclosure.Extract { // with SolveVt {

  //  val prog = Parser.run(Parser.prog, Models("bouncing_ball_convergent"))
  //  val prog = Parser.run(Parser.prog, Models.fullers_phenomenon)
  //  val des = Desugarer().run(prog)
  //  val main = classDef(ClassName("Main"), des)
  //
  //  val ps = parameters(main)
  //  val (h: HybridSystem, us) = extract(main)
  //
  //  val start = System.currentTimeMillis
  //
  //  val res =
  //    solver(
  //      h,
  //      ps.simulationTime,
  //      Set(us),
  //      ps.initialPicardPadding,
  //      ps.picardImprovements,
  //      ps.maxPicardIterations,
  //      ps.splittingDegree,
  //      ps.maxEventTreeSize,
  //      ps.minTimeStep,
  //      ps.maxTimeStep,
  //      ps.minComputationImprovement,
  //      "output",
  //      defaultCallback)
  //
  //  val end = System.currentTimeMillis
  //  val time = end - start
  //  println("computed " + res.size + " enclosures in " + time / 1000.0 + " seconds")
  //
  //  def twoMinusExpOfMinusT(t: Double) = 2 - scala.math.exp(-t) // DELETEME
  //  def fiveMinusFiveTSquare(t: Double) = 5 - 5 * scala.math.pow(t, 2) // DELETEME
  //
  //  val plotter = new acumen.interpreters.enclosure.Plotter
  //  plotter.plot("Plotter mockup")(null)(res)(new Rounding(10))


  def rename(e: UnivariateAffineEnclosure, oldName: String, newName: String) =
    UnivariateAffineEnclosure(e.domain, e.components.map { case (k, v) => ((if (k == oldName) newName else k), v) })
}
