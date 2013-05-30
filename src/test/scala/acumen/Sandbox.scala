package acumen

import acumen.interpreters.enclosure.Extract
import acumen.interpreters.enclosure.Interval
import acumen.interpreters.enclosure.Rounding
import acumen.interpreters.enclosure.affine.UnivariateAffineEnclosure
import acumen.interpreters.enclosure.affine.UnivariateAffineScalarEnclosure
import acumen.interpreters.enclosure.Parameters

object Sandbox extends App with Extract { // with SolveVt {

  //  val prog = Parser.run(Parser.prog, Models("bouncing_ball_convergent"))
  //  val prog = Parser.run(Parser.prog, Models.fullers_phenomenon)
  //  val des = Desugarer.run(prog)
  //  val main = classDef(ClassName("Main"), des)
  //
  //  val ps = parameters(main)
  //  implicit val rnd = Rounding(ps.bigDecimalDigits)
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

  implicit val rnd = Parameters.default.rnd

  def rename(e: UnivariateAffineEnclosure, oldName: String, newName: String) =
    UnivariateAffineEnclosure(e.domain, e.components.map { case (k, v) => ((if (k == oldName) newName else k), v) })

  val plotter = new acumen.ui.plot.EnclosurePlotter
  val dom = Interval(0, 0.5)
  //  val dom = Interval(0, 0.5)
  val y = UnivariateAffineEnclosure(dom, Map("x" ->
    UnivariateAffineScalarEnclosure(dom, Interval(0), Interval(1))))
  val z = UnivariateAffineEnclosure(dom, Map("x" ->
    UnivariateAffineScalarEnclosure(dom, Interval(1), Interval(-1))))
  //    val y = UnivariateAffineEnclosure(dom, Map("x" ->
  //    UnivariateAffineScalarEnclosure(dom, Interval(-1,0), Interval(0.5,1))))
  //    val z = UnivariateAffineEnclosure(dom, Map("x" ->
  //    UnivariateAffineScalarEnclosure(dom, Interval(0,1), Interval(-1,-0.5))))
  val yuz = y union z

  plotter.plot("f")(null)(Seq(
    rename(y, "x", "y"),
    rename(z, "x", "z"),
    rename(yuz, "x", "yuz")))(rnd)

}
