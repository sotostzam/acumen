package acumen.interpreters.enclosure.solver

import acumen.interpreters.enclosure.Interval
import acumen.interpreters.enclosure.Types._
import acumen.interpreters.enclosure.UnivariateAffineEnclosure
import acumen.interpreters.enclosure.Rounding
import acumen.interpreters.enclosure.Box
import acumen.interpreters.enclosure.EnclosureInterpreterCallbacks
import acumen.interpreters.enclosure.Util

trait HybridSolver extends AtomicStep {

  def solver(
    H: HybridSystem, // system to simulate
    T: Interval, // time segment to simulate over
    Ss: Set[UncertainState], // initial modes and initial conditions
    delta: Double, // parameter of solveVt
    m: Int, // parameter of solveVt
    n: Int, // maximum number of Picard iterations in solveVt
    K: Int, // maximum event tree size in solveVtE
    d: Double, // minimum time step size
    e: Double, // maximum time step size
    minImprovement: Double, // minimum improvement of enclosure
    output: String, // path to write output 
    cb: EnclosureInterpreterCallbacks)(implicit rnd: Rounding): Seq[UnivariateAffineEnclosure] = {
    Util.newFile(output)
    cb.endTime = T.hiDouble
	    val res = solveHybrid(H, delta, m, n, K, output, cb.log)(d, minImprovement)(Ss, T).get._1
    println(res)
    res
  }

  def solveHybrid(
    H: HybridSystem,
    delta: Double,
    m: Int,
    n: Int,
    K: Int,
    output: String,
    log: String => Unit)(
      minTimeStep: Double,
      minImprovement: Double)(us: Set[UncertainState], t: Interval)(implicit rnd: Rounding): MaybeResult = {
    val maybeResultT = atomicStep(H, delta, m, n, K, output, log)(us, t)
    if (t.width lessThanOrEqualTo minTimeStep) maybeResultT
    else maybeResultT match {
      case None => subdivideAndRecur(H, delta, m, n, K, output, log)(minTimeStep, minImprovement)(us, t)
      case Some(resultT) =>
        val maybeResultTR = subdivideOneLevelOnly(H, delta, m, n, K, output, log)(us, t)
        if (bestOf(resultT, maybeResultTR) == resultT) Some(resultT)
        else subdivideAndRecur(H, delta, m, n, K, output, log)(minTimeStep, minImprovement)(us, t)
    }
  }

  def subdivideAndRecur(
    H: HybridSystem,
    delta: Double,
    m: Int,
    n: Int,
    K: Int,
    output: String,
    log: String => Unit)(
      minTimeStep: Double,
      minImprovement: Double)(us: Set[UncertainState], t: Interval)(implicit rnd: Rounding): MaybeResult =
    solveHybrid(H, delta, m, n, K, output, log)(minTimeStep, minImprovement)(us, t.left) match {
      case None => None
      case Some((esl, usl)) =>
        solveHybrid(H, delta, m, n, K, output, log)(minTimeStep, minImprovement)(usl, t.right) match {
          case None => None
          case Some((esr, usr)) =>
            Some((esl ++ esr, usr))
        }
    }

  def subdivideOneLevelOnly(
    H: HybridSystem,
    delta: Double,
    m: Int,
    n: Int,
    K: Int,
    output: String,
    log: String => Unit)(us: Set[UncertainState], t: Interval)(implicit rnd: Rounding): MaybeResult =
    atomicStep(H, delta, m, n, K, output, log)(us, t.left) match {
      case None => None
      case Some((es, usl)) => atomicStep(H, delta, m, n, K, output, log)(usl, t.right)
    }

}


