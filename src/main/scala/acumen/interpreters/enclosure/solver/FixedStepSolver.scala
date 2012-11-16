package acumen.interpreters.enclosure.solver

import acumen.interpreters.enclosure._
import acumen.interpreters.enclosure.Interval._
import acumen.interpreters.enclosure.Types._
import acumen.ui.interpreter.Enclosure

trait FixedStepSolver extends SolveVtE {

  case class SolverException(message: String) extends Exception
  case class Aborted(resultSoFar: Seq[UnivariateAffineEnclosure]) extends Exception

  /**
   * Subdivides the simulation time segment into d-sized pieces.
   *
   * Add description...
   */
  def solver(
    H: HybridSystem, // system to simulate
    T: Interval, // time segment to simulate over
    uncertainStates: Set[UncertainState], // initial modes and initial conditions
    delta: Double, // padding for initial condition in solveVt
    m: Int, // number of extra Picard iterations in solveVt
    n: Int, // maximum number of Picard iterations in solveVt
    degree:Int, // pieces to split each initial condition interval
    K: Int, // maximum event tree size in solveVtE
    d: Double, // minimum time step size
    e: Double, // maximum time step size
    minImprovement: Double, // minimum improvement of enclosure
    output: String, // path to write output 
    cb: EnclosureInterpreterCallbacks // carrier for GUI call-backs 
    )(implicit rnd: Rounding): Seq[UnivariateAffineEnclosure] = {
    assert(d > 0, "minimum step size " + d + " not allowed!")
    Util.newFile(output)
    cb.endTime = T.hiDouble

    /**
     * Attempts to compute an enclosure and the next uncertainStates for H over T
     * starting from the uncertainStates.
     */
    def atomicStep(
      T: Interval, // time segment to simulate over
      uncertainStates: Set[UncertainState] // initial modes and initial conditions
      ): Option[(Set[UncertainState], Seq[UnivariateAffineEnclosure])] = {
      val onT = uncertainStates.map(solveVtE(H, T, _, delta, m, n, K, degree, output, cb.log))
      val failedToComputeAnEnclosure = onT contains None
      if (failedToComputeAnEnclosure) None
      else Some(onT.map(_.get).
        foldLeft((Set[UncertainState](), Seq[UnivariateAffineEnclosure]())) {
          case ((resss, resys), (ss, ys)) => (resss ++ ss, resys ++ ys)
        } //      match { case (ess, es) => (ess, UnivariateAffineEnclosure.unionThem(es).head) }
        )
    }

    var result = Seq[UnivariateAffineEnclosure]()
    var initialConditions = uncertainStates
    val pieces = (T.width / d).hiDouble.ceil

    var segments = Seq(T)
    while (segments.head.width.low greaterThan d) {
      segments = segments.flatMap(_.split match { case (l, r) => Seq(l, r) })
    }

    for (segment <- segments) {
      //    for (i <- 0 to pieces.toInt) {
      //      val segment = (T.low + T.width * i / pieces) /\ (T.low + T.width * (i + 1) / pieces)
      println("Segment:       " + segment)
      atomicStep(segment, initialConditions) match {
        case None =>
          throw SolverException("failed to construct enclosure on " + segment)
        case Some((nextUncertainStates, enclosures)) =>
          //        case Some((nextUncertainStates, enclosure)) =>
          initialConditions = nextUncertainStates
          cb.sendResult(enclosures)
          //          cb.sendResult(Seq(enclosure))
          //          result = result :+ enclosure
          result = result ++ enclosures
      }
    }
    result
  }

}
