package acumen.interpreters.enclosure.tree

import acumen.interpreters.enclosure._
import acumen.interpreters.enclosure.Interval._
import acumen.interpreters.enclosure.Types._

trait WalidSolver extends SolveVtE {

  case class SolverException(message: String) extends Exception
  case class Aborted(resultSoFar: Seq[UnivariateAffineEnclosure]) extends Exception

  /**
   * Subdivides the simulation time segment according to Walid's idea.
   *
   * Add description...
   */
  def solver(
    variableOfInterest: VarName, // the variable to measure precision of enclosures by
    targetPrecision: Double, // the desired width at T.high of the result's variableOfInterest component 
    H: HybridSystem, // system to simulate
    T: Interval, // time segment to simulate over
    uncertainStates: Set[UncertainState], // initial modes and initial conditions
    delta: Double, // padding for initial condition in solveVt
    m: Int, // number of extra Picard iterations in solveVt
    n: Int, // maximum number of Picard iterations in solveVt
    degree:Int, // number of pieces to split each initial condition interval
    K: Int, // maximum event tree size in solveVtE, gives termination condition for tree enlargement
    d: Double, // minimum time step size
    e: Double, // maximum time step size
    output: String, // path to write output 
    cb: EnclosureInterpreterCallbacks // carrier for call-backs for communicating with the GUI
    )(implicit rnd: Rounding): Seq[UnivariateAffineEnclosure] = {
    assert(uncertainStates.forall(_.initialCondition.keySet.contains(variableOfInterest)),
      "the variable " + variableOfInterest + " has not been assigned an initial condition")
    Util.newFile(output)
    cb.endTime = T.hiDouble

    var result = Seq[UnivariateAffineEnclosure]()
    var initialCondition = uncertainStates
    var leftEndpoint = T.low
    var rightEndpoint = T.high
    while (leftEndpoint lessThan rightEndpoint) {
      val currentSegment = leftEndpoint /\ rightEndpoint
      val canSubdivide = currentSegment.width / 2 greaterThanOrEqualTo d
      val makeAtomicStep = atomicStep(H, currentSegment, initialCondition, delta, m, n, degree, K, d, e, output, cb)
      makeAtomicStep match {
        case None =>
          println("failed to construct enclosure on " + currentSegment)
          if (canSubdivide) rightEndpoint = max(leftEndpoint, rightEndpoint - (currentSegment.width / 2))
          else throw SolverException("gave up at " + currentSegment)
        case Some((nextUncertainStates, enclosure)) =>
          println("constructed enclosure on " + currentSegment)
          if ({
            val enclosureGoodEnough = goodEnough(variableOfInterest, targetPrecision, T, enclosure)
            println("enclosure" + (if (enclosureGoodEnough) "" else " not") + " good enough!")
            println("can" + (if (canSubdivide) "" else "not") + " subdivide!")
            enclosureGoodEnough || !canSubdivide
          }) {
            println("logging enclosure on " + currentSegment)
            cb.sendResult(Seq(enclosure))
            result = result :+ enclosure
            initialCondition = nextUncertainStates
            leftEndpoint = rightEndpoint
            rightEndpoint = min(rightEndpoint + (currentSegment.width * 2), T.high)
          } else {
            rightEndpoint = max(leftEndpoint, rightEndpoint - (currentSegment.width / 2))
          }
      }
    }
    result
  }

  /**
   * Attempts to compute an enclosure and the next uncertainStates for H over T
   * starting from the uncertainStates.
   */
  def atomicStep(
    H: HybridSystem, // system to simulate
    T: Interval, // time segment to simulate over
    uncertainStates: Set[UncertainState], // initial modes and initial conditions
    delta: Double, // padding for initial condition in solveVt
    m: Int, // number of extra Picard iterations in solveVt
    n: Int, // maximum number of Picard iterations in solveVt
    degree:Int, // splittingDegree
    K: Int, // maximum event tree size in solveVtE, gives termination condition for tree enlargement
    d: Double, // minimum time step size
    e: Double, // maximum time step size
    output: String, // path to write output 
    cb: EnclosureInterpreterCallbacks // carrier for call-backs for communicating with the GUI
    )(implicit rnd: Rounding): Option[(Set[UncertainState], UnivariateAffineEnclosure)] = {
    val onT = uncertainStates.map(solveVtE(H, T, _, delta, m, n, degree, K, output, cb.log))
    val failedToComputeAnEnclosure = onT contains None
    if (failedToComputeAnEnclosure) None
    else Some(onT.map(_.get).foldLeft((Set[UncertainState](), Seq[UnivariateAffineEnclosure]())) {
      case ((resss, resys), (ss, ys)) => (resss ++ ss, resys ++ ys)
    } match { case (ess, es) => (ess, UnivariateAffineEnclosure.unionThem(es).head) })
  }

  /**
   * Decides if the rate of change of the width of the enclosure is less
   * than the targetPrecision divided by the total simulation time.
   */
  def goodEnough(
    variableOfInterest: VarName,
    targetPrecision: Double,
    initialTimeSegment: Interval,
    enclosure: UnivariateAffineEnclosure)(implicit rnd: Rounding): Boolean = {
    wideningRate(variableOfInterest, enclosure) lessThan
      (targetPrecision / initialTimeSegment.width)
  }

  /** Computes the rate at which the enclosure for the variable of interest widens */
  def wideningRate(
    variableOfInterest: VarName,
    enclosure: UnivariateAffineEnclosure)(implicit rnd: Rounding): Interval = {
    val segment = enclosure.domain
    val inWidth: Interval = enclosure(segment.low)(variableOfInterest).width
    val outWidth: Interval = enclosure(segment.high)(variableOfInterest).width
    (outWidth - inWidth) / segment.width
  }

}
