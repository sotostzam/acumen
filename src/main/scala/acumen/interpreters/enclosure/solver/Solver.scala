package acumen.interpreters.enclosure.solver

import SolveVt._
import acumen.interpreters.enclosure._
import acumen.interpreters.enclosure.Interval._
import acumen.interpreters.enclosure.Types._

trait Solver {

  // TODO add description
  def detectNextEvent(
    H: HybridSystem,
    T: Interval,
    q: Mode,
    Y: UnivariateAffineEnclosure)(implicit rnd: Rounding): Outcome = {
    val events = H.events.filter(e => e.sigma == q && H.guards(e)(Y.range) != Set(false))
    if (events.isEmpty) {
      MaybeOneOf(events)
    } else {
      if (H.domains(q)(Y(Y.domain.high)) == Set(false)) CertainlyOneOf(events)
      else MaybeOneOf(events)
    }
  }

  // TODO add description
  def solveVtE(
    H: HybridSystem,
    T: Interval,
    S: UncertainState,
    delta: Double,
    m: Int,
    n: Int,
    K: Int,
    output: String,
    log: String => Unit
    )(implicit rnd: Rounding): Option[(Set[UncertainState], Seq[UnivariateAffineEnclosure])] = {
    var res = EventTree.initialTree(T, H, S, delta, m, n, output)
    var tmp = res.addLayer
    while (res.size < K && tmp != res) {
      res = tmp
      tmp = tmp.addLayer
    }
    if (res != tmp)
      None
    else
      Some((res.endTimeStates, res.enclosures))
  }

  case class SolverException(message: String) extends Exception

  // TODO add description
  def solveHybrid(
    H: HybridSystem, // system to simulate
    T: Interval, // time segment to simulate over
    Ss: Set[UncertainState], // initial modes and initial conditions
    delta: Double, // padding for initial condition in solveVt
    m: Int, // number of extra Picard iterations in solveVt
    n: Int, // maximum number of Picard iterations in solveVt
    K: Int, // maximum event tree size in solveVtE, gives termination condition for tree enlargement
    d: Double, // minimum time step size
    e: Double, // maximum time step size
    output: String, // path to write output 
    log: String => Unit // function to call to log progress
    )(implicit rnd: Rounding): (Set[UncertainState], Seq[UnivariateAffineEnclosure]) = {
    val onT = Ss.map(solveVtE(H, T, _, delta, m, n, K, output, log))
    val mustSplit = T.width greaterThan e
    val (lT, rT) = T.split
    val cannotSplit = !(min(lT.width, rT.width) greaterThan d)
    if (mustSplit || (onT contains None))
      if (cannotSplit) {
        throw SolverException("gave up for minimum step size " + d + " at " + T)
      } else {
        log("splitting " + T)
        val (ssl, ysl) = solveHybrid(H, lT, Ss, delta, m, n, K, d, e, output, log)
        val (ssr, ysr) = solveHybrid(H, rT, ssl, delta, m, n, K, d, e, output, log)
        (ssr, ysl ++ ysr)
      }
    else {
      val resultForT @ (endStatesOnT, enclosuresOnT) =
        onT.map(_.get).foldLeft((Set[UncertainState](), Seq[UnivariateAffineEnclosure]())) {
          case ((resss, resys), (ss, ys)) => (resss ++ ss, resys ++ ys)
        }
      val ssT = M(endStatesOnT)

      val onlT = Ss.map(solveVtE(H, lT, _, delta, m, n, K, output, log))
      if (onlT contains None)
        resultForT
      else {
        val (endStatesOnlT, enclosuresOnlT) =
          onlT.map(_.get).foldLeft((Set[UncertainState](), Seq[UnivariateAffineEnclosure]())) {
            case ((resss, resys), (ss, ys)) => (resss ++ ss, resys ++ ys)
          }
        val sslT = M(endStatesOnlT)

        val onrT = sslT.map(solveVtE(H, rT, _, delta, m, n, K, output, log))
        if (onrT contains None)
          resultForT
        else {
          val (endStatesOnrT, enclosuresOnrT) =
            onrT.map(_.get).foldLeft((Set[UncertainState](), Seq[UnivariateAffineEnclosure]())) {
              case ((resss, resys), (ss, ys)) => (resss ++ ss, resys ++ ys)
            }
          val ssrT = M(endStatesOnrT)

          lazy val nowhereBetter = (endTimeInterval(ssrT) zip endTimeInterval(ssT)).forall {
            case ((_, l), (_, r)) => l.width greaterThanOrEqualTo r.width
          }
          lazy val somewhereWorse = (endTimeInterval(ssrT) zip endTimeInterval(ssT)).exists {
            case ((_, l), (_, r)) => l.width greaterThan r.width
          }
          //          lazy val noImprovement = nowhereBetter || somewhereWorse
          lazy val improvement = {
            val onT = endTimeInterval(ssT).values.foldLeft(Interval(0)) { case (res, i) => i.width + res }
            val onrT = endTimeInterval(ssrT).values.foldLeft(Interval(0)) { case (res, i) => i.width + res }
            onT - onrT
          }

          // TODO make the improvement threshold a parameter 
          if (cannotSplit ||
            ({
//              log("improvement : " + improvement);
              improvement lessThanOrEqualTo Interval(0.00001)
            })) {
            resultForT
          } else {
            log("splitting " + T)
            val (ssl, ysl) = solveHybrid(H, lT, Ss, delta, m, n, K, d, e, output, log)
            val (ssr, ysr) = solveHybrid(H, rT, ssl, delta, m, n, K, d, e, output, log)
            (ssr, ysl ++ ysr)
          }
        }
      }
    }
  }

  // TODO add description
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
    Tinit: Interval, // initial time segment
    output: String, // path to write output 
    log: String => Unit
    )(implicit rnd: Rounding): Seq[UnivariateAffineEnclosure] = {
    Util.newFile(output)
    try {
      solveHybrid(H, T, Ss, delta, m, n, K, d, e, output, log)._2
    } catch {
      case SolverException(message) => {
        log(message)
        log("solver: reducing minimum segement width to " + d / 2)
        solver(H, Tinit, Ss, delta, m, n, K, d / 2, e, Tinit, output, log)
      }
    }
  }

}
