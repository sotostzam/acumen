package acumen.interpreters.enclosure.solver

import SolveVt._
import acumen.interpreters.enclosure._
import acumen.interpreters.enclosure.Interval._
import acumen.interpreters.enclosure.Types._

object Solver {

  /** Old version, implements incomplete and possibly erroneous version of event detection. */
  def oldDetectNextEvent(
    H: HybridSystem,
    T: Interval,
    q: Mode,
    Y: Map[VarName, UnivariateAffineScalarEnclosure])(implicit rnd: Rounding): Outcome = {
    val events =
      H.events.filter(
        H.guards(_)(
          Y.mapValues(_.range)) != Set(false))
    if (events.isEmpty) {
      MaybeOneOf(events)
    } else {
      if (H.events.exists { e =>
        H.guardPrime(e)(Y.mapValues(c => c(c.domain.high))) == Set(true)
      }) {
        CertainlyOneOf(events)
      } else {
        MaybeOneOf(events)
      }
    }
  }

  def detectNextEvent(
    H: HybridSystem,
    T: Interval,
    q: Mode,
    Y: UnivariateAffineEnclosure)(implicit rnd: Rounding): Outcome = {
    val events = H.events.filter(H.guards(_)(Y.range) != Set(false))
    if (events.isEmpty) {
      MaybeOneOf(events)
    } else {
      if (H.domains(q)(Y(Y.domain.high)) == Set(false)) {
        println("detected one of " + events + " in " + T)
        CertainlyOneOf(events)
      } else MaybeOneOf(events)
    }
  }

  def solveVtE(
    H: HybridSystem,
    T: Interval,
    S: UncertainState,
    delta: Double,
    m: Int,
    n: Int,
    K: Int,
    output: String)(implicit rnd: Rounding): Option[(Set[UncertainState], Seq[UnivariateAffineEnclosure])] = {
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
    output: String // path to write output 
    )(implicit rnd: Rounding): (Set[UncertainState], Seq[UnivariateAffineEnclosure]) = {
    val onT = Ss.map(solveVtE(H, T, _, delta, m, n, K, output))
    val mustSplit = T.width greaterThan e
    val (lT, rT) = T.split
    val cannotSplit = !(min(lT.width, rT.width) greaterThan d)
    if (mustSplit || (onT contains None))
      if (cannotSplit) {
        throw SolverException("gave up for minimum step size " + d + " at " + T)
      } else {
        println("splitting " + T)
        val (ssl, ysl) = solveHybrid(H, lT, Ss, delta, m, n, K, d, e, output)
        val (ssr, ysr) = solveHybrid(H, rT, ssl, delta, m, n, K, d, e, output)
        (ssr, ysl ++ ysr)
      }
    else {
      val resultForT @ (endStatesOnT, enclosuresOnT) =
        onT.map(_.get).foldLeft((Set[UncertainState](), Seq[UnivariateAffineEnclosure]())) {
          case ((resss, resys), (ss, ys)) => (resss ++ ss, resys ++ ys)
        }
      val ssT = M(endStatesOnT)

      val onlT = Ss.map(solveVtE(H, lT, _, delta, m, n, K, output))
      if (onlT contains None)
        resultForT
      else {
        val (endStatesOnlT, enclosuresOnlT) =
          onlT.map(_.get).foldLeft((Set[UncertainState](), Seq[UnivariateAffineEnclosure]())) {
            case ((resss, resys), (ss, ys)) => (resss ++ ss, resys ++ ys)
          }
        val sslT = M(endStatesOnlT)

        val onrT = sslT.map(solveVtE(H, rT, _, delta, m, n, K, output))
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
          lazy val noImprovement = nowhereBetter || somewhereWorse

          if (cannotSplit || noImprovement) {
            resultForT
          } else {
            println("splitting " + T)
            val (ssl, ysl) = solveHybrid(H, lT, Ss, delta, m, n, K, d, e, output)
            val (ssr, ysr) = solveHybrid(H, rT, ssl, delta, m, n, K, d, e, output)
            (ssr, ysl ++ ysr)
          }
        }
      }
    }
  }

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
    output: String // path to write output 
    )(implicit rnd: Rounding): Seq[UnivariateAffineEnclosure] = {
    Util.newFile(output)
    try {
      solveHybrid(H, T, Ss, delta, m, n, K, d, e, output)._2
    } catch {
      case SolverException(message) => {
        println(message)
        println("solver: reducing minimum segement width to " + d / 2)
        solver(H, Tinit, Ss, delta, m, n, K, d / 2, e, Tinit, output)
      }
    }
  }

}
