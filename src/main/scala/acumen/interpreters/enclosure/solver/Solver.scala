package acumen.interpreters.enclosure.solver

import acumen.interpreters.enclosure._
import acumen.interpreters.enclosure.Interval._
import acumen.interpreters.enclosure.Types._
import acumen.interpreters.enclosure.EnclosureInterpreterCallbacks

trait Solver extends SolveVtE {

  case class SolverException(message: String) extends Exception
  case class Aborted(resultSoFar: Seq[UnivariateAffineEnclosure]) extends Exception

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
    cb: EnclosureInterpreterCallbacks
    )(implicit rnd: Rounding): (Set[UncertainState], Seq[UnivariateAffineEnclosure]) = {
    val onT = Ss.map(solveVtE(H, T, _, delta, m, n, K, output, cb.log))
    val mustSplit = T.width greaterThan e
    val (lT, rT) = T.split
    val cannotSplit = !(min(lT.width, rT.width) greaterThan d)
    if (mustSplit || (onT contains None))
      if (cannotSplit) {
        throw SolverException("gave up for minimum step size " + d + " at " + T)
      } else {
//        cb.log("splitting " + T)
        val (ssl, ysl) = solveHybrid(H, lT, Ss, delta, m, n, K, d, e, output, cb)
        val (ssr, ysr) = solveHybrid(H, rT, ssl, delta, m, n, K, d, e, output, cb)
        (ssr, ysl ++ ysr)
      }
    else {
      val resultForT @ (endStatesOnT, enclosuresOnT) =
        onT.map(_.get).foldLeft((Set[UncertainState](), Seq[UnivariateAffineEnclosure]())) {
          case ((resss, resys), (ss, ys)) => (resss ++ ss, resys ++ ys)
        }
      val ssT = M(endStatesOnT)

      val onlT = Ss.map(solveVtE(H, lT, _, delta, m, n, K, output, cb.log))
      if (onlT contains None) {
        cb.sendResult(resultForT._2)
        resultForT
      } else {
        val (endStatesOnlT, enclosuresOnlT) =
          onlT.map(_.get).foldLeft((Set[UncertainState](), Seq[UnivariateAffineEnclosure]())) {
            case ((resss, resys), (ss, ys)) => (resss ++ ss, resys ++ ys)
          }
        val sslT = M(endStatesOnlT)

        val onrT = sslT.map(solveVtE(H, rT, _, delta, m, n, K, output, cb.log))
        if (onrT contains None) {
          cb.sendResult(resultForT._2)
          resultForT
        } else {
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
            {
              //              log("improvement : " + improvement);
              improvement lessThanOrEqualTo Interval(0.00001)
            }) {
            cb.sendResult(resultForT._2)
            resultForT
          } else {
//            cb.log("splitting " + T)
            val (ssl, ysl) = solveHybrid(H, lT, Ss, delta, m, n, K, d, e, output, cb)
            val (ssr, ysr) = solveHybrid(H, rT, ssl, delta, m, n, K, d, e, output, cb)
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
    cb: EnclosureInterpreterCallbacks)
  (implicit rnd: Rounding): Seq[UnivariateAffineEnclosure] = {
    Util.newFile(output)
    cb.endTime = T.hiDouble
    solveHybrid(H, T, Ss, delta, m, n, K, d, e, output, cb)._2
  }
}

object Solver {
  def defaultCallback = new EnclosureInterpreterCallbacks {
    override def log(msg:String) = println(msg)
  }
}
