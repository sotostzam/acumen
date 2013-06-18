package acumen.interpreters.enclosure.limbo

import acumen.interpreters.enclosure.Interval.min
import acumen.interpreters.enclosure.Interval.toInterval
import acumen.interpreters.enclosure.Types.M
import acumen.interpreters.enclosure.Types.UncertainState
import acumen.interpreters.enclosure.Types.endTimeInterval
import acumen.interpreters.enclosure.EnclosureInterpreterCallbacks
import acumen.interpreters.enclosure.Interval
import acumen.interpreters.enclosure.Rounding
import acumen.interpreters.enclosure.affine.UnivariateAffineEnclosure
import acumen.interpreters.enclosure.Util
import acumen.interpreters.enclosure.HybridSystem
import acumen.interpreters.enclosure.event.tree.TreeEventEncloser

trait JanSolver extends TreeEventEncloser {

  case class SolverException(message: String) extends Exception
  case class Aborted(resultSoFar: Seq[UnivariateAffineEnclosure]) extends Exception

  // TODO add description
  def solver(
    H: HybridSystem, // system to simulate
    T: Interval, // time segment to simulate over
    Ss: Set[UncertainState], // initial modes and initial conditions
    delta: Double, // parameter of solveVt
    m: Int, // parameter of solveVt
    n: Int, // maximum number of Picard iterations in solveVt
    degree:Int, // splittingDegree
    K: Int, // maximum event tree size in solveVtE
    d: Double, // minimum time step size
    e: Double, // maximum time step size
    output: String, // path to write output 
    cb: EnclosureInterpreterCallbacks)(implicit rnd: Rounding): Seq[UnivariateAffineEnclosure] = {

    // TODO add description
    def solveHybrid(
      T: Interval, // time segment to simulate over
      Ss: Set[UncertainState] // initial modes and initial conditions
      ): (Set[UncertainState], Seq[UnivariateAffineEnclosure]) = {

      def localizeEvents: Interval = {
        def maybeContainsEvent(x: Interval): Boolean = {
          
          true
        }
        null
      }

      val onT = Ss.map(solveVtE(H, T, _, delta, m, n, degree, K, cb.log))
      val mustSplit = T.width greaterThan e
      val (lT, rT) = T.split
      val cannotSplit = !(min(lT.width, rT.width) greaterThan d)
      if (mustSplit || (onT contains None))
        if (cannotSplit) {
          throw SolverException("gave up for minimum step size " + d + " at " + T)
        } else {
          //        cb.log("splitting " + T)
          val (ssl, ysl) = solveHybrid(lT, Ss)
          val (ssr, ysr) = solveHybrid(rT, ssl)
          (ssr, ysl ++ ysr)
        }
      else {
        val resultForT @ (endStatesOnT, enclosuresOnT) =
          onT.map(_.get).foldLeft((Set[UncertainState](), Seq[UnivariateAffineEnclosure]())) {
            case ((resss, resys), (ss, ys)) => (resss ++ ss, resys ++ ys)
          }
        val ssT = M(endStatesOnT)

        val onlT = Ss.map(solveVtE(H, lT, _, delta, m, n, degree, K, cb.log))
        if (onlT contains None) {
          /**
           * STOP SUBDIVISION
           */
          cb.sendResult(resultForT._2)
          resultForT
        } else {
          val (endStatesOnlT, enclosuresOnlT) =
            onlT.map(_.get).foldLeft((Set[UncertainState](), Seq[UnivariateAffineEnclosure]())) {
              case ((resss, resys), (ss, ys)) => (resss ++ ss, resys ++ ys)
            }
          val sslT = M(endStatesOnlT)

          val onrT = sslT.map(solveVtE(H, rT, _, delta, m, n, K, degree, cb.log))
          if (onrT contains None) {
            /**
             * STOP SUBDIVISION
             */
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
              /**
               * STOP SUBDIVISION
               */
              cb.sendResult(resultForT._2)
              resultForT
            } else {
              //            cb.log("splitting " + T)
              val (ssl, ysl) = solveHybrid(lT, Ss)
              val (ssr, ysr) = solveHybrid(rT, ssl)
              (ssr, ysl ++ ysr)
            }
          }
        }
      }
    }

    Util.newFile(output)
    cb.endTime = T.hiDouble
    solveHybrid(T, Ss)._2
  }

  def leftBiasedDepthFirstSearch(maxDepth: Int, satisfies: Interval => Boolean, t: Interval): Option[Interval] =
    depthFirstSearch { case (x, y) => (x, y) }(maxDepth, satisfies, t)

  def rightBiasedDepthFirstSearch(maxDepth: Int, satisfies: Interval => Boolean, t: Interval): Option[Interval] =
    depthFirstSearch { case (x, y) => (y, x) }(maxDepth, satisfies, t)

  def depthFirstSearch(baise: ((Interval, Interval)) => (Interval, Interval))(maxDepth: Int, satisfies: Interval => Boolean, t: Interval): Option[Interval] = {
    require(maxDepth >= 0, "maxDepth has to be nonnegative!")
    def helper(queue: Seq[(Int, Interval)]): Option[Interval] = queue match {
      case (depth, s) :: ss =>
        if (depth < maxDepth)
          if (!satisfies(s)) helper(ss)
          else {
            val (l, r) = baise(s.split)
            helper((depth + 1, r) :: (depth + 1, l) :: ss)
          }
        else Some(s /\ ss.last._2)
      case _ => None
    }
    helper(Seq((0, t)))
  }

}

object JanSolver {
  def defaultCallback = new EnclosureInterpreterCallbacks {
    override def log(msg: String) = println(msg)
  }
}
