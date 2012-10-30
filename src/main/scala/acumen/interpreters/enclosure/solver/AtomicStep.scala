package acumen.interpreters.enclosure.solver

import acumen.interpreters.enclosure.UnivariateAffineEnclosure
import acumen.interpreters.enclosure.Rounding
import acumen.interpreters.enclosure.Box
import acumen.interpreters.enclosure.Interval
import acumen.interpreters.enclosure.Types._

trait AtomicStep extends SolveVtE {

  type MaybeResult = Option[(Seq[UnivariateAffineEnclosure], Set[UncertainState])]

  private def union(bs: Set[Box])(implicit rnd: Rounding) = bs.tail.fold(bs.head)(_ union _)

  def bestOf(
    result: (Seq[UnivariateAffineEnclosure], Set[UncertainState]),
    maybeResult: MaybeResult)(implicit rnd: Rounding) =
    (result, maybeResult) match {
      case (_, None) =>
        result
      case ((_, us1), Some(result2 @ (_, us2))) =>
        if (precision(union(us1.map(_.initialCondition))) -
          precision(union(us2.map(_.initialCondition))) lessThan 0.00001) result
        else result2
    }

  def atomicStep(
    H: HybridSystem,
    delta: Double,
    m: Int,
    n: Int,
    K: Int,
    output: String,
    log: String => Unit)(
      us: Set[UncertainState],
      t: Interval)(implicit rnd: Rounding): MaybeResult = {
    val res = us.map(solveVtE(H, t, _, delta, m, n, K, output, log))
    if (res contains None) None
    else {
      val (us2, es) = res.map(_.get).foldLeft((Set[UncertainState](), Seq[UnivariateAffineEnclosure]())) {
        case ((resss, resys), (ss, ys)) => (resss ++ ss, resys ++ ys)
      }
      Some((es, M(us2)))
    }
  }

}