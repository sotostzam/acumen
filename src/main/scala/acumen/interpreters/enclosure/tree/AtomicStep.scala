package acumen.interpreters.enclosure.tree

import acumen.interpreters.enclosure.UnivariateAffineEnclosure
import acumen.interpreters.enclosure.Rounding
import acumen.interpreters.enclosure.Box
import acumen.interpreters.enclosure.Interval
import acumen.interpreters.enclosure.Types._

trait AtomicStep extends SolveVtE {

  type MaybeResult = Option[(Seq[UnivariateAffineEnclosure], Set[UncertainState])]

  private def union(bs: Set[Box])(implicit rnd: Rounding) = bs.tail.fold(bs.head)(_ hull _)

  def bestOf(minImprovement: Double)(
    result: (Seq[UnivariateAffineEnclosure], Set[UncertainState]),
    maybeResult: MaybeResult)(implicit rnd: Rounding) =
    (result, maybeResult) match {
      case (_, None) =>
        result
      case ((_, us1), Some(result2 @ (_, us2))) =>
        val norm1 = norm(union(us1.map(_.initialCondition)))
        val norm2 = norm(union(us2.map(_.initialCondition)))
        if ((norm1 - norm2) lessThan minImprovement) result
        else result2
    }
  
  def atomicStep(
    H: HybridSystem,
    delta: Double,
    m: Int,
    n: Int,
    degree:Int, 
    K: Int,
    output: String,
    log: String => Unit)(
      us: Set[UncertainState],
      t: Interval)(implicit rnd: Rounding): MaybeResult = {
    val res = us.map(solveVtE(H, t, _, delta, m, n, degree, K, output, log))
    if (res contains None) None
    else {
      val (us2, es) = res.map(_.get).foldLeft((Set[UncertainState](), Seq[UnivariateAffineEnclosure]())) {
        case ((resss, resys), (ss, ys)) => (resss ++ ss, resys ++ ys)
      }
      Some((es, M(us2)))
    }
  }

}