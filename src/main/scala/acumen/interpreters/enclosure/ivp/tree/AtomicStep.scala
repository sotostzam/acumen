package acumen.interpreters.enclosure.ivp.tree

import acumen.interpreters.enclosure.Rounding
import acumen.interpreters.enclosure.Box
import acumen.interpreters.enclosure.Interval
import acumen.interpreters.enclosure.Types._
import acumen.interpreters.enclosure.affine.UnivariateAffineEnclosure

trait AtomicStep extends SolveVtE {

  type MaybeResult = Option[(Seq[UnivariateAffineEnclosure], Set[UncertainState])]

  private def union(bs: Set[Box])(implicit rnd: Rounding) = bs.tail.fold(bs.head)(_ hull _)

  def bestOf(minComputationImprovement: Double)(
    result: (Seq[UnivariateAffineEnclosure], Set[UncertainState]),
    maybeResult: MaybeResult)(implicit rnd: Rounding) =
    (result, maybeResult) match {
      case (_, None) =>
        result
      case ((_, us1), Some(result2 @ (_, us2))) =>
        val norm1 = union(us1.map(_.initialCondition)).l1Norm
        val norm2 = union(us2.map(_.initialCondition)).l1Norm
        if ((norm1 - norm2) lessThan minComputationImprovement) result
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
    val res = us.map(solveVtE(H, t, _, delta, m, n, degree, K, log))
    if (res contains None) None
    else {
      val (us2, es) = res.map(_.get).foldLeft((Set[UncertainState](), Seq[UnivariateAffineEnclosure]())) {
        case ((resss, resys), (ss, ys)) => (resss ++ ss, resys ++ ys)
      }
      Some((es, M(us2)))
    }
  }

}