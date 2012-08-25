package acumen.interpreters.enclosure

import Types._

/**
 * Type used to represent predicates.
 *
 * Implementation note: TODO motivate why predicates are separated from relations.
 */
abstract class Predicate {

  /**
   * Evaluate the predicate by taking the variables to range over the
   * intervals of the box x.
   */
  def apply(x: Box)(implicit rnd: Rounding): Set[Boolean] = this match {
    case All(ps) => {
      ps.foldLeft(Set(true)) {
        case (res, p) => {
          res.zipAll(p(x), true, true).map {
            case (l, r) => l && r
          }
        }
      }
    }
    case _ => sys.error("Predicate.eval: " + this.toString)
  }

  /** 
   * Compute an interval box wrapping the intersection of x with the support 
   * of the predicate. 
   */
  def support(x: Box)(implicit rnd: Rounding): Box = this match {
    case All(rs) => rs.foldLeft(x)((res, r) => r.support(res))
  }

}

/** Type representing a predicate that consists of a conjunction of inequalities. */
case class All(conjuncts: Seq[Relation]) extends Predicate {
  override def toString = conjuncts.map(_.toString).mkString(" /\\ ") match {
    case "" => "TRUE"
    case s => s
  }
}
