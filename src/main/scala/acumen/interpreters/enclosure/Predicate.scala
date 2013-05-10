package acumen.interpreters.enclosure

import Types._
import acumen.interpreters.enclosure.affine.UnivariateAffineEnclosure

/**
 * Type used to represent predicates.
 *
 * Implementation note: TODO motivate why predicates are separated from relations.
 */
abstract class Predicate {

  def compose(that: Expression, intoVariable: String): Predicate = this match {
    case True => True
    case all @ All(_) => All(all.conjuncts.map(_.compose(that, intoVariable)))
  }

  /**
   * Evaluate the predicate by taking the variables to range over the
   * intervals of the box x.
   */
  def apply(x: Box)(implicit rnd: Rounding): Set[Boolean] = this match {
    case True => Set(true)
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

  // TODO do something about the code duplication in these instances! 
  /**
   * Evaluate the predicate by composing with the enclosure and taking the
   * variables to range over the domains of the variables.
   */
  def apply(x: UnivariateAffineEnclosure)(implicit rnd: Rounding): Set[Boolean] = this match {
    case True => Set(true)
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
    case True => x
    case All(rs) =>
      val contracted = rs.foldLeft(x)((res, r) => r.support(res))
      if (contracted almostEqualTo x) contracted
      else support(contracted)
  }

}

/** Type representing a predicate that consists of a conjunction of inequalities. */
case object True extends Predicate {
  override def toString = "TRUE"
}
case class All(conjuncts: Seq[Relation]) extends Predicate {
  override def toString = conjuncts.map(_.toString).mkString(" /\\ ") match {
    case "" => "TRUE"
    case s => s
  }
}

object PredicateApp extends App {
  implicit val rnd = Rounding(10)

  val b = Box("r" -> Interval(0, 1), "v" -> Interval(0, 1))
  val r = Variable("r")
  val v = Variable("v")

  println(Relation.nonPositive(r - v * v / 2).support(b))

}





