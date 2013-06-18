package acumen.interpreters.enclosure

import Types._
import acumen.interpreters.enclosure.affine.UnivariateAffineEnclosure

/**
 * Type used to represent predicates.
 *
 * Implementation note: TODO motivate why predicates are separated from relations.
 */
sealed abstract class Predicate {

  def compose(that: Expression, intoVariable: String): Predicate = this match {
    case True            => True
    case False           => False
    case Or(left, right) => Or(left.compose(that, intoVariable), right.compose(that, intoVariable))
    case all @ All(_)    => All(all.conjuncts.map(_.compose(that, intoVariable)))
  }

  /**
   * Evaluate the predicate by taking the variables to range over the
   * intervals of the box x.
   */
  def apply(x: Box)(implicit rnd: Rounding): Set[Boolean] = this match {
    case True            => Set(true)
    case False           => Set(false)
    case Or(left, right) => for (l <- left(x); r <- right(x)) yield l || r
    case All(ps) => {
      ps.foldLeft(Set(true)) {
        case (res, p) => {
          res.zipAll(p(x), true, true).map {
            case (l, r) => l && r
          }
        }
      }
    }
  }

  // TODO do something about the code duplication in these instances! 
  /**
   * Evaluate the predicate by composing with the enclosure and taking the
   * variables to range over the domains of the variables.
   */
  def apply(x: UnivariateAffineEnclosure)(implicit rnd: Rounding): Set[Boolean] = this match {
    case True            => Set(true)
    case Or(left, right) => for (l <- left(x); r <- right(x)) yield l || r
    case All(ps) => {
      ps.foldLeft(Set(true)) {
        case (res, p) => {
          res.zipAll(p(x), true, true).map {
            case (l, r) => l && r
          }
        }
      }
    }
  }

  /**
   * Compute an interval box wrapping the intersection of x with the support
   * of the predicate.
   */
  def support(x: Box)(implicit rnd: Rounding): Box = this match {
    case True  => x
    case False => sys.error("empty support") // note that this represents the empty box not a program failure!
    case Or(left, right) => // FIXME desugar Or into min a'la Realpaver-0.4 user manual, page 16 and contract over min! 
      try { // FIXME avoid re-computation of l, _or better_ re-implement partiality using Option in place of exceptions
        val l = left.support(x)
        val r = right.support(x)
        l hull r
      }
      catch { // l or r is empty
        case _ => try {
          left.support(x) // if l is nonempty r must have been empty, so return l
        }
        catch { // l is empty, but r may not be
          case _ => try {
            right.support(x)
          }
        }
      }
    case All(rs) =>
      val contracted = rs.foldLeft(x)((res, r) => r.support(res))
      if (contracted almostEqualTo x) contracted
      else support(contracted)
  }

}

case object True extends Predicate {
  override def toString = "TRUE"
}
case object False extends Predicate {
  override def toString = "FALSE"
}
case class Or(left: Predicate, right: Predicate) extends Predicate {
  override def toString = "(" + left + " \\/ " + right + ")"
}
/** Type representing a predicate that consists of a conjunction of inequalities. */
case class All(conjuncts: Seq[Relation]) extends Predicate {
  override def toString = conjuncts.map(_.toString).mkString(" /\\ ") match {
    case "" => "TRUE"
    case s  => s
  }
}

object PredicateApp extends App {

  implicit val rnd = Parameters.default.rnd

  val b = Box("r" -> Interval(0, 1), "v" -> Interval(0, 1))
  val r = Variable("r")
  val v = Variable("v")

  println(All(Seq(Relation.lessThanOrEqualTo(r, v))).support(b))

}





