package acumen.interpreters.enclosure

import Interval._
import Types._

object UnaryRelationName extends Enumeration {
  type UnaryRelationName = Value
  val Positive, NonNegative, EqualToZero, NonPositive, Negative = Value
}
import UnaryRelationName._

/**
 * Type used to represent relations used to define predicates.
 *
 * Implementation note: TODO motivate why we are not supporting everything.
 */
abstract class Relation {
  /**
   * Evaluate the relation in the environment x.
   *
   * property (monotonicity/soundness): For any relation r and environment
   * y with each interval contained in the corresponding interval in x it
   * holds that r(y) is contained in r(x).
   *
   * TIP try to make the test include ys with all intervals thin.
   *
   */
  def apply(x: Box)(implicit rnd: Rounding): Set[Boolean] = this match {
    case UnaryRelation(relname, e) => relname match {
      case Positive => {
        val value = e(x)
        if (value greaterThan 0) Set(true)
        else if (value lessThanOrEqualTo 0) Set(false)
        else Set(true, false)
      }
      case NonNegative => {
        val value = e(x)
        if (value greaterThanOrEqualTo 0) Set(true)
        else if (value lessThan 0) Set(false)
        else Set(true, false)
      }
      case EqualToZero => {
        val value = e(x)
        if (value equalTo 0) Set(true)
        else if (value contains 0) Set(true, false)
        else Set(false)
      }
      case NonPositive => {
        val value = e(x)
        if (value lessThanOrEqualTo 0) Set(true)
        else if (value greaterThan 0) Set(false)
        else Set(true, false)
      }
      case Negative => {
        val value = e(x)
        if (value lessThan 0) Set(true)
        else if (value greaterThanOrEqualTo 0) Set(false)
        else Set(true, false)
      }
      case _ => sys.error("Relation.eval: " + this.toString)
    }
  }

  /** A conservative approximation of the intersection of x with the support of r. */
  def support(x: Box)(implicit rnd: Rounding): Box = this match {
    case r @ UnaryRelation(relname, Variable(name)) => relname match {
      case Positive => {
        if (x(name) lessThanOrEqualTo 0) sys.error("Relation.support: Positive: empty intersection")
        else x - name + (name -> max(0, x(name).low) /\ x(name).high)
      }
      case NonNegative => {
        if (x(name) lessThan 0) sys.error("Relation.support: NonNegative: empty intersection with " + x(name))
        else x - name + (name -> max(0, x(name).low) /\ x(name).high)
      }
      case EqualToZero => {
        if (!(x(name) contains 0)) sys.error("Relation.support: EqualToZero: empty intersection")
        else x - name + (name -> 0)
      }
      case NonPositive => {
        if (x(name) greaterThan 0) sys.error("Relation.support: NonPositive: empty intersection")
        else x - name + (name -> x(name).low /\ min(0, x(name).high))
      }
      case Negative => {
        if (x(name) greaterThanOrEqualTo 0) sys.error("Relation.support: Negative: empty intersection")
        else x - name + (name -> x(name).low /\ min(0, x(name).high))
      }
    }
    case r @ UnaryRelation(relname, Negate(e)) => relname match {
      case Positive => Relation.negative(e).support(x)
      case NonNegative => Relation.nonPositive(e).support(x)
      case EqualToZero => Relation.equalToZero(e).support(x)
      case NonPositive => Relation.nonNegative(e).support(x)
      case Negative => Relation.positive(e).support(x)
    }
    case r @ UnaryRelation(relname, Divide(e, Constant(c))) =>
      if (c greaterThan 0) UnaryRelation(relname, e).support(x)
      else if (c lessThan 0) UnaryRelation(relname, Negate(e)).support(x)
      else sys.error("Relation.support: division by 0")
  }
}
object Relation {
  def positive(that: Expression) = UnaryRelation(Positive, that)
  def nonNegative(that: Expression) = UnaryRelation(NonNegative, that)
  def equalToZero(that: Expression) = UnaryRelation(EqualToZero, that)
  def nonPositive(that: Expression) = UnaryRelation(NonPositive, that)
  def negative(that: Expression) = UnaryRelation(Negative, that)
}

case class UnaryRelation(name: UnaryRelationName, expression: Expression) extends Relation {
  override def toString = name match {
    case Positive => expression + " > 0"
    case NonPositive => expression + " <= 0"
    case EqualToZero => expression + " = 0"
    case NonNegative => expression + " >= 0"
    case Negative => expression + " < 0"
  }
}

