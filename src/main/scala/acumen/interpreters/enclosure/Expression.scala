package acumen.interpreters.enclosure

import Types._

/**
 * Type used to represent expressions used to define functions and 
 * predicates.
 *
 * Implementation note: by representing a function or predicate as an
 * explicit syntax tree we can easily determine if two functions are
 * equivalent. We will need to do this e.g. when implementing event-
 * variable independence analysis.
 */
abstract class Expression {

  /**
   * Evaluate the expression at the box x.
   *
   * Precondition: the box must contain the names of all variables in the
   * expression.
   */
  def apply(x: Box)(implicit rnd: Rounding): Interval = {
    assert(varNames subsetOf x.keySet, "The box must contain the names of all variables in the expression.")
    applyHelper(x).range
  } 

  /**
   * Helper for apply.
   *
   * It uses enclosure arithmetic to evaluate an expression over a box
   * before computing the result interval as the range of the resulting
   * enclosure. This minimizes dependency problems and wrapping effects
   * that would arise from using plain interval arithmetic throughout.
   *
   * Implementation note: we use the worker-wrapper pattern do stop
   * evaluation from prematurely using the top-level apply.
   */
  private def applyHelper(x: Box)(implicit rnd: Rounding): AffineScalarEnclosure = this match {
    case Constant(v) => AffineScalarEnclosure(x, v)
    case Variable(name) => AffineScalarEnclosure(x, name)
    case Negate(e) => -(e.applyHelper(x))
    case Plus(l, r) => l.applyHelper(x) + r.applyHelper(x)
    case Multiply(l, r) => l.applyHelper(x) * r.applyHelper(x)
    case Divide(e, Constant(v)) => e.applyHelper(x) / v
  }
  
  /** Returns the set of variable names which occur in the expression. */
  def varNames: Set[VarName] = this match {
    case Constant(_) => Set()
    case Variable(name) => Set(name)
    case Negate(e) => e.varNames
    case Plus(l, r) => l.varNames union r.varNames
    case Multiply(l, r) => l.varNames union r.varNames
    case Divide(l, r) => l.varNames union r.varNames
  }

  /* Arithmetic operations */

  def unary_- = this match {
    case Constant(v) => Constant(-v)
    case _ => Negate(this)
  }
  def +(that: Expression) = Plus(this, that)
  def -(that: Expression) = Plus(this, -that)
  def *(that: Expression) = Multiply(this, that)
  /** Only division by constants currently supported. */
  def /(that: Double)(implicit rnd: Rounding) = Divide(this, Constant(that))

}
object Expression {

  /** Implicit lifting of numeric values allows for writing e.g. x + 1. */
  implicit def lift(value: Interval) = Constant(value)
  implicit def lift(value: Double)(implicit r: Rounding) = Constant(value)
  implicit def lift(value: Int)(implicit r: Rounding) = Constant(value)

  /** Implicit lifting of variable names allows for writing e.g. x + "y". */
  implicit def lift(name: String) = Variable(name)

}

case class Constant(value: Interval) extends Expression {
  override def toString = value.toString
}
object Constant {
  def apply(value: Double)(implicit r: Rounding): Constant = Constant(Interval(value))
  def apply(value: Int)(implicit r: Rounding): Constant = Constant(Interval(value))
}

case class Variable(name: String) extends Expression {
  override def toString = name
}

case class Negate(expression: Expression) extends Expression {
  override def toString = "-" + expression
}

case class Plus(left: Expression, right: Expression) extends Expression {
  override def toString = "(" + left + " + " + right + ")"
}

case class Multiply(left: Expression, right: Expression) extends Expression {
  override def toString = "(" + left + " * " + right + ")"
}

case class Divide(left: Expression, right: Expression) extends Expression {
  override def toString = "(" + left + " / " + right + ")"
}

