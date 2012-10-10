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
   * Evaluate the expression at the box x using intervals.
   *
   * This is dome by replacing variables in the expression with intervals
   * and operations by interval operations.
   *
   * Precondition: the box must contain the names of all variables in the
   * expression.
   */
  def apply(x: Box)(implicit rnd: Rounding): Interval = {
    assert(varNames subsetOf x.keySet, "The box must contain the names of all variables in the expression.")
    this match {
      case Constant(v) => v
      case Variable(name) => x(name)
      case Negate(e) => -e(x)
      case Plus(l, r) => l(x) + r(x)
      case Multiply(l, r) => l(x) * r(x)
      case Divide(e, Constant(v)) => e(x) / v
    }
  }

  // TODO do something about the code duplication in these instances!
  /**
   * Evaluate the expression at the enclosure x using enclosures.
   *
   * This is done by replacing the variables in the expression with the
   * corresponding component in the enclosure and the operations by enclosure
   * operations.
   *
   * Precondition: the box must contain the names of all variables in the
   * expression.
   */
  def apply(x: UnivariateAffineEnclosure)(implicit rnd: Rounding): UnivariateAffineScalarEnclosure = {
    assert(varNames subsetOf x.components.keySet,
      "The enclosure must contain the names of all variables in the expression.")
    this match {
      case Constant(v) => UnivariateAffineScalarEnclosure(x.domain, v)
      case Variable(name) => x(name)
      case Negate(e) => -e(x)
      case Plus(l, r) => l(x) + r(x)
      case Multiply(l, r) => l(x) * r(x)
      case Divide(e, Constant(v)) => e(x) / v
    }
  }

  /**
   * Evaluate the expression at the box x using enclosures.
   *
   * This is done by replacing variables in the expressions by the affine
   * scalar enclosures in their corresponding component and operations by
   * affine scalar enclosure operations.
   *
   * Precondition: the box must contain the names of all variables in the
   * expression.
   */
  def apply(x: AffineEnclosure)(implicit rnd: Rounding): AffineScalarEnclosure = this match {
    case Constant(v) => AffineScalarEnclosure(x.domain, v)
    case Variable(name) => x(name)
    case Negate(e) => -e(x)
    case Plus(l, r) => l(x) + r(x)
    case Multiply(l, r) => l(x) * r(x)
    case Divide(e, Constant(v)) => e(x) / v
  }

  /**
   * Evaluate the expression at the box x using affine enclosures.
   *
   * Precondition: the box must contain the names of all variables in the
   * expression.
   */
  def enclosureEval(x: Box)(implicit rnd: Rounding): Interval = {
    assert(varNames subsetOf x.keySet, "The box must contain the names of all variables in the expression.")
    enclosureEvalHelper(x).range
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
  private def enclosureEvalHelper(x: Box)(implicit rnd: Rounding): AffineScalarEnclosure = this match {
    case Constant(v) => AffineScalarEnclosure(x, v)
    case Variable(name) => AffineScalarEnclosure(x, name)
    case Negate(e) => -(e.enclosureEvalHelper(x))
    case Plus(l, r) => l.enclosureEvalHelper(x) + r.enclosureEvalHelper(x)
    case Multiply(l, r) => l.enclosureEvalHelper(x) * r.enclosureEvalHelper(x)
    case Divide(e, Constant(v)) => e.enclosureEvalHelper(x) / v
  }

  // TODO add explanation!
  def contractBox(box: Box, ran: Interval)(implicit rnd: Rounding): Box =
    enclosureEvalHelper(box).contractDomain(ran)

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
  def +(that: Expression) = (this, that) match {
    case (Constant(c), e) if (c isZero) => e
    case (e, Constant(c)) if (c isZero) => e
    case _ => Plus(this, that)
  }
  def -(that: Expression) = this + (-that)
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

