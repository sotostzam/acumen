package acumen.interpreters.enclosure

import Types._
import acumen.interpreters.enclosure.affine.UnivariateAffineEnclosure
import acumen.interpreters.enclosure.affine.AffineScalarEnclosure
import acumen.interpreters.enclosure.affine.UnivariateAffineScalarEnclosure
import acumen.interpreters.enclosure.affine.AffineEnclosure
import scala.collection.SortedMap

/**
 * Type used to represent expressions used to define functions and
 * predicates.
 *
 * Implementation note: by representing a function or predicate as an
 * explicit syntax tree we can easily determine if two functions are
 * equivalent. We will need to do this e.g. when implementing event-
 * variable independence analysis.
 */
sealed abstract class Expression {
  
  val rnd = new Rounding(Parameters.default)

  def isConstant = this match {
    case Constant(_) => true
    case _           => false
  }

  // FIXME implement map for Expressions to avoid this
  def compose(that: Expression, intoVariable: String): Expression = this match {
    case Variable(name) if name == intoVariable => that
    case Constant(_) | Variable(_)              => this
    case Abs(e)                                 => Abs(e.compose(that, intoVariable))
    case Sqrt(e)                                => Sqrt(e.compose(that, intoVariable))
    case Exp(e)                                 => Exp(e.compose(that, intoVariable))
    case Log(e)                                 => Log(e.compose(that, intoVariable))
    case Sin(e)                                 => Sin(e.compose(that, intoVariable))
    case Cos(e)                                 => Cos(e.compose(that, intoVariable))
    case Negate(e)                              => Negate(e.compose(that, intoVariable))
    case Plus(l, r)                             => Plus(l.compose(that, intoVariable), r.compose(that, intoVariable))
    case Multiply(l, r)                         => Multiply(l.compose(that, intoVariable), r.compose(that, intoVariable))
    case Divide(l, r)                           => Divide(l.compose(that, intoVariable), r.compose(that, intoVariable))
  }

  /**
   * Evaluate the expression at the box x using intervals.
   *
   * This is dome by replacing variables in the expression with intervals
   * and operations by interval operations.
   *
   * Precondition: the box must contain the names of all variables in the
   * expression.
   */
  def apply(x: Box): Interval = {
    assert(varNames subsetOf x.keySet, "The box " + x + " must contain the names of all variables in the expression " + this)
    this match {
      case Constant(v)    => v
      case Variable(name) => x(name)
      case Abs(e)         => e(x).abs
      case Sqrt(e)        => e(x).sqrt
      case Exp(e)         => e(x).exp
      case Log(e)         => sys.error("undefined")
      case Sin(e)         => sys.error("undefined")
      case Cos(e)         => sys.error("undefined")
      case Negate(e)      => -e(x)
      case Plus(l, r)     => l(x) + r(x)
      case Multiply(l, r) =>
        if (l == r) l(x) square
        else l(x) * r(x)
      case Divide(n, d) => n(x) / d(x)
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
  def apply(x: UnivariateAffineEnclosure): UnivariateAffineScalarEnclosure = {
    assert(varNames subsetOf x.components.keySet,
      "The enclosure must contain the names of all variables in the expression.")
    this match {
      case Constant(v)            => UnivariateAffineScalarEnclosure(x.domain, v)
      case Variable(name)         => x(name)
      case Negate(e)              => -e(x)
      case Sqrt(e)                => sys.error("undefined")
      case Exp(e)                 => sys.error("undefined")
      case Log(e)                 => sys.error("undefined")
      case Sin(e)                 => sys.error("undefined")
      case Cos(e)                 => sys.error("undefined")
      case Plus(l, r)             => l(x) + r(x)
      case Multiply(l, r)         => l(x) * r(x)
      case Divide(e, Constant(v)) => e(x) / v // division not supported by enclosure arithmetic
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
   * 
   * NOTE: For sqrt and exp, Interval over-approximations are used.
   */
  def apply(x: AffineEnclosure): AffineScalarEnclosure = this match {
    case Constant(v)    => AffineScalarEnclosure(x.domain, v)
    case Variable(name) => x(name)
    case Negate(e)      => -e(x)
    case Sqrt(e)        => AffineScalarEnclosure(x.domain, e(x).range.sqrt) // FIXME Interval over-approximation!
    case Exp(e)         => AffineScalarEnclosure(x.domain, e(x).range.exp) // FIXME Interval over-approximation!
    case Log(e)         => sys.error("undefined")
    case Cos(e)         => rnd.transcendentals.cos(e(x))
    case Sin(e)         => rnd.transcendentals.sin(e(x))
    case Plus(l, r)     => l(x) + r(x)
    case Multiply(l, r) => l(x) * r(x)
    case Divide(l, r)   => rnd.transcendentals.div(l(x), r(x))
  }

  /**
   * Evaluate the expression at the box x using affine enclosures.
   *
   * Precondition: the box must contain the names of all variables in the
   * expression.
   */
  def enclosureEval(x: Box): Interval = {
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
  private def enclosureEvalHelper(x: Box): AffineScalarEnclosure = this match {
    case Constant(v)            => AffineScalarEnclosure(x, v)
    case Variable(name)         => AffineScalarEnclosure(x, name)
    case Negate(e)              => -(e.enclosureEvalHelper(x))
    case Sqrt(e)                => sys.error("undefined")
    case Exp(e)                 => sys.error("undefined")
    case Log(e)                 => sys.error("undefined")
    case Sin(e)                 => sys.error("undefined")
    case Cos(e)                 => sys.error("undefined")
    case Plus(l, r)             => l.enclosureEvalHelper(x) + r.enclosureEvalHelper(x)
    case Multiply(l, r)         => l.enclosureEvalHelper(x) * r.enclosureEvalHelper(x)
    case Divide(e, Constant(v)) => e.enclosureEvalHelper(x) / v
  }

  // TODO add explanation!
  def contractBox(box: Box, ran: Interval): Box =
    enclosureEvalHelper(box).contractDomain(ran)

  /** Returns the set of variable names which occur in the expression. */
  def varNames: Set[VarName] = this match {
    case Constant(_)    => Set()
    case Variable(name) => Set(name)
    case Abs(e)         => e.varNames
    case Sqrt(e)        => e.varNames
    case Exp(e)         => e.varNames
    case Log(e)         => e.varNames
    case Sin(e)         => e.varNames
    case Cos(e)         => e.varNames
    case Negate(e)      => e.varNames
    case Plus(l, r)     => l.varNames union r.varNames
    case Multiply(l, r) => l.varNames union r.varNames
    case Divide(l, r)   => l.varNames union r.varNames
  }

  /* Arithmetic operations */

  def unary_- = this match {
    case Constant(v) => Constant(-v)
    case _           => Negate(this)
  }
  def +(that: Expression) = (this, that) match {
    case (Constant(c), e) if c isZero => e
    case (e, Constant(c)) if c isZero => e
    case (l, r) if l == r             => Multiply(Constant(2), l)
    case _                            => Plus(this, that)
  }
  def -(that: Expression) = this + (-that)
  def *(that: Expression) = (this, that) match {
    case (Constant(c), e) if c equalTo 1 => e
    case (Constant(c), e) if c isZero    => Constant(0)
    case (e, Constant(c)) if c equalTo 1 => e
    case (e, Constant(c)) if c isZero    => Constant(0)
    case _                               => Multiply(this, that)
  }
  def /(that: Double) = Divide(this, Constant(that))
  def /(that: Expression) = (this, that) match {
    case (Constant(c), e) if c isZero    => Constant(0)
    case (e, Constant(c)) if c equalTo 1 => e
    case _                               => Divide(this, that)
  }
  def sqrt = Sqrt(this)
  def exp = Exp(this)
  def log = Log(this)
  def sin = Sin(this)
  def cos = Cos(this)

  def dif(name: VarName): Expression = this match {
    case Constant(_)    => Constant(0)
    case Variable(n)    => if (n == name) Constant(1) else Constant(0)
    case Negate(e)      => -e.dif(name)
    case Sqrt(e)        => e.dif(name) * 0.5 / this
    case Exp(e)         => this * e.dif(name)
    case Plus(l, r)     => l.dif(name) + r.dif(name)
    case Multiply(l, r) => l.dif(name) * r + l * r.dif(name)
    case Divide(l, r)   => (l.dif(name) * r - l * r.dif(name)) / (r * r)
    // fixme: Figure out why plain "this" will not work here and
    //   "this.toString" is now required -- kevina
    case _              => sys.error(this.toString + ".dif(" + name + ") is not defined!")
  }

  def taylorCoefficient(multiIndex: SortedMap[VarName, Int]) = null

  def variables: Set[VarName]

}
object Expression {

  /** Implicit lifting of numeric values allows for writing e.g. x + 1. */
  implicit def lift(value: Interval): Expression = Constant(value)
  implicit def lift(value: Double): Expression = Constant(value)
  implicit def lift(value: Int): Expression = Constant(value)

  /** Implicit lifting of variable names allows for writing e.g. x + "y". */
  implicit def lift(name: String) = Variable(name)

}

case class Constant(value: Interval) extends Expression {
  override def toString = value.toString
  def variables: Set[VarName] = Set()
}
object Constant {
  def apply(value: Double): Constant = Constant(Interval(value))
  def apply(value: Int): Constant = Constant(Interval(value))
}

case class Variable(name: String) extends Expression {
  override def toString = name
  def variables: Set[VarName] = Set(name)
}

case class Abs(expression: Expression) extends Expression {
  override def toString = "abs(" + expression + ")"
  def variables: Set[VarName] = expression.variables
}

case class Sqrt(expression: Expression) extends Expression {
  override def toString = "sqrt(" + expression + ")"
  def variables: Set[VarName] = expression.variables
}

case class Exp(expression: Expression) extends Expression {
  override def toString = "exp(" + expression + ")"
  def variables: Set[VarName] = expression.variables
}

case class Log(expression: Expression) extends Expression {
  override def toString = "exp(" + expression + ")"
  def variables: Set[VarName] = expression.variables
}

case class Sin(expression: Expression) extends Expression {
  override def toString = "sin(" + expression + ")"
  def variables: Set[VarName] = expression.variables
}

case class Cos(expression: Expression) extends Expression {
  override def toString = "cos(" + expression + ")"
  def variables: Set[VarName] = expression.variables
}

case class Negate(expression: Expression) extends Expression {
  override def toString = "-" + expression
  def variables: Set[VarName] = expression.variables
}

case class Plus(left: Expression, right: Expression) extends Expression {
  override def toString = "(" + left + " + " + right + ")"
  def variables: Set[VarName] = left.variables ++ right.variables
}

case class Multiply(left: Expression, right: Expression) extends Expression {
  override def toString = "(" + left + " * " + right + ")"
  def variables: Set[VarName] = left.variables ++ right.variables
}

case class Divide(left: Expression, right: Expression) extends Expression {
  override def toString = "(" + left + " / " + right + ")"
  def variables: Set[VarName] = left.variables ++ right.variables
}

object ExpressionApp extends App {

  val x = Variable("x")
  val y = Variable("y")
  println((x * (x + y)).compose(1 + y, "x"))

}
