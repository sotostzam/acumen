package acumen.interpreters.enclosure.solver

import acumen.interpreters.enclosure.Types._
import acumen.interpreters.enclosure.Rounding
import acumen.interpreters.enclosure.AffineEnclosure
import acumen.interpreters.enclosure.Expression
import acumen.interpreters.enclosure.Variable
import acumen.interpreters.enclosure.Constant

/**
 * Type for representing fields of differential equations.
 *
 * Implementation note: TODO
 */
// TODO write tests.
case class Field(components: Map[VarName, Expression]) {

  /**
   * Apply the field to an enclosure.
   *
   * Implementation note: TODO
   */
  def apply(x: AffineEnclosure)(implicit rnd: Rounding) = {
    AffineEnclosure(x.domain, x.normalizedDomain, components.mapValues(_(x)))
  }

  /**
   * The components of the Jacobian matrix of the field.
   */
  def jacobian(component: VarName, variable: VarName)(implicit rnd: Rounding): Expression = {
    require(components.keySet contains component, this + " must have a " + component + " component!")
    components(component).dif(variable)
  }
  
}

object FieldApp extends App {

  implicit val rnd = Rounding(10)

  println(Field(Map("x" -> "x'", "x'" -> -(Variable("x") + Variable("x'")))).jacobian("x", "x'"))

}