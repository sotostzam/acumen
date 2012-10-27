package acumen.interpreters.enclosure.solver

import acumen.interpreters.enclosure.Types._
import acumen.interpreters.enclosure.Rounding
import acumen.interpreters.enclosure.AffineEnclosure
import acumen.interpreters.enclosure.Expression
import acumen.interpreters.enclosure.Variable

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

}

object Field {
 
  /** The identity field, returns its argument when evaluated. */
  // TODO test the description.
  def id(names: Set[VarName]) = Field(names.map(name => name -> Variable(name)).toMap)

}
