package acumen.interpreters.enclosure.ivp.tree

import acumen.interpreters.enclosure._
import acumen.interpreters.enclosure.Types._

/**
 * Type for hybrid automaton reset maps
 *
 * Implementation note: TODO
 */
// TODO write tests.
case class ResetMap(components: Map[VarName, Expression]) {

  /**
   * Apply the reset map to a state vector
   *
   * Implementation note: TODO
   */
  def apply(x: Box)(implicit rnd: Rounding) = components.mapValues(_(x))
  def +(kv: (VarName, Expression)) = ResetMap(components + kv)

}

object ResetMap {

  /** The identity reset map, returns its argument when evaluated. */
  // TODO test the description.
  def id(names: Set[VarName]) = ResetMap(names.map(name => name -> Variable(name)).toMap)

}
