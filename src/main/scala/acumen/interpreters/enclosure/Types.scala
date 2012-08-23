package acumen.interpreters.enclosure

object Types {

  /* Type synonyms */

  /** Variable name */
  type VarName = String

  /** Interval boxes with named components. */
  type Box = Map[VarName, Interval]

}
