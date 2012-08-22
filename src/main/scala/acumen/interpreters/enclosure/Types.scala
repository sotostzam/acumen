package acumen.interpreters.enclosure

object Types {

  /* Type synonyms */

  /** Interval boxes with named components */
  type Box = Map[VarName, Interval]
  object Box { def empty: Box = Map[VarName, Interval]() }

  /** Variable name */
  type VarName = String

}