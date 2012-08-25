package acumen.interpreters.enclosure

import Box._

object Types {

  /* Type synonyms */

  /** Variable name */
  type VarName = String

  type AffineIntervalFunction = Map[VarName, AffineScalarIntervalFunction]

  type AffineScalarIntervalFunction = (Interval, Box)
}
