package acumen
package util

import Pretty.pprint
import Canonical.{setInSimulator,getInSimulator}
import Conversions.extractDouble

object ExpectsObserves 
{
  val TOLERANCE = 1e-15 
  // Check that expects and observes match in simulator.
  // no need to check if the exist as they are guaranteed to be there
  def check(p: Prog, lastRes: CStore) : CStore = try {
    val expects  = extractDouble(getInSimulator(Name("expects",0), lastRes))
    val observes = extractDouble(getInSimulator(Name("observes",0), lastRes))
    val res = 
      if (expects.isNaN)
        ExpectationUnspecified
      else if (expects - TOLERANCE < observes && observes < expects + TOLERANCE)
             ExpectationMet
      else
        ExpectationNotMet
    setInSimulator(Name("expectsResult",0), VExpectsResult(res), lastRes)
  } catch {
    // expects/observes should be in the simulator in most cases, but
    // if the store is manually specified (as in some test cases) they
    // may not...
    case _: java.util.NoSuchElementException => lastRes
  }
}
