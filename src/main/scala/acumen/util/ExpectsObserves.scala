package acumen
package util

import Pretty.pprint
import Canonical.getInSimulator
import Conversions.extractDouble

object ExpectsObserves 
{
  val TOLERANCE = 1e-15 
  // Check that expects and observes match in simulator.
  // no need to check if the exist as they are guaranteed to be there
  def check(p: Prog, lastRes: CStore) : Option[String] = try {
    val expects  = extractDouble(getInSimulator(Name("expects",0), lastRes))
    val observes = extractDouble(getInSimulator(Name("observes",0), lastRes))
    if (expects - TOLERANCE < observes && observes < expects + TOLERANCE)
      None
    else
      fail(expects.toString, observes.toString)
  } catch {
    // expects/observes should be in the simulator in most cases, but
    // if the store is manually specified (as in some test cases) they
    // may not...
    case _: java.util.NoSuchElementException => None
  }
  private def fail(expected: String, observed: String) : Some[String] = {
    Some("Error: Expected value %s but observed value %s".format(expected,observed))
  }
}
