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
  def check(p: Prog, lastRes: CStore) : Option[String] = {
    val expects  = extractDouble(getInSimulator(Name("expects",0), lastRes))
    val observes = extractDouble(getInSimulator(Name("observes",0), lastRes))
    if (expects - TOLERANCE < observes && observes < expects + TOLERANCE)
      None
    else
      fail(expects.toString, observes.toString)
  }
  private def fail(expected: String, observed: String) : Some[String] = {
    Some("Error: Expected value %s but observed value %s".format(expected,observed))
  }
}
