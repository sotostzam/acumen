package acumen
package interpreters
package enclosure

import org.scalacheck.Properties
import acumen.interpreters.enclosure.TestingContext

object RegressionTest extends Properties("Regression") {

  import TestingContext._

  // TODO: replace by iterating through XXX_internal/enclosure/regression/ 
  val models = List("ticker" -> ticker)

  val ticker = """
class Main(simulator) 
private mode := "On"; x := 1; x' := -1 end
  simulator.endTime := 2.5;
  switch mode
    case "On" require x >= 0
      if x == 0
        x := 1; 
        mode := "On"
      end; 
      x' = -1 
  end
end
"""

  property("moo") = {
    for ((name,model) <- models) {
      println(model == model)
    }
    true
  }

}