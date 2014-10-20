package acumen.testutil

import org.scalacheck.Prop.propBoolean
import org.scalacheck.Properties
import TransformationTestUtil.userModeVariables
import acumen.Parser
import acumen.Name

object TransformationTestUtilTest extends Properties("TransformationTestUtil") {
  
  property("userModeVariables of day-night-heater-ha.acm") = {
    val source = acumen.util.System.readFile("examples/05_Language_Research/06_Esaes/11_day-night-heater-ha.acm")
    val prog = Parser.run(Parser.prog, source)
    userModeVariables(prog).toSet == Set(Name("heating",0), Name("period",0))
  }
  
}
