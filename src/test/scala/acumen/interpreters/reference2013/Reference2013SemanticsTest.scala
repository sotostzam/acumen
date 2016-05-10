package acumen.interpreters.reference2013

import acumen._
import acumen.Errors._
import acumen.interpreters.semanticstest._

class Reference2013SemanticsTest extends SemanticsTestBase with BasicErrorNoPosTests {

  override def semantics = SemanticsImpl.Ref2013
  override def examples = Examples2013
  
  override def examplesSkip(f: String) = {
    super.examplesSkip(f) ||
    f.endsWith("03_Projects/02_Jawad/02_ADAS_RTL_Rear_END.acm") ||
    f.endsWith("02_Jawad/04_ADAS_REAR_END_novisual.acm") ||
    f.endsWith("00_Demos/09_Cam_Follower.acm") ||
    f.endsWith("00_Demos/02_Passive_walking.acm")
  }

  test("ACUMEN-348") {
    val err = DuplicateDiscreteAssingment(Name("period",0))
    getError("data/ShouldCrash/ACUMEN-348.acm") should be (Some(err))
  }

}

