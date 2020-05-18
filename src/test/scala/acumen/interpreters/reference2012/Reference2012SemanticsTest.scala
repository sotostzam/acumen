package acumen.interpreters.reference2012

import acumen._
import acumen.interpreters.semanticstest._
import java.io.File

class Reference2012SemanticsTest extends SemanticsTestBase with BasicErrorNoPosTests {

  override def semantics = SemanticsImpl.Ref2012
  override def examples = Examples2012
  override def examplesSkip(f: String) = {
    super.examplesSkip(f) ||
    f.endsWith("03_Projects" + File.separator + "02_Jawad" + File.separator + "02_ADAS_RTL_Rear_END.acm") ||
    f.endsWith("02_Jawad" + File.separator + "04_ADAS_REAR_END_novisual.acm") ||
    f.endsWith("00_Demos" + File.separator + "09_Cam_Follower.acm") ||
    f.endsWith("00_Demos" + File.separator + "02_Passive_walking.acm")
  }

}
