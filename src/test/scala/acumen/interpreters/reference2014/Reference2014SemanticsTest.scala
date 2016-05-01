package acumen.interpreters.reference2014

import acumen._
import acumen.Errors._
import acumen.interpreters.semanticstest._

class Reference2014SemanticsTest extends Traditional2014Tests {

  def semantics = SemanticsImpl.Ref2014
  
  override def examplesSkip(f: String) = {
    super.examplesSkip(f) ||
    f.endsWith("03_Projects/02_Jawad/02_ADAS_RTL_Rear_END.acm") ||
    f.endsWith("02_Jawad/04_ADAS_REAR_END_novisual.acm") ||
    f.endsWith("00_Demos/09_Cam_Follower.acm") ||
    f.endsWith("00_Demos/02_Passive_walking.acm")
  }

}
