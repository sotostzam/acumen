package acumen.interpreters.optimized

import acumen._
import acumen.Errors._
import acumen.interpreters.semanticstest._
import java.io.File

class Optimized2014SemanticsTest extends Traditional2014Tests {

  override def semantics = SemanticsImpl.Opt2014

  override def examplesSkip(f: String) = {
    super.examplesSkip(f) ||
    f.endsWith("03_Projects" + File.separator + "02_Jawad" + File.separator + "02_ADAS_RTL_Rear_END.acm") ||
    f.endsWith("02_Jawad" + File.separator + "04_ADAS_REAR_END_novisual.acm") ||
    f.endsWith("00_Demos" + File.separator + "09_Cam_Follower.acm") ||
    f.endsWith("00_Demos" + File.separator + "02_Passive_walking.acm")
  }

}

