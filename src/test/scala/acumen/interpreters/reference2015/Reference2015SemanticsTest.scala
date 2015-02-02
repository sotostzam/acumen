package acumen.interpreters.reference2015

import acumen._
import acumen.Errors._
import acumen.interpreters.semanticstest._

class Reference2015SemanticsTest extends Traditional2015Tests {

  def semantics = SemanticsImpl.Ref2015
  
  override def examplesSkip(f: String) = {
    super.examplesSkip(f) ||
    f.endsWith("03_Projects/02_Jawad/02_ADAS_RTL_Rear_END.acm") ||
    f.endsWith("02_Jawad/04_ADAS_REAR_END_novisual.acm")
  }

}
