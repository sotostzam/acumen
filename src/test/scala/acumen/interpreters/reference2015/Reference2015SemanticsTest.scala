package acumen.interpreters.reference2015

import acumen._
import acumen.Errors._
import acumen.interpreters.semanticstest._

class Reference2015SemanticsTest extends Traditional2015Tests {

  def semantics = SemanticsImpl.Ref2015
  
  override def examplesSkip(f: String) = {
    super.examplesSkip(f) ||
      List( "03_Projects/02_Jawad/02_ADAS_RTL_Rear_END.acm"
          , "02_Jawad/04_ADAS_REAR_END_novisual.acm"
          , "09_Ha-extract/05_two-tanks-extracted-w-else.acm" // These are Zeno models,
          , "09_Ha-extract/06_two-tanks-extracted.acm"        // and will stall at t = 2.0 
          , "09_Ha-extract/07_two-tanks.acm"                  // Zeno point when simulated
          , "09_Ha-extract/08_two-two-tanks-helper.acm"       // using 2015 semantics
          ).exists(f endsWith _)
  }

}
