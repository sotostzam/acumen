package acumen.interpreters.optimized2015

import acumen._
import acumen.Errors._
import acumen.interpreters.semanticstest._

class Optimized2015SemanticsTest extends Traditional2015Tests {

  override def semantics = SemanticsImpl.Opt2015

  override def examplesSkip(f: String) = {
    super.examplesSkip(f) ||
      List( "03_Projects/02_Jawad/02_ADAS_RTL_Rear_END.acm"
          , "02_Jawad/04_ADAS_REAR_END_novisual.acm"
          , "09_Ha-extract/05_two-tanks-extracted-w-else.acm" // These are Zeno models,
          , "09_Ha-extract/06_two-tanks-extracted.acm"        // and will stall at t = 2.0 
          , "09_Ha-extract/07_two-tanks.acm"                  // Zeno point when simulated
          , "09_Ha-extract/08_two-two-tanks-helper.acm"       // using 2015 semantics
          //The following examples are not compatible anymore with the new intervals semantics
          , "04_Experimental/02_Robust_Simulation/07_BB_OC_validated_w_mutiple_uncertainties.acm"
          , "05_Language_Research/16_Desca/bouncing-ball-hypothesis.acm"
          //----------------------------------------------------------------------------------
          ).exists(f endsWith _)
  }

}

