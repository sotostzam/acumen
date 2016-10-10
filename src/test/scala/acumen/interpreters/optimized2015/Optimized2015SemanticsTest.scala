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
          //The following examples are temporarily skipped and must be reintegrated once the fix
          //point detection is fixed in the optimized interpreter
          , "04_Experimental/04_BTA/Equational-Models/06_linearEquatioins.acm"
          , "04_Experimental/04_BTA/Equational-Models/02_discreteAlgebric.acm"
          , "03_Projects/08_Veinge/Groups 1-5 together.acm"
          , "03_Projects/08_Veinge/Group 1.acm"
          , "03_Projects/08_Veinge/Group 2.acm"
          , "03_Projects/08_Veinge/Group 3.acm"
          , "03_Projects/08_Veinge/Group 4.acm"
          , "03_Projects/08_Veinge/Group 5.acm"
          , "03_Projects/04_Yingfu_Zeng/02_3DText.acm"
          , "01_CPS_Course/01_Introduction/01_Geometry/11_orientation.acm"
          , "01_Introduction/01_Geometry/10_cone.acm"
          , "01_CPS_Course/01_Introduction/01_Geometry/09_trigonometric_functions.acm"
          , "01_CPS_Course/01_Introduction/01_Geometry/08_cylinder.acm"
          , "01_CPS_Course/01_Introduction/01_Geometry/07_coordinate_frame.acm"
          , "01_CPS_Course/01_Introduction/01_Geometry/06_more_angles.acm"
          , "01_CPS_Course/01_Introduction/01_Geometry/05_colors.acm"
          , "01_CPS_Course/01_Introduction/01_Geometry/04_box.acm"
          , "01_CPS_Course/01_Introduction/01_Geometry/03_angles in pi.acm"
          , "01_CPS_Course/01_Introduction/01_Geometry/02_text.acm"
          , "01_CPS_Course/01_Introduction/01_Geometry/01_sphere.acm"
          , "00_Demos/12_Print_Variable.acm"
          //----------------------------------------------------------------------------------
          ).exists(f endsWith _)
  }

}

