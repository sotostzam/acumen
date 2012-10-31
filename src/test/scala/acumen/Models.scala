package acumen

object Models {

  val kevin_test = """
class Main(simulator)
private x = 1 +/- 0.01; x' = -1; mode = 0 end
  simulator.maxTimeStep = 0.1;
  switch mode 
    case 0 assume x >= 0 
      if x == 0 
        x = 1 +/- 0.01;
        mode = 0;
      end;
      x' [=] -1;
  end
end
"""
  
  val bouncing_ball_floorup = """
////////////////////////////////////////////////////
// This file is called bouncing_ball_floorup.acm  //
////////////////////////////////////////////////////
class Main(simulator)
  private mode = "Fly"; x = 5; x' = 0; x'' = 0; y = 0; y'= 0; y'' = 0 end
  simulator.endTime = 3.5;
  simulator.minTimeStep = 0.01;
  simulator.maxTimeStep = 0.1;
  switch mode
    case "Fly" assume x >= y
      if x == y && x' <= y'
        x' = y'-0.5 * (x'-y');
        mode = "Fly"
      end;
      x'' [=] -10; y'' [=] 1
  end
end
"""
  
  val two_tanks_sum = """
////////////////////////////////////////////////
// This file is called two_tanks_sum.acm      //
// It implements the S2T model from the paper.//
////////////////////////////////////////////////
class Main(simulator) 
  private 
    mode = "Fill1"; 
    x1 = 1; x1' = 2;  
    x2 = 1; x2' = -3;
    x12 = 2; x12' = -1;
  end
  simulator.endTime = 2.5;
  simulator.minTimeStep = 0.001;
  simulator.maxTimeStep = 0.1;
  simulator.minImprovement = 0.1;
switch mode
    case "Fill1" assume x1 >= 0 && x2 >= 0 && x12 >= 0 && x12 == x1 + x2
      if x2 == 0 mode = "Fill2" end; 
      x1'  [=] 2; 
      x2'  [=] -3;
      x12' [=] -1;
    case "Fill2" assume x1 >= 0 && x2 >= 0 && x12 >= 0 && x12 == x1 + x2
      if x1 == 0 mode = "Fill1" end; 
      x1'  [=] -2; 
      x2'  [=] 1;
      x12' [=] -1;
  end
end
"""

  val two_tanks = """
////////////////////////////////////////////////
// This file is called two_tanks.acm          //
// It implements the S2T model from the paper.//
////////////////////////////////////////////////
class Main(simulator) 
  private 
    mode = "Fill1"; 
    x1 = 1; x1' = 2;  
    x2 = 1; x2' = -3;
  end
  simulator.endTime = 2.5;
  simulator.minTimeStep = 0.001;
  simulator.maxTimeStep = 0.1;
  switch mode
    case "Fill1" assume x1 >= 0 && x2 >= 0 
      if x2 == 0 mode = "Fill2" end; 
      x1' [=] 2; 
      x2' [=] -3;
    case "Fill2" assume x1 >= 0 && x2 >= 0 
      if x1 == 0 mode = "Fill1" end; 
      x1' [=] -2; 
      x2' [=] 1;
  end
end"""

  val bouncing_ball_air = """
////////////////////////////////////////////////
// This file is called bouncing_ball_air.acm  //
////////////////////////////////////////////////
class Main(simulator)
  private mode = "Fall"; x = 5; x' = 0; x'' = 0 end
  simulator.endTime = 3.5;
  simulator.minTimeStep = 0.01;
  simulator.maxTimeStep = 0.2;
  switch mode
    case "Rise" assume x >= 0 && x' >= 0
      if x' == 0
        mode = "Fall"
      end;
      x'' [=] -10 - 0.1*x'*x'
    case "Fall" assume x >= 0 && x' <= 0
      if x == 0
        x' = -0.5 * x';
        mode = "Rise"
      end;
      x'' [=] -10 + 0.1*x'*x'
  end
end
"""

  val bouncing_ball_risefall_explicit_energy_equality = """
/////////////////////////////////////////////////////////////////////////////
// This file is called bouncing_ball_risefall_explicit_energy_equality.acm //
/////////////////////////////////////////////////////////////////////////////
class Main(simulator)
  private 
    mode = "Fall"; 
    x = 5; x' = 0; x'' = 0;  
    r = 100; r' = 0;
  end
  simulator.endTime = 3.5;
  simulator.minTimeStep = 0.1;
  simulator.maxTimeStep = 0.2;
  switch mode
    case "Rise" assume x >= 0 && x' >= 0 && 0 <= r && r == x'*x' + 20*x
      if x' == 0
        mode = "Fall";
      end;
      x'' [=] -10;
      r' [=] 0;
    case "Fall" assume x >= 0 && x' <= 0 && 0 <= r && r == x'*x' + 20*x
      if x == 0
        x' = -0.5 * x';
        r = [0:0.25]*r;
        mode = "Rise";
      end;
      x'' [=] -10;
      r' [=] 0;
  end
end
"""

  val bouncing_ball_explicit_energy_convergent = """
//////////////////////////////////////////////////////////////////////
// This file is called bouncing_ball_explicit_energy_convergent.acm //
// It implements a version of the EBB model from the paper.         //
//////////////////////////////////////////////////////////////////////
class Main(simulator)
  private 
    mode = "Fly"; 
    x = 5; x' = 0; x'' = 0;  
    r = 100; r' = 0;
  end
  simulator.endTime = 3.5;
  simulator.minTimeStep = 0.0001;
  simulator.maxTimeStep = 0.1;
  switch mode
    case "Fly" 
    assume x >= 0 && r == x'*x' + 20*x
      if x == 0 && x' <= 0
        x' = -0.5*x';
        r = [0:0.25]*r;
        mode = "Fly";
      end;
      x'' [=] -10;
      r'  [=] 0;
  end
end
"""

  val bouncing_ball_explicit_energy_mik1 = """
////////////////////////////////////////////////////////////////
// This file is called bouncing_ball_explicit_energy_mik1.acm //
// It implements the EBB model from the paper.                //
////////////////////////////////////////////////////////////////
class Main(simulator)
  private 
    mode = "Fly"; 
    x = 5; x' = 0; x'' = 0;  
    r = 100; r' = 0;
  end
  simulator.endTime = 3.5;
  simulator.minTimeStep = 0.01;
  switch mode
    case "Fly"
    assume x >= 0 && 0 <= r && r == x'*x' + 20*x
      if x == 0 && x' <= 0
        x' = -0.5*x';
        r = (0.125 +/- 0.125)*r;
    mode = "Fly";
      end;
      x'' [=] -10;
      r'  [=] 0;
  end
end
"""

  val bouncing_ball_explicit_energy_mik2 = """
////////////////////////////////////////////////////////////////
// This file is called bouncing_ball_explicit_energy_mik2.acm //
// It implements the EBB model from the paper.                //
////////////////////////////////////////////////////////////////
class Main(simulator)
  private 
    mode = "Fly"; 
    x1 = 5; x1' = 0; x1'' = 0;  
    r1 = 100; r1' = 0;
  end
  simulator.endTime = 3.5;
  simulator.minTimeStep = 0.5;
  simulator.maxTimeStep = 1;
  switch mode
    case "Fly"
    assume x1 >= 0 && 0 <= r1 && r1 == x1'*x1' + 20*x1
      if x1 == 0 && x1' <= 0
        x1' = -0.5*x1';
        r1 = 0.25*r1;
        mode = "Fly";
      end;
      x1'' [=] -10;
      r1'  [=] 0;
  end
end
"""

  val ticker = """ 
////////////////////////////////////
// This file is called ticker.acm //
////////////////////////////////////
cclass Main (simulator)
 private x = [1:1]; x' = -1; mode = "decreasing" end
 simulator.endTime = 2.5;
 simulator.minTimeStep = 0.1;
 simulator.maxTimeStep = 0.25;
 switch mode
   case "decreasing" assume x >= 0
     if x == 0 
       x = 1;
       mode = "decreasing"
     end;
     x' [=] -1;
 end
end
"""

  val tictoc = """ 
////////////////////////////////////
// This file is called tictoc.acm //
////////////////////////////////////
class Main (simulator)
 private x = 1; x' = -1; mode = "decreasing" end
 simulator.endTime = 1.5;
 simulator.minTimeStep = 0.1;
 simulator.maxTimeStep = 0.5;
 simulator.minImprovement = 0.001;
 switch mode
   case "decreasing" assume x <= 1
     if x == 0 
       mode = "increasing"
     end;
     x' [=] -1;
   case "increasing" assume x >= 0
     if x == 1 
       mode = "decreasing"
     end;
     x' [=] 1;
 end
end
"""

  val radiator = """ 
//////////////////////////////////////
// This file is called radiator.acm //
//////////////////////////////////////
class Main (simulator)
 private x = 22; x' = 0; mode = "on" end
 simulator.endTime = 1;
 simulator.minTimeStep = 0.0001;
 simulator.maxTimeStep = 0.1;
 simulator.minImprovement = 0.001;
 switch mode
   case "on" assume x <= 25
     if x >= 25
       mode = "off"
     end;
     x' [=] 100 - x;
   case "off" assume x >= 19
     if x <= 19
       mode = "on"
     end;
     x' [=] - x;
 end
end
"""

  val bouncing_ball = """
///////////////////////////////////////////
// This file is called bouncing_ball.acm //
///////////////////////////////////////////
class Main(simulator)
  private 
    mode = "Fly"; 
    x = 5; x' = 0; x'' = 0;  
  end
  simulator.endTime = 3.5;
  simulator.minTimeStep = 4.7e-7;
  switch mode
    case "Fly" 
    assume x >= 0 
      if x == 0 && x' <= 0
        x' = -0.5*x';
        mode = "Fly";
      end;
      x'' [=] -10;
  end
end
"""

  val ebb = """
///////////////////////////////////////////////////////////
// This file is called bouncing_ball_explicit_energy.acm //
// It implements the EBB model from the paper.           //
///////////////////////////////////////////////////////////
class Main(simulator)
  private 
    mode = "Fly"; 
    x = 5; x' = 0; x'' = 0;  
    r = [0..100]; r' = 0;
  end
  simulator.endTime = 3.5;
  simulator.minTimeStep = 4.7e-7;
  switch mode
    case "Fly" 
    assume x >= 0 && 0 <= r && r <= x'*x' + 20*x
      if x == 0 && x' <= 0
        x' = -0.5*x';
        r  = 0.25*r;
        mode = "Fly";
      end;
      x'' [=] -10;
      r'  [=] 0;
  end
end
"""

  val ebb2 = """
//////////////////////////////////////////////////////////////
// This file is called bouncing_ball_explicit_energy_2.acm  //
// It implements a version of the EBB model from the paper. //
//////////////////////////////////////////////////////////////
class Main(simulator)
  private 
    mode = "Fly"; 
    x = 5; x' = 0; x'' = 0;  
    r = [0..100]; r' = 0;
  end
  simulator.endTime = 3.5;
  simulator.minTimeStep = 4.7e-7;
  simulator.maxTimeStep = 1;
  switch mode
    case "Fly" 
    assume 0 <= x && 0 <= r && 
           abs(x') == sqrt(r-20*x) && 
           x == (r-x'*x')/20
      if x == 0 && x' <= 0
        x' = -0.5*x';
        r = [0..0.25]*r;
        mode = "Fly";
      end;
      x'' [=] -10;
      r'  [=] 0;
  end
end
"""

}