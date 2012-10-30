package acumen

import acumen.interpreters.Common._
import acumen.interpreters.enclosure.Types._
import acumen.interpreters.enclosure.Extract
import acumen.interpreters.enclosure.Rounding
import acumen.interpreters.enclosure.Interval
import acumen.interpreters.enclosure.Interval._
import acumen.interpreters.enclosure.Types._
import acumen.interpreters.enclosure.solver._
import acumen.interpreters.enclosure.solver.Solver._
import acumen.interpreters.enclosure.UnivariateAffineScalarEnclosure
import acumen.interpreters.enclosure.UnivariateAffineEnclosure
import TransformTest._
import acumen.ui.EnclosureTraceModel
import acumen.ui.PlotEnclosure
import java.awt.Color

object Sandbox extends App with Extract with ReusingSolver {

  val bbrf = """
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
  
  val convergent = """
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
  
  val mik1 = """
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
        r = [0:0.25]*r;
        mode = "Fly";
      end;
      x'' [=] -10;
      r'  [=] 0;
  end
end
"""

  val mik2 = """
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

  val tic = """ 
class Main (simulator)
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

  val bb = """
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
class Main(simulator)
  private 
    mode = "Fly"; 
    x = 5; x' = 0; x'' = 0;  
    r = [0:100]; r' = 0;
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
class Main(simulator)
  private 
    mode = "Fly"; 
    x = 5; x' = 0; x'' = 0;  
    r = [0:100]; r' = 0;
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
        r = [0:0.25]*r;
        mode = "Fly";
      end;
      x'' [=] -10;
      r'  [=] 0;
  end
end
"""

  val prog = Parser.run(Parser.prog, convergent)
  val des = Desugarer.run(prog)
  val main = classDef(ClassName("Main"), des)

  val ps = parameters(main)
  implicit val rnd = Rounding(ps.precision)
  val (h, us) = extract(main)

  val start = System.currentTimeMillis
  val res = solver(
    h,
    ps.simulationTime,
    Set(us),
    ps.solveVtInitialConditionPadding,
    ps.extraPicardIterations,
    ps.maxPicardIterations,
    ps.maxEventTreeSize,
    ps.minTimeStep,
    ps.maxTimeStep,
    ps.minImprovement,
    "output",
    defaultCallback)
  val end = System.currentTimeMillis
  val time = end - start
  println("computed " + res.size + " enclosures in " + time / 1000.0 + " seconds")
  UnivariateAffineScalarEnclosure.plot(
    res.map(e => (Color.BLUE, e("x"))) ++ res.map(e => (new Color(0, 127, 0), e("x'"))) ++ res.map(e => (Color.RED, e("r"))): _*)

}
