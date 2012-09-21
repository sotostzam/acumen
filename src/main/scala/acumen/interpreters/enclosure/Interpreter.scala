package acumen
package interpreters
package enclosure

import util.Canonical._
import Types._
import ui.TraceModel
import acumen.interpreters.Common.classDef
import acumen.interpreters.enclosure.solver.Solver
import acumen.interpreters.enclosure.solver.HybridSystem
import acumen.ui.EnclosureTraceModel

/**
 * Proxy for the enclosure-based solver.
 */
object Interpreter extends acumen.Interpreter with Solver with Transform {

  // FIXME do not use null
  def newTraceModel = new EnclosureTraceModel(null)

  //TODO Get this from the Simulator object
  implicit val rnd = Rounding(10)

  //FIXME do this properly
  override def generateTraceModel(text: String): EnclosureTraceModel = {
    val prog = Parser.run(Parser.prog, text)
    val (h: HybridSystem, uis) = extract(classDef(ClassName("Main"), prog))
    val H = h
    val Ss = Set(uis)

    val startTime = 0 // parameter 
    val endTime = 2.5 // parameter 
    val T = Interval(startTime, endTime)
    val delta = 0 // parameter
    val m = 20 // parameter
    val n = 200 // parameter
    val K = 30 // parameter
    val d = 0.01 // parameter
    val e = // parameter 
      T.width match { case Interval(_, hi) => hi.doubleValue / 2 }
    val res = solver(H, T, Ss, delta, m, n, K, d, e, T, "output")
    new EnclosureTraceModel(res)
  }

  type Store = Seq[UnivariateAffineEnclosure]

  val emptyStore: CStore = Map.empty

  def repr(s: Store): CStore = emptyStore

  def fromCStore(cs: CStore, root: CId): Store = null

  def init(prog: Prog): (Prog, Store) = (prog, null)

  def step(p: Prog, st: Store): Option[Store] = Some(null)

  // Simulator object
  def magicClassTxt =
    """class Simulator(time, timeStep, endTime, stepType, lastCreatedId) end"""
  def initStoreTxt =
    """#0.0 { className = Simulator, parent = #0, time = 0.0, timeStep = 0.01, 
              endTime = 10.0, stepType = @Discrete, nextChild = 0,
						  seed1 = 0, seed2 = 0 }"""

  lazy val magicClass = Parser.run(Parser.classDef, magicClassTxt)
  lazy val initStore = Parser.run(Parser.store, initStoreTxt)

}

object BBA extends App with Transform {

  val des =
    """
class Main(simulator)
  private mode = ("Fly"); x = (1); x' = (0); x'' = (0) end
  switch ((self).mode)
    case "Fly" assert (((x) >= (0)))
      if ((((((self).x) < (0))) && ((((self).x') < (0)))))
        ((self).x') = (-(((self).x')));
        ((self).mode) = ("Fly")
      else 
      end;
      ((self).x'') =[t] (-9.8);
      ((self).x') =[i] ((self).x'');
      ((self).x) =[i] ((self).x')
  end
end
"""

  val prog = Parser.run(Parser.prog,des)
  
  println(prog)
  
  implicit val rnd = Rounding(10)
  
  val (h,ic) = extract(classDef(ClassName("Main"),prog))
    
  println(h)

}

object Ticker extends App with Transform {

  val des = """
class Main(simulator)
  private mode = (0); x = (1); x' = (-1) end
  switch ((self).mode)
    case 0 assert (((x) >= (0)))
      ((self).x') =[t] (-1);
      ((self).x) =[i] ((self).x');
      if ((((self).x) <= (0)))
        ((self).x) = (1); ((self).mode) = (0)
      else 
      end
  end
end 
"""

  val prog = Parser.run(Parser.prog,des)
  
  println(prog)
  
  implicit val rnd = Rounding(10)
  
  val (h,ic) = extract(classDef(ClassName("Main"),prog))
    
  println(h)

}
