package acumen
package interpreters
package enclosure

import util.Canonical._
import Types._
import ui.interpreter._
import acumen.interpreters.Common.classDef
import acumen.interpreters.enclosure.solver.Solver
import acumen.interpreters.enclosure.solver.HybridSystem

trait EnclosureInterpreterCallbacks extends InterpreterCallbacks {
  def log(msg: String) : Unit
  var endTime : Double = 0.0
  def sendResult(data: Iterable[UnivariateAffineEnclosure]) : Unit = {}
}

/**
 * Proxy for the enclosure-based solver.
 */
object Interpreter extends acumen.RecursiveInterpreter with Solver with Transform {

  def newTraceModelData = new EnclosureTraceModelData

  //FIXME do this properly
  override def runInterpreter(des: Prog, cb0: InterpreterCallbacks) {
    val cb = cb0.asInstanceOf[EnclosureInterpreterCallbacks]
    val main = classDef(ClassName("Main"), des)

    val ps = parameters(main)
    implicit val rnd = Rounding(ps.precision)
    val (hs, uss) = extract(main)

    solver(
      hs,
      ps.simulationTime,
      Set(uss),
      ps.solveVtInitialConditionPadding,
      ps.extraPicardIterations,
      ps.maxPicardIterations,
      ps.maxEventTreeSize,
      ps.minTimeStep,
      ps.maxTimeStep,
      ps.simulationTime,
      "output",
      cb)
  }

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

  val txt = """
class Main(simulator)
  private mode = "Fly"; x = 1; x' = 0; x'' = 0 end
  switch mode
    case "Fly" assume x >= 0
      if x < 0 && x' < 0
        x' = -x';
        mode = "Fly"
      end;
      x'' [=] -9.8
  end
end
"""

  val prog = Parser.run(Parser.prog, txt)
  val des = Desugarer.run(prog)
  val main = classDef(ClassName("Main"), des)
  
  val ps = parameters(main)
  implicit val rnd = Rounding(ps.precision)
  val (h, ic) = extract(main)

  println(h)
  println(ic)
  println(ps)

}

object Ticker extends App with Transform {

  val txt = """
class Main(simulator) 
private mode = 0; x = 1; x' = -1 end
  switch mode
    case 0 assume x >= 0
      x' [=] -1; 
      if x == 0
        x = 1; 
        mode = 0 
      end
  end
end
"""

  val prog = Parser.run(Parser.prog, txt)
  val des = Desugarer.run(prog)
  val main = classDef(ClassName("Main"), des)
  
  val ps = parameters(main)
  implicit val rnd = Rounding(ps.precision)
  val (h, ic) = extract(main)

  println(h)
  println(ic)
  println(ps)

}
