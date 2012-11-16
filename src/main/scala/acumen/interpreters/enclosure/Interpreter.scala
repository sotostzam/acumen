package acumen
package interpreters
package enclosure

import util.Canonical._
import Types._
import ui.interpreter._
import acumen.interpreters.Common.classDef
import acumen.interpreters.enclosure.solver._
import acumen.interpreters.enclosure.solver.HybridSystem

trait EnclosureInterpreterCallbacks extends InterpreterCallbacks {
  def log(msg: String): Unit
  var endTime: Double = 0.0
  def sendResult(data: Iterable[UnivariateAffineEnclosure]): Unit = {}
}

/**
 * Proxy for the enclosure-based solver.
 */
object Interpreter extends acumen.RecursiveInterpreter with Solver with Extract {

  def newInterpreterModel = new EnclosureModel

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
      ps.splittingDegree, 
      ps.maxEventTreeSize,
      ps.minTimeStep,
      ps.maxTimeStep,
      ps.minImprovement,
      "output",
      cb)
  }

}
