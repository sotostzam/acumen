package acumen
package interpreters
package enclosure

import acumen.InterpreterCallbacks
import acumen.InterpreterRes
import acumen.Prog
import acumen.interpreters.Common.classDef
import acumen.interpreters.enclosure.affine.UnivariateAffineEnclosure
import acumen.interpreters.enclosure.event.tree.Solver
import acumen.interpreters.enclosure.ivp.LohnerSolver
import acumen.interpreters.enclosure.ivp.PicardSolver
import acumen.interpreters.enclosure.ivp.IVPSolver
import acumen.interpreters.enclosure.ivp.VeroSolver
import ui.interpreter.EnclosureModel
import acumen.interpreters.enclosure.event.pwl.EncloseHybrid

trait EnclosureInterpreterCallbacks extends InterpreterCallbacks {
  def log(msg: String): Unit
  var endTime: Double = 0.0
  def sendResult(data: Iterable[UnivariateAffineEnclosure]): Unit = {}
}

case class EnclosureRes(res: Seq[UnivariateAffineEnclosure]) extends InterpreterRes {
  def print = println(res)
  def printLast = println(res.last)
}

/**
 * Proxy for the enclosure-based solver.
 */
class Interpreter(override var ivpSolver: IVPSolver)
    extends acumen.RecursiveInterpreter
    with Checker
    with Extract
    with Solver
    with EncloseHybrid {

  def newInterpreterModel = new EnclosureModel

  val defaultInterpreterCallbacks = new EnclosureInterpreterCallbacks {
    override def log(msg: String) = println(msg)
  }

  val noAdjustParms = (p: Parameters) => p

  def run(des: Prog) = runInterpreter(des, defaultInterpreterCallbacks)

  def runInterpreter(des: Prog, cb0: InterpreterCallbacks) =
    runInterpreter(des, cb0, noAdjustParms)

  var localizing = true

  //FIXME do this properly
  def runInterpreter(des: Prog,
                     cb0: InterpreterCallbacks,
                     adjustParms: Parameters => Parameters) = {
    val cb = cb0.asInstanceOf[EnclosureInterpreterCallbacks]
    if (des.defs.size > 1) sys.error("Multiple classes are not currently supported by the enclosure interperter!")
    val main = classDef(ClassName("Main"), des)

    // checking that main class embeds a hybrid automaton
    checkValidAutomatonEmbedding(main)

    val ps0 = parameters(main)
    val ps = adjustParms(ps0)
    implicit val rnd = Rounding(ps.bigDecimalDigits)
    val (hs, uss) = extract(main)

    cb.endTime = ps.endTime

    val res = if (localizing) {
      encloseHybrid(
        ps,
        hs,
        ps.simulationTime,
        new StateEnclosure(StateEnclosure.emptyState(hs) + (uss.mode -> Some(uss.initialCondition))),
        cb)

    }
    else {

      solver(
        hs,
        ps.simulationTime,
        Set(uss),
        ps.initialPicardPadding,
        ps.picardImprovements,
        ps.maxPicardIterations,
        ps.splittingDegree,
        ps.maxEventTreeSize,
        ps.minTimeStep,
        ps.maxTimeStep,
        ps.minComputationImprovement,
        cb)
    }

    EnclosureRes(res)
  }

}

/** Singleton interpreter. All concrete IVP solvers are declared here */
object Interpreter extends Interpreter(null) {
  private lazy val picard = new PicardSolver {}
  private lazy val lohner = new LohnerSolver {}
  private lazy val vero = new VeroSolver {}
  ivpSolver = picard // default IVP solver

  def asPicard: Interpreter = { ivpSolver = picard; this }
  def asVero: Interpreter = { ivpSolver = vero; this }
  def asLohner: Interpreter = { ivpSolver = lohner; this }
  def asLocalizing: Interpreter = { localizing = true; this }
  def asNonLocalizing: Interpreter = { localizing = false; this }
  def toggleContraction: Unit = { ivpSolver = if (ivpSolver == lohner) picard else lohner }
} 
