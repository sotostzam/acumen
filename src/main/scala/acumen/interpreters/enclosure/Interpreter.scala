package acumen
package interpreters
package enclosure

import acumen.InterpreterCallbacks
import acumen.InterpreterRes
import acumen.Prog
import acumen.interpreters.Common.classDef
import acumen.interpreters.enclosure.affine.UnivariateAffineEnclosure
import acumen.interpreters.enclosure.event.EventEncloser
import acumen.interpreters.enclosure.event.pwl.PWLEventEncloser
import acumen.interpreters.enclosure.event.tree.Solver
import acumen.interpreters.enclosure.ivp.IVPSolver
import acumen.interpreters.enclosure.ivp.LohnerSolver
import acumen.interpreters.enclosure.ivp.PicardSolver
import acumen.interpreters.enclosure.ivp.VeroSolver
import acumen.interpreters.enclosure.strategy.LocalizingStrategy
import acumen.interpreters.enclosure.strategy.Strategy
import ui.interpreter.EnclosureModel
import acumen.interpreters.enclosure.event.tree.TreeEventEncloser

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
class Interpreter
    extends acumen.RecursiveInterpreter
    with Checker
    with Extract {

  def newInterpreterModel = new EnclosureModel

  val defaultInterpreterCallbacks = new EnclosureInterpreterCallbacks {
    override def log(msg: String) = println(msg)
  }

  val noAdjustParms = (p: Parameters) => p

  def run(des: Prog) = runInterpreter(des, defaultInterpreterCallbacks)

  def runInterpreter(des: Prog, cb0: InterpreterCallbacks) =
    runInterpreter(des, cb0, noAdjustParms)

  //FIXME do this properly
  def runInterpreter(des: Prog,
                     cb0: InterpreterCallbacks,
                     adjustParms: Parameters => Parameters) = {
    val cb = cb0.asInstanceOf[EnclosureInterpreterCallbacks]
    if (des.defs.size > 1) sys.error("Multiple classes are not currently supported by the enclosure interperter!")
    val cdes = CleanParameters.run(des, EnclosureInterpreterType)
    val main = classDef(ClassName("Main"), cdes)

    // checking that main class embeds a hybrid automaton
    checkValidAutomatonEmbedding(main)

    val ps0 = parameters(main)
    val ps = adjustParms(ps0)
    implicit val rnd = Rounding(ps.bigDecimalDigits)
    val (hs, uss) = extract(main)

    cb.endTime = ps.endTime

    EnclosureRes(Interpreter.strategy.enclosePiecewise(
      ps,
      hs,
      ps.simulationTime,
      new StateEnclosure(StateEnclosure.emptyState(hs) + (uss.mode -> Some(uss.initialCondition))),
      cb))
  }

}

/** Singleton interpreter. All concrete IVP solvers are declared here */
object Interpreter extends Interpreter {

  // IVP solvers
  private val picard = new PicardSolver {}
  private val vero = new VeroSolver {}
  private val lohner = new LohnerSolver {}

  var strategy = new LocalizingStrategy(new PWLEventEncloser(new PicardSolver {}))

  /** Sets the IVP solver to PicardSolver */
  def asPicard() = { strategy.eventEncloser.ivpSolver = picard; this }
  /** Sets the IVP solver to VeroSolver */
  def asVero() = { strategy.eventEncloser.ivpSolver = vero; this }
  /** Sets the IVP solver to LohnerSolver */
  def asLohner() = { strategy.eventEncloser.ivpSolver = lohner; this }

  /** Sets the event handler to PWL */
  def asPWL() = { strategy.eventEncloser = new PWLEventEncloser(strategy.eventEncloser.ivpSolver); this }
  /** Sets the event handler to EVT */
  def asEVT() = { strategy.eventEncloser = new TreeEventEncloser(strategy.eventEncloser.ivpSolver); this }

  /** Sets the strategy LocalizingStrategy */
  def asLocalizing() = { strategy = new LocalizingStrategy(strategy.eventEncloser); this }
  /** Sets the strategy to the non-localizing SimpleRecursiveStrategy */
  //FIXME Jan: Replace with non-localizing strategy
  def asNonLocalizing() = { strategy = new LocalizingStrategy(strategy.eventEncloser); this }

  /** Toggles between PicardSolver and LohnerSolver IVP solvers */
  def toggleContraction(): Unit = {
    strategy.eventEncloser.ivpSolver =
      if (strategy.eventEncloser.ivpSolver == lohner) picard else lohner
  }

}
