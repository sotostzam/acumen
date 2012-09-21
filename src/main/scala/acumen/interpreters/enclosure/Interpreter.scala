package acumen
package interpreters
package enclosure

import util.Canonical._
import Types._
import ui.TraceModel
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
  override def generateTraceModel(text: String, log: String => Unit): EnclosureTraceModel = {
    val prog = Parser.run(Parser.prog, text)
    val (h: HybridSystem, uis) = extract(prog.defs(0))
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
    val res = solver(H, T, Ss, delta, m, n, K, d, e, T, "output",log)
    new EnclosureTraceModel(res)
  }

  type Store = Seq[UnivariateAffineEnclosure]

  val emptyStore: CStore = Map.empty

  //TODO Implement "repr" for enclosure interpreter
  def repr(s: Store): CStore = emptyStore

  //TODO Implement "fromCStore" for enclosure interpreter
  def fromCStore(cs: CStore, root: CId): Store = null

  //TODO Implement "init" for enclosure interpreter
  def init(prog: Prog): (Prog, Store) = (prog, null)

  //TODO Implement "step" for enclosure interpreter
  def step(p: Prog, st: Store): Option[Store] = Some(null)

  // Simulator object
  def magicClassTxt =
    """
    class Simulator(
    time, timeStep, endTime, stepType, lastCreatedId,
    minTimeStep, maxTimeStep 
    ) end
    """
  def initStoreTxt =
    """#0.0 { className = Simulator, parent = #0, time = 0.0, timeStep = 0.01, 
              endTime = 10.0, stepType = @Discrete, nextChild = 0,
						  seed1 = 0, seed2 = 0 }"""

  lazy val magicClass = Parser.run(Parser.classDef, magicClassTxt)
  lazy val initStore = Parser.run(Parser.store, initStoreTxt)

}











