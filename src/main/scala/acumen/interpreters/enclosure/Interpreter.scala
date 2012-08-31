package acumen
package interpreters
package enclosure

import util.Canonical._
import Types._

/**
 * Proxy for the enclosure-based solver.
 */
object Interpreter extends acumen.Interpreter {

  type Store = AffineEnclosure

  implicit val rnd = Rounding(10) //TODO Get this from the Simulator object

  val emptyStore: CStore = Map.empty

  def repr(s: Store): CStore = emptyStore //TODO Implement "repr" for enclosure interpreter

  def fromCStore(cs: CStore, root: CId): Store = //TODO Implement "fromCStore" for enclosure interpreter
    AffineEnclosure(Box.empty, Box.empty)

  def init(prog: Prog): (Prog, Store) = //TODO Implement "init" for enclosure interpreter
    (prog, AffineEnclosure(Box.empty, Box.empty))
    
  def step(p: Prog, st: Store): Option[Store] = //TODO Implement "step" for enclosure interpreter
    Some(AffineEnclosure(Box.empty, Box.empty))

}