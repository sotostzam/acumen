//package acumen.interpreters.imperative
//
//abstract class Interpreter extends acumen.CStoreInterpreter {
//  val I = acumen.interpreters.imperative.Interpreter
//  import I._
//  import acumen._
//
//  type Store = I.Store
//  def init(prog: Prog) = I.init(prog)
//  def fromCStore(st: CStore, root: CId) = I.fromCStore(st, root)
//  def repr(st: Store) = I.repr(st)
//
//  def step(p: Prog, st: Store, t: ObjId => Changeset): Option[Store] = {
//    val magic = getSimulator(st)
//    if (getTime(magic) > getEndTime(magic)) None
//    else Some {
//      val chtset = t(st)
//      getNextStepType(magic) match {
//        case Discrete() =>
//          chtset match {
//            case SomeChange => // Continue fixpoint computation
//            case NoChange => setNextStepType(magic, Continuous())
//          }
//        case Continuous() =>
//          setNextStepType(magic, Discrete())
//          setTime(magic, getTime(magic) + getTimeStep(magic))
//      }
//      st
//    }
//  }
//
//}
//
//object Interpreter extends acumen.interpreters.imperative.Common {
//
//}
