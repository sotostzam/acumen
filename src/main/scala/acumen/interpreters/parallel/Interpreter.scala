package acumen
package interpreters
package parallel

import scala.math._
import scala.concurrent.SyncVar

import util.Canonical.{ classf, cmagic }
import Common._
import Interpreter._

/**
 * A scheduler is a strategy for evaluating an object into a Changeset. 
 * This strategy is realized by implementing the traverseMain method, 
 * which is called on the root object of a model by the interpreter.
 */
trait Scheduler {
  def traverseMain(f: ObjId => Changeset, root: ObjId): Changeset
  val threadPool: ThreadPool[Changeset]
  def nbThreads = threadPool.nbThreads  
  def dispose = threadPool.dispose
  def reset = threadPool.reset
}

/**
 * A thread pool makes it possible to asynchronously evaluate a closure
 * f by calling the run method. Instead of returning the value of f, the 
 * run method returns a SyncVar (c.f. a future), which provides a get 
 * function to which a blocking call can be issued at a later stage, 
 * allowing execution to continue in the thread that called run. 
 */
trait ThreadPool[A] {
  def run(f: () => A): SyncVar[A]
  def nbThreads: Int
  def dispose: Unit
  def reset: Unit
}

class Interpreter(var scheduler: Scheduler) extends CStoreInterpreter {

  type Store = Interpreter.Store
  def dispose = scheduler.dispose

  def init(prog: Prog) = Interpreter.init(prog)
  def fromCStore(st: CStore, root: CId) = Interpreter.fromCStore(st, root)
  def repr(st: Store) = Interpreter.repr(st)
  
  def step(p: Prog, st: Store): Option[Store] = {
    scheduler.reset
    val magic = getSimulator(st)
    if (getTime(magic) > getEndTime(magic)) None
    else Some {
      val chtset =
        if (scheduler.nbThreads > 1) scheduler.traverseMain(evalStep(p, magic), st)
        else traverseSimple(evalStep(p, magic), st)
      getStepType(magic) match {
        case Discrete() =>
          chtset match {
            case SomeChange(dead, rps) =>
              for ((o, p) <- rps)
                changeParent(o, p)
              for (o <- dead) {
                o.parent match {
                  case None => ()
                  case Some(op) =>
                    for (oc <- o.children) changeParent(oc, op)
                    op.children = op.children diff Seq(o)
                }
              }
            case NoChange() =>
              setStepType(magic, Continuous())
          }
        case Continuous() =>
          setStepType(magic, Discrete())
          setTime(magic, getTime(magic) + getTimeStep(magic))
      }
      st
    }
  }

}

object Interpreter extends Common {

  def staticInterpreter(nbThreads: Int) =
	new Interpreter(new StaticScheduler(new StaticThreadPool[Changeset](nbThreads)))

  def sharingInterpreter(nbThreads: Int) =
	new Interpreter(new SharingScheduler(new SharingThreadPool[Changeset](nbThreads)))

  def apply(nbThreads: Int) = sharingInterpreter(nbThreads)

  def withInterpreter[A](nbThreads: Int)(f: Interpreter => A): A = {
    val pi = Interpreter(nbThreads)
    try { f(pi) } finally { pi.dispose }
  }

}
