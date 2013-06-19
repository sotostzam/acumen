package acumen.interpreters.imperative.parallel

import scala.math._
import scala.concurrent.SyncVar
import acumen.util.Canonical.{ classf, cmagic }
import acumen.interpreters.Common._

import scala.annotation.tailrec

/**
 * A scheduler is a strategy for evaluating an object into a Changeset. 
 * This strategy is realized by implementing the traverseMain method, 
 * which is called on the root object of a model by the interpreter.
 */
trait Scheduler {
  import Interpreter._
  def traverseMain(f: ObjId => Changeset, root: ObjId): Changeset
  val threadPool: ThreadPool[Changeset]
  def nbThreads = threadPool.nbThreads  
  def reset: Unit = threadPool.reset
  def reset(n: Int): Unit = { threadPool.reset(n) }
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
  def reset: Unit = reset(nbThreads)
  def reset(n: Int): Unit
}

class Interpreter(private var scheduler: Scheduler) extends acumen.CStoreInterpreter {
  val I = acumen.interpreters.imperative.parallel.Interpreter
  import I._
  import acumen._
  
  type Store = I.Store
  def init(prog: Prog, opts: CStoreOpts) = {I.init(prog, opts)}
  def fromCStore(st: CStore, root: CId) = I.fromCStore(st, root)
  def repr(st: Store) = I.repr(st)

  def step(p: Prog, st: Store): Option[Store] = {
    val magic = getSimulator(st)
    if (getTime(magic) > getEndTime(magic)) {
      None
    } else {
      @tailrec def step0() : Unit = {
        scheduler.reset
        if (getTime(magic) > getEndTime(magic))
          return
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
                if (!I.cstoreOpts.outputSomeDiscrete) return step0()
              case NoChange() =>
                setStepType(magic, Continuous())
                if (!I.cstoreOpts.outputAllRows) return step0()
            }
          case Continuous() =>
            setStepType(magic, Discrete())
            setTime(magic, getTime(magic) + getTimeStep(magic))
            if (I.cstoreOpts.outputLastOnly) return step0()
        }
      }
      step0()
      Some(st)
    }
  }

}

object Interpreter extends acumen.interpreters.imperative.Common {
  
  private val cores = Runtime.getRuntime.availableProcessors
  
  private val sharingTP = new SharingThreadPool[Changeset](0, cores)
  private val sharingS = new SharingScheduler(sharingTP)
  
  private val staticTP = new StaticThreadPool[Changeset](0, cores)
  private val staticS = new StaticScheduler(staticTP)
  
  val instance = new Interpreter(staticS) 

  def sharing: Interpreter = sharing(cores) 
  def sharing(n: Int) = { sharingTP.reset(n); instance.scheduler = sharingS; instance }
  
  def static: Interpreter = static(cores)
  def static(n: Int) = { staticTP.reset(n); instance.scheduler = staticS; instance } 

  def apply() = { instance.scheduler.reset(cores); instance }
  def apply(nbThreads: Int) = { instance.scheduler.reset(nbThreads); instance }
    
}
