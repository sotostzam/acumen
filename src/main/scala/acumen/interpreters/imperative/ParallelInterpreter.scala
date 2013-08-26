package acumen.interpreters.imperative

import scala.math._
import scala.concurrent.SyncVar
import acumen.util.Canonical.{ classf, cmagic }
import acumen.interpreters.Common._
import acumen.interpreters.imperative.Common._

import scala.annotation.tailrec

/**
 * A scheduler is a strategy for evaluating an object into a Changeset. 
 * This strategy is realized by implementing the traverseMain method, 
 * which is called on the root object of a model by the interpreter.
 */
trait Scheduler {
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

class ParallelInterpreter(private var scheduler: Scheduler) extends ImperativeInterpreter {
  import acumen._

  override def id = ParallelInterpreter.id

  override def stepInit : Unit = scheduler.reset
  override def traverse(f: ObjId => Changeset, root: ObjId): Changeset =
    if (scheduler.nbThreads > 1) scheduler.traverseMain(f, root)
    else traverseSimple(f, root)
}

object ParallelInterpreter {
  import Common._

  var id = Array("parallel")
  def updateId(args: String*) = 
    id = Array("parallel") ++ args
  
  private val cores = Runtime.getRuntime.availableProcessors
  
  // Note: The work sharing thread pool is considered experimental and
  //       should not be enabled in a release without review -- Walid and Kevin
  private val sharingTP = new SharingThreadPool[Changeset](0, cores)
  private val sharingS = new SharingScheduler(sharingTP)
  
  private val staticTP = new StaticThreadPool[Changeset](0, cores)
  private val staticS = new StaticScheduler(staticTP)
  
  val instance = new ParallelInterpreter(staticS) 

  def sharing: ParallelInterpreter = {updateId("sharing"); sharing(cores)}
  def sharing(n: Int) = { updateId("sharing", n.toString); sharingTP.reset(n); instance.scheduler = sharingS; instance }
  
  def static: ParallelInterpreter = {updateId(); static(cores) }
  def static(n: Int) = { updateId(n.toString); staticTP.reset(n); instance.scheduler = staticS; instance } 

  def apply() = { instance.scheduler.reset(cores); instance }
  def apply(nbThreads: Int) = { instance.scheduler.reset(nbThreads); instance }
    
}
