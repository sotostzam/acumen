package acumen.interpreters.imperative.parallel

import java.util.concurrent.LinkedBlockingQueue

import scala.concurrent.SyncVar

/**
 * A work sharing thread pool. Work is scheduled by passing a function to the run
 * method. The function returns a SyncVar which is used to communicate results back
 * to the caller of run. The function that was passed to run is put on a queue,
 * from which worker threads take work items. When a thread completes its work, it
 * writes it to the SyncVar.
 *
 * SharingThreadPool is parametric in a type variable A, this is the result type of
 * the functions that are passed to run and must be the same over all calls to run
 * for a specific instance of SharingThreadPool.
 *
 * The constructor parameter nbThreads is the number of threads in the pool.
 */
class SharingThreadPool[A](@volatile private var active: Int, val total: Int) extends ThreadPool[A] {

  sealed abstract class WorkItem
  case class Compute(f: () => A, outbox: SyncVar[A]) extends WorkItem
  case object Sleep extends WorkItem
  case object Die extends WorkItem
  
  val workQueue = new LinkedBlockingQueue[WorkItem]()

  private val pool = this
  private var threads = new Array[SharingAcumenThread](total)
  /** Note: Mutation of this variable must be done atomically */
  @volatile var free = active
  
  for (i <- 0 until total) {
    val tr = new SharingAcumenThread(i + 1)
    tr.start
    threads(i) = tr
  }
  
  /** Increment the count of available threads */
  private def release = pool.synchronized { free += 1 }
  /** Decrement the count of available threads */
  private def reserve = pool.synchronized { free -= 1 }

  def nbThreads = active

  def reset(n: Int) = {
    active = n
    workQueue.clear
    while(threads.exists(_.awake)) workQueue put Sleep
    workQueue.clear
    for (i <- 0 until active) threads(i).wake
  }

  def run(f: () => A): SyncVar[A] = this.synchronized {
    val box = new SyncVar[A]
    workQueue.put(Compute(f, box))
    box
  }

  class SharingAcumenThread(i: Int) extends Thread("acumen sharing thread #" + i) {
    /** Make thread listen to messages on the workQueue. */
    def wake = waker set true
    setDaemon(true)
    @volatile var alive = true
    @volatile var awake = false
    private val waker = new SyncVar[Boolean]
    override def run = {
      while (alive) {
        awake = waker.take
        release
        while (awake) {
          workQueue.take match {
            case Compute(f, outbox) =>
              reserve
              outbox.set(f())
              release
            case Sleep =>
              awake = false
              reserve
            case Die =>
              alive = false
          }
        }
      }
    }
  }

}
