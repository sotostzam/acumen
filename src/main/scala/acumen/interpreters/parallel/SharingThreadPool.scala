package acumen.interpreters.parallel

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
 * The constructor parameter n is the number of threads in the pool.
 */
class SharingThreadPool[A](val n: Int) {
  private val pool = this
  private val threads = new Array[AcumenThread](n)
  /** Note: Mutation of this variable must be done atomically */
  @volatile var free = threads.size

  case class WorkItem(f: () => A, outbox: SyncVar[A])

  val workQueue = new LinkedBlockingQueue[WorkItem]()

  for (i <- 0 until threads.size) {
    val tr = new AcumenThread(i + 1)
    tr.start
    threads(i) = tr
  }

  def reset = workQueue.clear

  def run(f: () => A): SyncVar[A] = this.synchronized {
    val box = new SyncVar[A]
    workQueue.put(WorkItem(f, box))
    box
  }

  def dispose = pool.synchronized { for (t <- threads) t.join }

  class AcumenThread(i: Int) extends Thread("acumen thread #" + i) {
    var keepRunning = true
    override def run = while (keepRunning) {
      val WorkItem(f, outbox) = workQueue.take
      pool synchronized ( free -= 1 )
      val res = f()
      outbox.set(res)
      pool synchronized ( free += 1 )
    }
  }

}
