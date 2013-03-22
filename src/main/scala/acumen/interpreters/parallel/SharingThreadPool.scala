package acumen.interpreters.parallel
import scala.concurrent.SyncVar
import acumen.Errors._
import java.util.concurrent.LinkedBlockingQueue

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
class SharingThreadPool[A](val n:Int) {
  private val threads = new Array[AcumenThread](n)
  
  private val workQueue = 
    new LinkedBlockingQueue[Tuple2[() => A, SyncVar[A]]]()

  for (i <- 0 until n) {
    val tr = new AcumenThread(i+1)
    tr.start
    threads(i) = tr
  }

  def reset = workQueue.clear

  def run(f: ()=>A) : SyncVar[A] = synchronized {
	val box = new SyncVar[A]
    workQueue.put((f,box))
    box
  }

  def dispose : Unit = synchronized {
    for (t <- threads) t.join
  }

  class AcumenThread(i: Int) extends Thread("acumen thread #" + i) {
    setDaemon(true)
    var keepRunning = true 
    override def run = while (keepRunning) 
      workQueue.take match { case (f, outbox) => outbox.set(f()) }
  }
  
  /** Map mapOp over xs and fold redOp over the result, beginning with zero. */
  def mapReduce[I](
    xs: Traversable[I],
    mapOp: I => A,
    zero: A,
    redOp: (A, A) => A): A = {
    val boxes = xs map { x: I => run(() => mapOp(x)) }
    var res = zero
    for (b <- boxes) res = redOp(res, b.get)
    res
  }
  
}
