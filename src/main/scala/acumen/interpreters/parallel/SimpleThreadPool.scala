package acumen.interpreters.parallel
import scala.concurrent.SyncVar
import acumen.Errors._

class SimpleThreadPool[A](val n:Int) {
  private val threads = new Array[AcumenThread](n)
  private var next = 0

  for (i <- 0 until n) {
    val tr = new AcumenThread(i+1)
    tr.start
    threads(i) = tr
  }

  def reset = { next = 0 }

  def run(f: ()=>A) : SyncVar[A] = synchronized {
    if (next >= n) 
      throw ShouldNeverHappen()
    else {
      val box = new SyncVar[A]
      val tr = threads(next)
      threads(next).inbox.set(Some((f,box)))
      next += 1
      box
    }
  }

  def dispose : Unit = synchronized {
    for (t <- threads) {
      t.inbox.set(None)
      t.join
    }
  }

  class AcumenThread(i:Int) extends Thread("acumen thread #"+i) {
    setDaemon(true)
    val inbox = new SyncVar[Option[Tuple2[() => A, SyncVar[A]]]]
    override def run = {
      var done = false 
      while (!done) {
        inbox.take match {
          case Some((f,outbox)) =>
            outbox.set(f())
          case None =>
            done = true
        }
      }
    }
  }
}
