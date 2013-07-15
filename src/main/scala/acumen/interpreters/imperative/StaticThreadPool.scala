package acumen
package interpreters
package imperative

import scala.concurrent.SyncVar
import Errors._

class StaticThreadPool[A](private var active: Int, val total: Int) extends ThreadPool[A] {
  private val threads = new Array[StaticAcumenThread](total)
  private var next = 0

  for (i <- 0 until total) {
    val tr = new StaticAcumenThread(i+1)
    tr.start
    threads(i) = tr
  }

  def nbThreads = active
  
  def reset(n: Int) = {
    active = n
    next = 0 
  }

  def run(f: ()=>A) : SyncVar[A] = synchronized {
    if (next >= active) 
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

  class StaticAcumenThread(i:Int) extends Thread("acumen static thread #"+i) {
    setDaemon(true)
    val inbox = new SyncVar[Option[Tuple2[() => A, SyncVar[A]]]]
	@volatile var done = false 
    override def run = {
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
