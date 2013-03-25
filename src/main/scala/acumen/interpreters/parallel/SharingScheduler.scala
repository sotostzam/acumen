package acumen
package interpreters
package parallel

import util.Canonical.{ classf, cmagic }
import Interpreter.{ Changeset, combine, getField, noChange, ObjId }

class SharingScheduler(val threadPool: SharingThreadPool[Changeset]) extends Scheduler {
  
  /**
   * Produces a Seq of n parts (each a Seq[A]), consisting of elements
   * of the input Seq[A]. Each part differs in length by at most one
   * compared to the other parts.
   */
  def divideEvenly[A](v: Seq[A], n: Int): Seq[Seq[A]] = {
    val sizes = Array.fill(n)(0)
    var t = 0
    while (t < v.length) {
      sizes(t % n) += 1
      t = t + 1
    }
    val chunkStartIndices = sizes.scan(0)(_ + _)
    val chunks = chunkStartIndices zip chunkStartIndices.tail
    chunks.map { case (b, e) => v.slice(b, e) }.toIndexedSeq
  }

  /**
   * Divide up the work (objects to be evaluated into Changesets) into
   * work to be done by the current thread and chunks to be processed
   * by available threads. If there are free threads in the thread pool,
   * divide the objects in cs into chunks and schedule them on these
   * threads, returning a SyncVar for each ObjectId, which can later be
   * evaluated to obtain the resulting Changeset.
   */
  def divideWork(f: ObjId => Changeset, cs: Vector[ObjId]) =
    threadPool.synchronized(
      if (threadPool.free == 0) (cs, None)
      else {
        val chunks = divideEvenly(cs, threadPool.free + 1)
        (chunks.head,
          Some(chunks.tail.map(chunk =>
            threadPool.run(() => traverseCombineThreaded(f, chunk)))))
      })

  /**
   * Evaluates the objects in s (into Changesets) and combines these
   * into a single Changeset.
   */
  def traverseCombineThreaded(f: ObjId => Changeset, s: Seq[ObjId]) =
    combine(s.map(traverseThreaded(f, _: ObjId)))

  /**
   * Evaluates the objects in cs (into Changesets) and combines these
   * into a single Changeset. This is done in parallel if there are
   * free threads in the threadPool.
   */
  def parCombine(f: ObjId => Changeset, cs: Vector[ObjId]): Changeset =
    if (cs.length == 1) traverseThreaded(f, cs(0))
    else {
      val (myWork, theirWork) = divideWork(f, cs)
      traverseCombineThreaded(f, myWork) || (theirWork match {
        case None => noChange
        case Some(i) => {
          var r: Changeset = noChange
          for { c <- i } { r = r || c.get }
          r
        }
      })
    }

  /* precondition: n != 0 */
  def traverseMain(f: ObjId => Changeset, root: ObjId): Changeset = {
    val r = f(root)
    val cs = root.children filter (getField(_, classf) != VClassName(cmagic))
    if (cs.isEmpty) r else r || parCombine(f, cs)
  }

  def traverseThreaded(f: ObjId => Changeset, root: ObjId): Changeset = {
    val r = f(root)
    val cs = root.children
    if (cs.isEmpty) r else r || parCombine(f, cs)
  }
  
}