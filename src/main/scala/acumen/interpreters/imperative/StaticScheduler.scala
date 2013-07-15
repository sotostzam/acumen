package acumen.interpreters.imperative

import acumen.util.Canonical.{ classf, cmagic  }
import Common.{ Changeset, combine, getField, noChange, ObjId, traverseSimple }

class StaticScheduler(val threadPool: StaticThreadPool[Changeset]) extends Scheduler {
  
  /* precondition: n != 0 */
  def traverseMain(f:ObjId => Changeset, root:ObjId) : Changeset = {
    val r = f(root)
    val cs = root.children filter (getField(_,classf) != acumen.VClassName(cmagic))
    r || recurse(f, cs, threadPool.nbThreads)
  }
  
  def parCombine[A](xs:Traversable[A], f: A => Changeset) : Changeset = {
    val boxes = xs map { x => threadPool.run(() => f(x)) }
    var res : Changeset = noChange
    for (b <- boxes) res = res || b.get 
    res
  }
  
  def splitInto(size: Int, chunks: Int): Option[List[(Int, Int)]] = {
    if (chunks > size) None
    else {
      val len = size / chunks
      val rem = size % chunks
      def helper1(i: Int, r: Int, acc: List[(Int, Int)]): List[(Int, Int)] = {
        if (r > 0) helper2(i, len, r - 1, i, acc)
        else helper2(i, len - 1, r, i, acc)
      }
      def helper2(i: Int, k: Int, r: Int, start: Int, acc: List[(Int, Int)]): List[(Int, Int)] = {
        if (i == size - 1) {
          ((start, i) :: acc).reverse
        }
        else if (k == 0) {
          helper1(i + 1, r, ((start, i) :: acc))
        }
        else {
          helper2(i + 1, k - 1, r, start, acc)
        }
      }
      Some(helper1(0, rem, Nil))
    }
  }
  
  def recurse(f:ObjId => Changeset, cs:Vector[ObjId], n:Int) : Changeset = {
    val len = cs.length
    splitInto(len, n) match {
      case Some(slices) =>
        parCombine[(Int,Int)](
          slices, 
          { case (b,e) =>
              var res : Changeset = noChange
              for (i <- b to e) res = res || traverseSimple(f, cs(i))
              res
          })
      case None => // n < len
        if (len>1) {
          val tasks = n-len+1 
          val quot = tasks / len
          val rem = tasks % len
          val (load::loads) = List.fill(rem)(quot+1) ::: List.fill(len-rem)(quot)
          val threads = (cs zip loads) map {
            case (c,i) => threadPool.run( () => traverseThreaded(f,c,i) )
          }
          var res = traverseThreaded(f,cs.last,load)
          for (t <- threads) res = res || t.get
          res
        } else if (len == 1) {
          traverseThreaded(f, cs.head, n)
        } else noChange
    }
  }
  
  def traverseThreaded(f:ObjId => Changeset, root:ObjId, n:Int) : Changeset = {
    if (n==0) traverseSimple(f, root)
    else {
      val r = f(root)
      val cs = root.children
      r || recurse(f, cs, n)
    }
  }
  
}
