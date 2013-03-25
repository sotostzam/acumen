package acumen
package interpreters
package parallel

import util.Canonical.{ classf, cmagic  }
import Interpreter.{ Changeset, combine, getField, noChange, ObjId }
import Interpreter.{ splitInto, traverseSimple }

class StaticScheduler(val threadPool: StaticThreadPool[Changeset]) extends Scheduler {
  
  /* precondition: n != 0 */
  def traverseMain(f:ObjId => Changeset, root:ObjId) : Changeset = {
    val r = f(root)
    val cs = root.children filter (getField(_,classf) != VClassName(cmagic))
    r || recurse(f, cs, threadPool.nbThreads)
  }
  
  def parCombine[A](xs:Traversable[A], f: A => Changeset) : Changeset = {
    val boxes = xs map { x => threadPool.run(() => f(x)) }
    var res : Changeset = noChange
    for (b <- boxes) res = res || b.get 
    res
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
      case None =>
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