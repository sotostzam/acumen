package acumen
package extract

import scala.collection.mutable.{ ListMap => MutListMap, ArrayBuffer }
import scala.util.control.Breaks.{ break, breakable }

/***************************************************************************
 * Data structures used for the extraction.  The act of forming these
 * data structures performs the SPLIT, TRANSFORM and FLATTEN steps
 ***************************************************************************/

// Type classes, the scala way
trait ActionOps[ActionT] {
  def toAST(a: ActionT) : Action
}
object ActionOps {
  implicit object AssignActionOps extends ActionOps[Assign] {
    def toAST(a: Assign) = Discretely(a)
  }
  implicit object ContActionOps extends ActionOps[ContinuousAction] {
    def toAST(a: ContinuousAction) = Continuously(a)
  }
}

class If[ActionT](var conds: Seq[Cond])(implicit ops: ActionOps[ActionT])  {
  var actions = new ArrayBuffer[ActionT];
  var claims: List[Cond] = Nil; // i.e. "claims" used to annotate modes
  var visited : Boolean = false
  def toAST: IfThenElse =
    IfThenElse(Cond.toExpr(conds),
        actions.map(ops.toAST(_)).toList, Nil)
  def dump = Pretty.pprint[Action](toAST) + "\n"
}

class Mode(val label: String, 
           var claims: List[Cond], 
           var actions: Seq[ContinuousAction]) 
{
  var resets: List[If[Assign]] = Nil
}

class Ifs[ActionT : ActionOps] {
  type IfT = If[ActionT]
  val data = new MutListMap[Seq[Cond], IfT]
  def find(conds: Seq[Cond]): IfT = data.get(conds) match {
    case Some(res) => res
    case None => throw new java.util.NoSuchElementException
  }
  def add(conds: Seq[Cond]): IfT = data.getOrElseUpdate(conds, new IfT(conds))
  // pushDown transforms a series of ifs into a unique form such that all
  // actions for a given set of conditions are in exactly one if.
  // Requires empty ifs to push the actions into.
  def pushDown(if0: IfT, actions: List[ActionT]) {
    if0.visited = true
    val candidates = withPrefix(if0.conds).toList.sortWith {(a,b) => a.conds.size < b.conds.size}
    if (candidates.nonEmpty) {
      candidates.foreach{if1 => 
        if (!if1.visited)
          pushDown(if1, if0.actions.toList ++ actions)
      }
      if0.actions.clear
    } else {
      if0.actions ++= actions
    }
  }
  def pushDown() {
    data.values.foreach(_.visited = false)
    pushDown(find(Nil), Nil)
    data.retain { (_, if0) => !if0.actions.isEmpty }
  }
  def withPrefix(toFind: Seq[Cond]): Iterable[IfT] =
    data.filterKeys { conds => conds.size > toFind.size && conds.startsWith(toFind) }.values
}
