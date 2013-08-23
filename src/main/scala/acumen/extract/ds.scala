package acumen
package extract

import scala.collection.mutable.{ ListMap => MutListMap, ArrayBuffer }
import scala.util.control.Breaks.{ break, breakable }

/***************************************************************************
 * Data structures used for the extraction.  The act of forming these
 * data structures performs the SPLIT, TRANSFORM and FLATTEN steps
 ***************************************************************************/

abstract class If[ActionT](var conds: Seq[Cond], val label: String) {
  def toAST: IfThenElse
  var actions = new ArrayBuffer[ActionT];
  def dump: String;
}
abstract class MkIf[IfT] {
  def apply(conds: Seq[Cond]): IfT
}

class ContIf(conds0: Seq[Cond], label: String = ContIf.newLabel) extends If[ContinuousAction](conds0, label) {
  def toAST: IfThenElse =
    IfThenElse(Cond.toExpr(conds),
      resets.map(_.toAST) ++
        actions.map(Continuously(_)).toList, Nil)
  def dump = label + ": " + Pretty.pprint[Action](toAST) + "\n"
  var claims: List[Cond] = Nil; // i.e. "claims" used to annotate modes
  var resets: List[DiscrIf] = Nil;
}
object ContIf extends MkIf[ContIf] {
  def apply(conds: Seq[Cond]) = new ContIf(conds)
  var counter = 0
  def newLabel = { counter += 1; "C" + counter; }
}

class DiscrIf(conds0: Seq[Cond]) extends If[Assign](conds0, DiscrIf.newLabel) {
  def toAST: IfThenElse =
    IfThenElse(Cond.toExpr(conds),
      actions.map(Discretely(_)).toList, Nil)
  def dump = {
    (label + ": "
      + Pretty.pprint[Action](toAST)
      + "\n")
  }
}
object DiscrIf extends MkIf[DiscrIf] {
  def apply(conds: Seq[Cond]) = new DiscrIf(conds)
  var counter = 0
  def newLabel = { counter += 1; "d" + counter; }
}

class Ifs[ActionT, IfT <: If[ActionT]](mkIf: MkIf[IfT]) {
  val data = new MutListMap[Seq[Cond], IfT]
  def find(conds: Seq[Cond]): Option[IfT] = data.get(conds)
  def add(conds: Seq[Cond]): IfT = data.getOrElseUpdate(conds, mkIf(conds))
  // pushDown transforms a series of ifs into a unique form such that all
  // actions for a given set of conditions are in exactly one if.
  // Requires empty ifs to push the actions into.
  def pushDown() {
    var i = 0
    breakable { while (true) {
      val bla = withCondsOfLength(i)
      if (bla.isEmpty) break
      bla.foreach { if1 =>
        val ifs2 = withPrefix(if1.conds)
        ifs2.foreach { if2 =>
          if2.actions ++= if1.actions
        }
        if (!ifs2.isEmpty) if1.actions.clear
      }
      i += 1;
    }}
    data.retain { (_, if0) => !if0.actions.isEmpty }
  }
  def withCondsOfLength(length: Int): Iterable[IfT] =
    data.filterKeys { _.size == length }.values
  def withPrefix(toFind: Seq[Cond]): Iterable[IfT] =
    data.filterKeys { conds => conds.size > toFind.size && conds.startsWith(toFind) }.values
}
