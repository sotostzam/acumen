package acumen
package extract

import scala.collection.mutable.{ ListMap => MutListMap, ArrayBuffer }
import scala.util.control.Breaks.{ break, breakable }

/***************************************************************************
 * Data structures used for the extraction.  The act of forming these
 * data structures performs the SPLIT, TRANSFORM and FLATTEN steps
 ***************************************************************************/

sealed abstract class MatchRes
case object CondTrue extends MatchRes;
case object CondFalse extends MatchRes;
case class CantTell(unmatched: Seq[Cond]) extends MatchRes;

abstract class If[ActionT](var conds: Seq[Cond], val label: String) {
  def toAST: IfThenElse
  var actions = new ArrayBuffer[ActionT];
  def dump: String;
  def reset: Unit; 

  // matchConds: Given "have" try to determine if the conditionals for
  //   this if are true or false
  def matchConds(have: Seq[Cond]): MatchRes = {
    // filter out predicates that exists in both
    val unmatched = conds.filter { a => !have.exists { b => a == b } }
    // if no unmatched predicates return true
    if (unmatched.isEmpty) return CondTrue
    // if A is required and ~A is known, return false
    if (unmatched.exists { a => have.exists { b => Cond.not(a) == b } }) return CondFalse
    // if SYM == SOMETHING is required and SYM == ANYTHING is known
    // return false (the case when SYM == SOMETHING is known was
    // already eliminated)
    if (unmatched.exists {
      _ match {
        case Cond.Eq(a, _) => have.exists {
          _ match {
            case Cond.Eq(b, _) => a == b;
            case _ => false
          }
        }
        case _ => false
      }
    }) return CondFalse
    // there are predicates that can't be shown to be either true or
    // false so return maybe
    return CantTell(unmatched)
  }
  var reachable = false
  var d0Mode = false;
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
  var needGuard = false;
  var resets: List[DiscrIf] = Nil;
  var origConds: Seq[Cond] = null;
  def reset() { needGuard = false; resets = null; }
  def discrConds(deps: Seq[Name]): Seq[Cond] =
    conds.filter(Cond.getDeps(_).intersect(deps).isEmpty)
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
    val post = postConds()
    (label + ": "
      + Pretty.pprint[Action](toAST)
      + "\nPost Conds: "
      + Pretty.pprint[Expr](Cond.toExpr(post))
      + "\n")
  }
  def postConds(conds0: Seq[Cond] = null) : Seq[Cond] = {
    var res = ArrayBuffer((if (conds0 != null) conds0 else conds): _*)
    actions.foreach {
      case a @ Assign(lhs, rhs) => (Cond.getName(lhs), rhs) match {
        case (Some(name), Lit(value)) =>
          invalidate(List(name))
          res += Cond.Eq(name, value)
        case (Some(name), expr) =>
          invalidate(Cond.extractDeps(expr))
        case _ =>
          // If we can't extract a name, than it likely an assignment to the 
          // simulator parameters so ignore it for now.
          //throw UnhandledSyntax(a:DiscreteAction, "Can't determine PostCond.")
      }
    }
    def invalidate(deps: Seq[Name]) = res.indices.foreach { i =>
      val reqDeps = Cond.getDeps(res(i))
      if (!deps.intersect(reqDeps).isEmpty)
        res(i) = null
    }
    res.filter(_ != null)
  }
  var matches: Seq[ContIf] = null;
  var maybe: Seq[ContIf] = null;
  def reset = { matches = null; maybe = null; }
  def dup(conds: Seq[Cond]) = {
    var res = new DiscrIf(conds)
    res.actions ++= actions
    res
  }
}
object DiscrIf extends MkIf[DiscrIf] {
  def apply(conds: Seq[Cond]) = new DiscrIf(conds)
  var counter = 0
  def newLabel = { counter += 1; "d" + counter; }
}

class Ifs[ActionT, IfT <: If[ActionT]](mkIf: MkIf[IfT]) {
  //def findAll(Seq[Conds]) : Seq[If[ActionT]]
  //def find(Seq[Conds]) : Maybe[If[ActionT]]
  val data = new MutListMap[Seq[Cond], IfT]
  var emptyIfs: List[IfT] = null;
  def find(conds: Seq[Cond]): Option[IfT] = data.get(conds)
  def add(conds: Seq[Cond]): IfT = data.getOrElseUpdate(conds, mkIf(conds))
  def addwEmpties(conds: Seq[Cond]): IfT = {
    if (!conds.isEmpty) {
      addwEmpties(conds.init) // init = all but last element of list
      add(conds.init :+ Cond.not(conds.last))
    }
    add(conds)
  }
  // Uniquify transforms a series of ifs into a unique form such that all
  // actions for a given set of conditions are in exactly one if.
  // Part of TRANSFORM step.
  def uniquify() {
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
    emptyIfs = data.values.filter(_.actions.isEmpty).toList
    data.retain { (_, if0) => !if0.actions.isEmpty }
  }
  def withCondsOfLength(length: Int): Iterable[IfT] =
    data.filterKeys { _.size == length }.values
  def withPrefix(toFind: Seq[Cond]): Iterable[IfT] =
    data.filterKeys { conds => conds.size > toFind.size && conds.startsWith(toFind) }.values
  def reset() { data.values.foreach(_.reset); }
}
