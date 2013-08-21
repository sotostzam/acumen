package acumen
package extract

import scala.collection.mutable.{ ArrayBuffer }
import scala.util.control.Breaks.{ break, breakable }

class MainClass(prog: Prog) {
  // State variables
  val origDef = {
    if (prog.defs.size > 1) 
      throw OtherUnsupported("Multiple objects not supported.")
    if (prog.defs(0).name != ClassName("Main"))
      throw OtherUnsupported("Could not find Main class.")
    prog.defs(0)
  }

  var init = origDef.priv
  var contIfs = new Ifs[ContinuousAction, ContIf](ContIf)
  var discrIfs = new Ifs[Assign, DiscrIf](DiscrIf)
  val simulatorName = origDef.fields(0)
  var simulatorAssigns: Seq[Assign] = Nil

  // Builds the initial data structures
  // notConds is here to simplify other operations
  def extract(allowSeqIfs: Boolean = false) {
    def f(conds: Seq[Cond], claims: List[Cond], notConds: Seq[Cond], actions: List[Action]) : Unit = {
      var prevConditional = false
      def checkPrevConditional() = 
        if (!allowSeqIfs && prevConditional)
          throw OtherUnsupported("Multiple conditionals in the same scope unsupported.")
        else
           prevConditional = true
      actions.foreach {
        case IfThenElse(cond, ifTrue, ifFalse) =>
          checkPrevConditional()
          val ifTrueConds = conds :+ Cond(cond)
          val ifFalseConds = conds :+ Cond.not(cond)
          f(ifTrueConds, claims, ifFalseConds, ifTrue)
          f(ifFalseConds, claims, ifTrueConds, ifFalse)
        case Switch(subject, clauses) =>
          checkPrevConditional()
          val switchNot = conds ++ clauses.map { case Clause(lhs, _, _) => Cond.Not(Cond.eq(subject, lhs)) }
          clauses.foreach {
            case Clause(lhs, newClaim, actions) =>
              val newClaims = if (newClaim != Lit(GBool(true))) claims :+ Cond(newClaim) else claims
              f(conds :+ Cond.eq(subject, lhs), newClaims, switchNot, actions)
          }
        case Continuously(action) =>
          val if0 = contIfs.add(conds)
          if0.claims = claims
          if0.actions += action
          contIfs.add(notConds)
        case Discretely(action: Assign) =>
          discrIfs.add(conds).actions += action
          discrIfs.add(notConds)
        case action =>
          // Note: This will include Discrete actions that are not an
          // assignment, and hance any object creation
          throw UnhandledSyntax(action, "")
      }
    }
    f(Nil,Nil,Nil,origDef.body)
  }

  // Extract simulator assigns from a DiscrIf and add to simulatorAssigns
  def extractSimulatorAssigns(if0: DiscrIf) = {
    var (sim, non) = if0.actions.partition {
      _ match {
        case Assign(Dot(Var(s), _), Lit(_)) if s == simulatorName => true
        case Assign(Dot(Dot(Var(Name("self", 0)), s), _), Lit(_)) if s == simulatorName => true
        case a @ _ => false
      }
    }
    simulatorAssigns ++= sim
    if0.actions = non
  }

}
