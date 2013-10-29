package acumen
package extract

import scala.collection.SeqLike
import scala.collection.mutable.ArrayBuffer
import CondImplicits._

object Util {

  //
  // Name normalization
  //

  def getName(x: Expr): Option[Name] = x match {
    case Var(name) => Some(name)
    case Dot(expr, name) => getNameDot(expr, name)
    case _ => None
  }
  def getNameDot(expr: Expr, field: Name) : Option[Name] = expr match {
    case Var(Name("self", 0)) => Some(field)
    case Var(name) => Some(Name(name.x ++ "." ++ field.x, field.primes))
    case Dot(e,f) => getNameDot(e, Name(f.x ++ "." ++ field.x, field.primes))
    case _ => None
  }

  //
  // Deps. extraction
  //

  def extractDeps(x: Expr): Set[Name] = x match {
    case Op(_, lst) => lst.flatMap(extractDeps(_)).toSet
    case Lit(_) => Set.empty
    case ExprVector(lst) => lst.flatMap(extractDeps(_)).toSet
    case ExprInterval(e1, e2) => extractDeps(e1) ++ extractDeps(e2)
    case ExprIntervalM(e1, e2) => extractDeps(e1) ++ extractDeps(e2)
    case _ => getName(x) match {
      case Some(name) => Set(name)
      case None => throw UnhandledSyntax(x, "Can't extract dependencies.")
    }
  }

  def extractLHSDeps(a: ContinuousAction): Set[Name] = a match {
    case Equation(lhs, rhs) => extractDeps(lhs)
    case EquationI(lhs, rhs) => extractDeps(lhs)
    case EquationT(lhs, rhs) => extractDeps(lhs)
  }
  def extractRHSDeps(a: ContinuousAction): Set[Name] = a match {
    case Equation(lhs, rhs) => extractDeps(rhs)
    case EquationI(lhs, rhs) => extractDeps(rhs)
    case EquationT(lhs, rhs) => extractDeps(rhs)
  }

  //
  // Post Condition analysis
  //
  
  def postConds(cond: Cond, actions: Seq[Assign]) : Cond = 
  {
    var res = ArrayBuffer(cond.toSeq: _*)
    actions.foreach {
      case a @ Assign(lhs, rhs) => (getName(lhs), rhs) match {
        case (Some(name), Lit(value)) =>
          invalidate(Set(name))
          res += Cond.eq(name, value)
        case (Some(name), expr) =>
          invalidate(extractDeps(expr))
        case _ => 
          throw UnhandledSyntax(a:DiscreteAction, "Can't determine PostCond.")
      }
    }
    def invalidate(deps: Set[Name]) = res.indices.foreach { i =>
      if (res(i) != null) {
        val reqDeps = res(i).deps
        if (!deps.intersect(reqDeps).isEmpty)
          res(i) = null
      }
    }
    Cond.fromSeq(res.filter(_ != null))
  }  

  //
  // Other
  //

  def discrConds(conds: Cond, discrVars: Set[Name]) : Cond =
    conds.filter{c => c.deps.diff(discrVars).isEmpty}

  def getSimulatorAssigns[SeqT <: SeqLike[Assign,SeqT]](simulatorName: Name, actions: SeqT) : (SeqT,SeqT) =
    actions.partition {
      _ match {
        case Assign(Dot(Var(s), _), Lit(_)) if s == simulatorName => true
        case Assign(Dot(Dot(Var(Name("self", 0)), s), _), Lit(_)) if s == simulatorName => true
        case a @ _ => false
      }
    }

}
