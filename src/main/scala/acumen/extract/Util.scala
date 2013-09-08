package acumen
package extract

import scala.collection.mutable.ArrayBuffer

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

  def extractDeps(x: Expr): Seq[Name] = x match {
    case Op(_, lst) => lst.flatMap(extractDeps(_)).distinct
    case Lit(_) => Nil
    case ExprVector(lst) => lst.flatMap(extractDeps(_)).distinct
    case ExprInterval(e1, e2) => (extractDeps(e1) ++ extractDeps(e2)).distinct
    case ExprIntervalM(e1, e2) => (extractDeps(e1) ++ extractDeps(e2)).distinct
    case _ => getName(x) match {
      case Some(name) => List(name)
      case None => throw UnhandledSyntax(x, "Can't extract dependencies.")
    }
  }

  def extractLHSDeps(a: ContinuousAction): Seq[Name] = a match {
    case Equation(lhs, rhs) => extractDeps(lhs)
    case EquationI(lhs, rhs) => extractDeps(lhs)
    case EquationT(lhs, rhs) => extractDeps(lhs)
  }
  def extractRHSDeps(a: ContinuousAction): Seq[Name] = a match {
    case Equation(lhs, rhs) => extractDeps(rhs)
    case EquationI(lhs, rhs) => extractDeps(rhs)
    case EquationT(lhs, rhs) => extractDeps(rhs)
  }

  //
  // Condition matching
  //

  sealed abstract class MatchRes
  case object CondTrue extends MatchRes;
  case object CondFalse extends MatchRes;
  case class CantTell(unmatched: Seq[Cond]) extends MatchRes;

  def matchConds(have: Seq[Cond], need: Seq[Cond]): MatchRes = {
    // filter out predicates that exists in both
    val unmatched = need.filter { a => !have.exists { b => a == b } }
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

  //
  // Post Condition analysis
  //
  
  def initPostCond(init: Seq[Init])  = init.flatMap {
    _ match {
      case Init(name, ExprRhs(Lit(value))) => List(Cond.Eq(name, value))
      case _ => Nil
    }
  }

  def postConds(conds: Seq[Cond], actions: Seq[Assign]) : Seq[Cond] = 
  {
    var res = ArrayBuffer(conds: _*)
    actions.foreach {
      case a @ Assign(lhs, rhs) => (getName(lhs), rhs) match {
        case (Some(name), Lit(value)) =>
          invalidate(List(name))
          res += Cond.Eq(name, value)
        case (Some(name), expr) =>
          invalidate(extractDeps(expr))
        case _ => 
          throw UnhandledSyntax(a:DiscreteAction, "Can't determine PostCond.")
      }
    }
    def invalidate(deps: Seq[Name]) = res.indices.foreach { i =>
      if (res(i) != null) {
        val reqDeps = res(i).deps
        if (!deps.intersect(reqDeps).isEmpty)
          res(i) = null
      }
    }
    res.filter(_ != null)
  }  

  def discrConds(conds: Seq[Cond], contVars: Seq[Name]) : Seq[Cond] =
    conds.filter{c => c.deps.intersect(contVars).isEmpty}
  
}
