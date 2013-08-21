package acumen
package extractfull

sealed abstract class Cond { def toExpr: Expr; }

object Cond {

  case class Eq(name: Name, value: GroundValue) extends Cond {
    def toExpr = Op(Name("==", 0), List(Dot(Var(Name("self", 0)), name), Lit(value)))
  }
  case class Not(cond: Cond) extends Cond {
    def toExpr = Op(Name("not", 0), List(cond.toExpr))
  }
  case class Other(expr: Expr, deps: Seq[Name]) extends Cond {
    def toExpr = expr
  }

  def getName(x: Expr): Option[Name] = x match {
    case Var(name) => Some(name)
    case Dot(Var(Name("self", 0)), name) => Some(name)
    case _ => None
  }
  def getDeps(cond: Cond): Seq[Name] = cond match {
    case null => Nil
    case Eq(name, _) => List(name)
    case Not(cond) => getDeps(cond)
    case Other(_, deps) => deps
  }

  // Constructors
  def apply(e: Expr): Cond = e match {
    case Op(Name("==", 0), List(x, Lit(value))) => eq(x, value)
    case Op(Name("not", 0), List(x)) => not(Cond(x))
    case _ => Other(e, extractDeps(e))
  }
  def eq(x: Expr, y: Expr): Cond = (x, y) match {
    case (_, Lit(value)) => eq(x, value)
    case _ => Other(Op(Name("==", 0), List(x, (y))), extractDeps(x) ++ extractDeps(y))
  }
  def eq(x: Expr, y: GroundValue): Cond = (getName(x), y) match {
    case (Some(name), _) => Eq(name, y)
    case _ => Other(Op(Name("==", 0), List(x, Lit(y))), extractDeps(x))
  }
  def not(cond: Cond): Cond = cond match {
    case Not(cond) => cond
    case _ => Not(cond)
  }
  def not(x: Expr): Cond = not(Cond(x))

  def extractDeps(x: Expr): Seq[Name] =
    x match {
      case Op(_, lst) => lst.flatMap(extractDeps(_)).distinct
      case Var(name) => List(name)
      case Dot(Var(Name("self", 0)), name) => List(name)
      case Lit(_) => Nil
      case ExprVector(lst) => lst.flatMap(extractDeps(_)).distinct
      case ExprInterval(e1, e2) => (extractDeps(e1) ++ extractDeps(e2)).distinct
      case ExprIntervalM(e1, e2) => (extractDeps(e1) ++ extractDeps(e2)).distinct
      case _ => throw UnhandledSyntax(x, "Can't extract dependencies.")
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
  def toExpr(conds: Seq[Cond]): Expr = 
    if (conds.nonEmpty) 
      conds.tail.foldLeft(conds.head.toExpr) { (a, b) => Op(Name("&&", 0), List(a, b.toExpr)) }
    else
      Lit(GBool(true))
}
