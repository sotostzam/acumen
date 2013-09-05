package acumen
package extract

sealed abstract class Cond { 
  def toExpr: Expr
  def deps : Seq[Name]
}

object Cond {
  import Util._
  //
  // case class members, constructing them directly is discoursed, use
  // factor methods/constructors below
  // 

  case object True extends Cond {
    def toExpr = Lit(GBool(true))
    def deps = Nil
  }
  case class Eq(name: Name, value: GroundValue) extends Cond {
    def toExpr = Op(Name("==", 0), List(Dot(Var(Name("self", 0)), name), Lit(value)))
    def deps = Seq(name)
  }
  case class Not(cond: Cond) extends Cond {
    def toExpr = Op(Name("not", 0), List(cond.toExpr))
    def deps = cond.deps
  }
  case class Other(expr: Expr, deps: Seq[Name]) extends Cond {
    def toExpr = expr
  }

  //
  // Constructors
  //

  def apply(e: Expr): Cond = e match {
    case Lit(GBool(true)) => True
    case Lit(GBool(false)) => Not(True)
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

  //
  // Utility
  // 

  def toExpr(conds: Seq[Cond]): Expr = 
    if (conds.nonEmpty) 
      conds.tail.foldLeft(conds.head.toExpr) { (a, b) => Op(Name("&&", 0), List(a, b.toExpr)) }
    else
      Lit(GBool(true))
}
