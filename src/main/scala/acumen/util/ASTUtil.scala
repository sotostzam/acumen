package acumen
package util

object ASTUtil {

  def exprSubParts(e: Expr) : List[Expr] = e match {
    case Lit(v) => Nil
    case Var(v) => Nil
    case Op(name, es) => es
    case Dot(a,b) => List(a)
    case ExprVector(l) => l
    case Sum(s, i, col, cond) => List(s,col,cond)
    case TypeOf(v) => Nil
    case ExprInterval(lo, hi) => List(lo,hi)
    case ExprIntervalM(mid, pm) => List(mid,pm)
  }

  def exprSubParts(i: InitRhs) : List[Expr] = i match {
    case NewRhs(_, fields) => fields
    case ExprRhs(e) => List(e)
  }
  
  def op(o: String, ps: Expr*): Expr = Op(Name(o,0), ps.toList)
  
  /** Returns all variables that occur in e. */
  def dots(e: Expr): List[Dot] = e match {
    case d @ Dot(_, _) => d :: Nil
    case Op(_, es)     => es flatMap dots
    case _             => Nil
  } 
  
}

