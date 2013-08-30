package acumen

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
}

