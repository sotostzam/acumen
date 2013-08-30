package acumen

import ASTUtil.exprSubParts

object InlineInitDeps {
  def proc(p: Prog) : Prog = {
    Prog(p.defs.map{c => c.copy(priv = procPriv(c.priv))})
  }

  def procPriv(priv: List[Init]) : List[Init] = {
    priv.map{init => 
      new ASTMap {
        override def mapExpr(e: Expr) = e match {
          case Var(name) => lookup(name)
          case Dot(Var(Name("self", 0)), name) => lookup(name)
          case e => super.mapExpr(e)
        }
        def lookup(name: Name) : Expr = {
          priv.find{_.x == name} match {
            case Some(Init(_,ExprRhs(e))) => e
            case _ => null
          }
        }
      }.mapInit(init)}
  }
}
