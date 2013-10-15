package acumen
import Errors.ShouldNeverHappen

object KillNot extends ASTMap {

  def negate(e: Expr) : Expr = e match {
    case Op(Name(op,_), es) => op match {
      case "not" => es.head
      case "==" => Op(Name("~=",0), es map {mapExpr(_)})
      case "~=" => Op(Name("==",0), es map {mapExpr(_)})
      case "<" => Op(Name(">=",0), es map {mapExpr(_)})
      case ">" => Op(Name("<=",0), es map {mapExpr(_)})
      case "<=" => Op(Name(">",0), es map {mapExpr(_)})
      case ">=" => Op(Name("<",0), es map {mapExpr(_)})
      case "&&" => Op(Name("||",0), es map {negate(_)})
      case "||" => Op(Name("&&",0), es map {negate(_)})
      case _ => throw ShouldNeverHappen()
    }
    case _ => throw ShouldNeverHappen()
  }

  override def mapExpr(e: Expr) = e match {
    case Op(Name("not",_), List(e1)) => negate(e1)
    case _ => super.mapExpr(e)
  }

}
