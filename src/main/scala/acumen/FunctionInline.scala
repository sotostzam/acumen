package acumen

/**
 * @author Yingfu Zeng
 * FunctionInline performs an inline semantics for functions
 */
class FunctionInline (functions:List[Function]) extends util.ASTMap {
  val functionNames = functions.map(x => x.name)
  implicit val nosub = Map[Var,Expr]()
  def inline(func: Function, args:List[Expr]): Expr = {
    val sub = (func.paras map Var zip args).toMap
    mapExprWithSub(func.body)(sub)
  }
 override def mapExpr(e: Expr) = mapExprWithSub(e)(nosub)
 def mapExprWithSub(e: Expr)(sub:Map[Var,Expr]) : Expr = (e match{
    case Op(name, es) =>{
      val args = es.map{mapExprWithSub(_)(sub)}
      if (functionNames contains name.x) {
        inline(functions.find(_.name == name.x).get, args)
      }else
        Op(name,args)
    }
    case Lit(v) => Lit(v)
    case Var(v) => sub.getOrElse(Var(v), Var(v))
    case Dot(a,b) => Dot(mapExprWithSub(a)(sub),b)
    case Quest(a,b) => Quest(mapExprWithSub(a)(sub), b)
    case ResolvedDot(id, obj, field) => ResolvedDot(id,mapExprWithSub(obj)(sub),field)
    case Input(s,i) => Input(mapExprWithSub(s)(sub),i)
    case Index(a,idx) => Index(mapExprWithSub(a)(sub),idx.map{mapExprWithSub(_)(sub)})
    case ExprVector(l) => ExprVector(l.map{mapExprWithSub(_)(sub)})
    case Sum(s, i, col, cond) => Sum(mapExprWithSub(s)(sub), i, mapExprWithSub(col)(sub), mapExprWithSub(cond)(sub))
    case TypeOf(v) => TypeOf(v)
    case ExprInterval(lo, hi) => ExprInterval(mapExprWithSub(lo)(sub), mapExprWithSub(hi)(sub))
    case ExprLet(bindings, e2) => ExprLet(bindings.map{case (n,e) => (n,mapExprWithSub(e)(sub))},
                                          mapExprWithSub(e2)(sub))
    case Pattern(ps) => Pattern(ps.map{mapExprWithSub(_)(sub)})
    case _ => e

  }).setPos(e.pos)
}
