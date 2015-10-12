package acumen
package util

import Errors.PositionalAcumenError

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

  /** Replaces all occurrences of d in h for e. */
  def substitute(d: Expr, e: Expr, h: Expr): Expr = {
    def sub(h: Expr): Expr = substitute(d, e, h)
    if (h == d) e else h match {
      case _: Dot | _: ResolvedDot | _: Lit | _: TypeOf | _: Var => h
      case Op(name, es)           => Op(name, es map sub)
      case Index(a, idx)          => Index(sub(a), idx map sub)
      case ExprVector(l)          => ExprVector(l map sub)
      case Sum(s, i, col, cond)   => Sum(sub(s), i, sub(col), sub(cond))
      case ExprInterval(lo, hi)   => ExprInterval(sub(lo), sub(hi))
      case ExprIntervalM(mid, pm) => ExprIntervalM(sub(mid), sub(pm))
      case ExprLet(bindings, e2)  => ExprLet(bindings.map { case (n, e) => n -> sub(e) }, sub(e2))
      case Pattern(ps)            => Pattern(ps map sub)
    }
  }.setPos(h.pos)

  /** Returns all variables that occur in e. */
  def dots(e: Expr): List[Dot] = e match {
    case d: Dot                 => d :: Nil
    case Op(_, es)              => es flatMap dots
    case Index(a, idx)          => dots(a) ::: idx flatMap dots
    case ExprVector(l)          => l flatMap dots
    case Sum(s, i, col, cond)   => dots(s) ::: dots(col) ::: dots(cond)
    case ExprInterval(lo, hi)   => dots(lo) ::: dots(hi)
    case ExprIntervalM(mid, pm) => dots(mid) ::: dots(pm)
    case ExprLet(bindings, e2)  => bindings.flatMap(b => dots(b._2)) ::: dots(e2)
    case _                      => Nil
  }

  /** Allow hypotheses only at top level of models. */
  def checkNestedHypotheses(prog: Prog): Unit = {
    def disallowHypotheses(a: Action, atTopLevel: Boolean): Unit = a match {
      case IfThenElse(_,t,e) => for (s <- t ::: e) disallowHypotheses(s, false) 
      case ForEach(_,_,b) => for (s <- b) disallowHypotheses(s, false)
      case Switch(_,cs) => for (c <- cs; s <- c.rhs) disallowHypotheses(s, false)
      case Hypothesis(s,p) if !atTopLevel => 
        throw new PositionalAcumenError{ def mesg = "Hypothesis statements are only allowed at the top level of models." }.setPos(p.pos)
      case _ =>
    }
    for (cd <- prog.defs; a <- cd.body) disallowHypotheses(a, true)
  }

  /** Disallow continuous assignments to simulator parameters.
   *  Note: Only throws an error if the Main class is defined. */
  def checkContinuousAssignmentToSimulator(prog: Prog): Unit =
    prog.defs.find(_.name == Canonical.cmain).map { mainClass =>
      val simulatorName = mainClass fields 0
      new ASTMap {
        override def mapContinuousAction(a: ContinuousAction): ContinuousAction = a match {
          case EquationT(Dot(Dot(Var(Canonical.self), `simulatorName`), _), rhs) =>
            throw new Errors.ContinuousAssignmentToSimulator(rhs)
          case _ => super.mapContinuousAction(a)
        }
      }.mapProg(prog)
    }
  
  /** Apply mapG recursively to v. */
  def mapValue[Id,G](v: Value[Id], mapG: G => Value[Id]): Value[Id] = v match {
    case VLit(g: G) => mapG(g)
    case VVector(l: List[Value[Id]]) => VVector(l map (mapValue(_, mapG)))
    case VList(l: List[Value[Id]]) => VList(l map (mapValue(_, mapG)))
  }
  
  /** Apply mapGs recursively to l and r, assuming they have identical structure. */
  def mapValuePair[Id,G](l: Value[Id], r: Value[Id], mapGs: (G,G) => Value[Id]): Value[Id] = (l, r) match {
    case (VLit(gl: G), VLit(gr: G)) => mapGs(gl, gr)
    case (VVector(ll: List[Value[Id]]), VVector(lr: List[Value[Id]])) => 
      VVector((ll zip lr) map { case (l,r) => mapValuePair(l, r, mapGs) })
    case (VList(ll: List[Value[Id]]), VList(lr: List[Value[Id]])) => 
      VList((ll zip lr) map { case (l,r) => mapValuePair(l, r, mapGs) })
  }

}

