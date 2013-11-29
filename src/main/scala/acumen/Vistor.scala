package acumen

// Vistor performs a traveal over the AST using the typical vistor
// pattern.  

class Visitor {
  def visitProg(p: Prog) : Unit = p match {
    case Prog(defs) => defs.foreach{visitClassDef(_)}
  }
  
  def visitClassDef(d: ClassDef) : Unit = d match {
    case ClassDef(name, fields, priv, body) =>
      {priv.foreach{visitInit(_)}; visitActions(body)}
  }

  def visitInit(i: Init) : Unit = i match {
    case Init(x, rhs) => visitInitRhs(rhs)
  }
  
  def visitInitRhs(rhs: InitRhs) : Unit = rhs match {
    case NewRhs(c, f) => f.foreach{visitExpr(_)}
    case ExprRhs(e) => visitExpr(e)
  }

  def visitAction(a: Action) : Unit = a match {
    case IfThenElse(cond, ifTrue, ifFalse) =>
      {visitExpr(cond); visitActions(ifTrue); visitActions(ifFalse)}
    case Switch(subject, clauses) =>
      {visitExpr(subject); clauses.foreach{visitClause(_)}}
    case ForEach(it, col, body) =>
      {visitExpr(col); visitActions(body)}
    case Continuously(a) =>
      visitContinuousAction(a)
    case Discretely(a) =>
      visitDiscreteAction(a)
    case Claim(p) =>
      visitExpr(p)
  }

  def visitActions(as: List[Action]) : Unit = as.foreach{visitAction(_)}
  
  // Must make a copy of Expr even if there is nothing to do as 
  // Expr has state information associated with it
  def visitExpr(e: Expr) : Unit = e match {
    case Lit(v) => /* nothing to do */
    case Var(v) => /* nothing to do */
    case Op(name, es) => es.foreach{visitExpr(_)}
    case Dot(a,b) => visitExpr(a)
    case ExprVector(l) => l.foreach{visitExpr(_)}
    case Sum(s, i, col, cond) => visitExpr(s); visitExpr(col); visitExpr(cond)
    case TypeOf(v) => /* nothing to do */
    case ExprInterval(lo, hi) => visitExpr(lo); visitExpr(hi)
    case ExprIntervalM(mid, pm) => visitExpr(mid); visitExpr(pm)
    // DO NOT add a default case here, it's important that all
    // possibilities are covered when new syntax is added
  }

  def visitClause(c: Clause) : Unit = c match {
    case Clause(lhs, as, rhs) => visitExpr(as); visitActions(rhs)
  }

  def visitContinuousAction(a: ContinuousAction) : Unit = a match {
    case Equation(lhs, rhs) => visitExpr(lhs); visitExpr(rhs)
    case EquationI(lhs, rhs) => visitExpr(lhs); visitExpr(rhs)
    case EquationT(lhs, rhs) => visitExpr(lhs); visitExpr(rhs)
  }

  def visitDiscreteAction(a: DiscreteAction) : Unit = a match {
    case Assign(lhs, rhs) => visitExpr(lhs); visitExpr(rhs)
    case Create(lhs, name, args) => lhs.foreach{visitExpr(_)}; args.foreach{visitExpr(_)}
    case Elim(e) => visitExpr(e)
    case Move(obj, newParent) => visitExpr(obj); visitExpr(newParent)
  }
}
