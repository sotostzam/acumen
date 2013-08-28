package acumen

// ASTMap performs a map over the AST to transform it into a new AST
// with a similar structure, also makes a semi-deep copy so that every
// map* method returns a copy of the object even if there is nothing
// to do.  State variables are not copied.

class ASTMap {
  def mapProg(p: Prog) : Prog = p match {
    case Prog(defs) => Prog(defs.map{mapClassDef(_)})
  }
  
  def mapClassDef(d: ClassDef) : ClassDef = d match {
    case ClassDef(name, fields, priv, body) =>
      ClassDef(name, fields, priv.map{mapInit(_)}, mapActions(body))
  }

  def mapInit(i: Init) : Init = i match {
    case Init(x, rhs) => Init(x, mapInitRhs(rhs))
  }
  
  def mapInitRhs(rhs: InitRhs) : InitRhs = rhs match {
    case NewRhs(c, f) => NewRhs(c, f.map{mapExpr(_)})
    case ExprRhs(e) => ExprRhs(mapExpr(e))
  }

  def mapAction(a: Action) : Action = a match {
    case IfThenElse(cond, ifTrue, ifFalse) =>
      IfThenElse(mapExpr(cond), mapActions(ifTrue), mapActions(ifFalse))
    case Switch(subject, clauses) =>
      Switch(mapExpr(subject), clauses.map{mapClause(_)})
    case ForEach(it, col, body) =>
      ForEach(it, mapExpr(col), mapActions(body))
    case Continuously(a) =>
      Continuously(mapContinuousAction(a))
    case Discretely(a) =>
      Discretely(mapDiscreteAction(a))
  }

  def mapActions(as: List[Action]) : List[Action] = as.map{mapAction(_)}
  
  // Must make a copy of Expr even if there is nothing to do as 
  // Expr has state information associated with it
  def mapExpr(e: Expr) : Expr = e match {
    case Lit(v) => Lit(v)
    case Var(v) => Var(v)
    case Op(name, es) => Op(name, es.map{mapExpr(_)})
    case Dot(a,b) => Dot(mapExpr(a),b)
    case ExprVector(l) => ExprVector(l.map{mapExpr(_)})
    case Sum(s, i, col, cond) => Sum(mapExpr(s), i, mapExpr(col), mapExpr(cond))
    case TypeOf(v) => TypeOf(v)
    case ExprInterval(lo, hi) => ExprInterval(mapExpr(lo), mapExpr(hi))
    case ExprIntervalM(mid, pm) => ExprIntervalM(mapExpr(mid), mapExpr(pm))
  }

  def mapClause(c: Clause) : Clause = c match {
    case Clause(lhs, as, rhs) => Clause(lhs, mapExpr(as), mapActions(rhs))
  }

  def mapContinuousAction(a: ContinuousAction) : ContinuousAction = a match {
    case Equation(lhs, rhs) => Equation(mapExpr(lhs), mapExpr(rhs))
    case EquationI(lhs, rhs) => EquationI(mapExpr(lhs), mapExpr(rhs))
    case EquationT(lhs, rhs) => EquationT(mapExpr(lhs), mapExpr(rhs))
  }

  def mapDiscreteAction(a: DiscreteAction) : DiscreteAction = a match {
    case Assign(lhs, rhs) => Assign(mapExpr(lhs), mapExpr(rhs))
    case Create(lhs, name, args) => Create(lhs.map{mapExpr(_)}, name, args.map{mapExpr(_)})
    case Elim(e) => Elim(mapExpr(e))
    case Move(obj, newParent) => Move(mapExpr(obj), mapExpr(newParent))
  }
}
