package acumen
package util

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
    case ParaRhs(cn, vn, f) => ParaRhs(cn, vn, f.map{mapExpr(_)})
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
    case Claim(p) =>
      Claim(mapExpr(p))
    case Hypothesis(s, p) =>
      Hypothesis(s, mapExpr(p))
    case LinearEquations(m, v, b) =>
      LinearEquations(m.map(r => r map mapExpr),
        v map mapExpr,
        b map mapExpr)
  }

  def mapActions(as: List[Action]) : List[Action] = as.map{mapAction(_)}
  
  def mapGroundValue(gv: GroundValue): GroundValue = gv
  
  // Must make a copy of Expr even if there is nothing to do as 
  // Expr has state information associated with it
  def mapExpr(e: Expr) : Expr = (e match {
    case Lit(v) => Lit(mapGroundValue(v))
    case Var(v) => Var(v)
    case Op(name, es) => Op(name, es.map{mapExpr(_)})
    case Dot(a,b) => Dot(mapExpr(a),b)
    case Quest(a,b) => Quest(mapExpr(a), b)
    case ResolvedDot(id, obj, field) => ResolvedDot(id,mapExpr(obj),field)
    case Input(s,i) => Input(mapExpr(s),i)
    case Index(a,idx) => Index(mapExpr(a),idx map mapExpr)
    case ExprVector(l) => ExprVector(l.map{mapExpr(_)})
    case Sum(s, i, col, cond) => Sum(mapExpr(s), i, mapExpr(col), mapExpr(cond))
    case TypeOf(v) => TypeOf(v)
    case ExprInterval(lo, hi) => ExprInterval(mapExpr(lo), mapExpr(hi))
    case ExprIntervalM(mid, pm) => ExprIntervalM(mapExpr(mid), mapExpr(pm))
    case ExprSplitterWeights(i, ws) => ExprSplitterWeights(mapExpr(i), ws map mapExpr)
    case ExprSplitterPoints(ps, keeps) => ExprSplitterPoints(ps map mapExpr, keeps map mapExpr)
    case ExprSplitterN(i, n) => ExprSplitterN(mapExpr(i), mapExpr(n))
    case ExprSplitInterval(i, s) => ExprSplitInterval(mapExpr(i), mapExpr(s))
    case ExprSplitterNormal(m, s, c, n) => ExprSplitterNormal(mapExpr(m), mapExpr(s), mapExpr(c), mapExpr(n))
    case ExprSplitterUniform(lo, hi, c, n) => ExprSplitterUniform(mapExpr(lo), mapExpr(hi), mapExpr(c), mapExpr(n))
    case ExprSplitterBeta(lo, hi, a, b, c, n) => ExprSplitterBeta(mapExpr(lo), mapExpr(hi), mapExpr(a), mapExpr(b), mapExpr(c), mapExpr(n))
    case ExprLet(bindings, e2) => ExprLet(bindings.map{case (n,e) => (n,mapExpr(e))},
                                          mapExpr(e2))
    case Pattern(ps) => Pattern(ps.map{mapExpr(_)})
    case Lambda(vs, f) => e

  }).setPos(e.pos)

  def mapClause(c: Clause) : Clause = c match {
    case Clause(lhs, as, rhs) => Clause(mapGroundValue(lhs), mapExpr(as), mapActions(rhs))
  }

  def mapContinuousAction(a: ContinuousAction) : ContinuousAction = a match {
    case Equation(lhs, rhs) => Equation(mapExpr(lhs), mapExpr(rhs))
    case EquationI(lhs, rhs) => EquationI(mapExpr(lhs), mapExpr(rhs))
    case EquationT(lhs, rhs) => EquationT(mapExpr(lhs), mapExpr(rhs))
    case Assignment(Pattern(ps), rhs) => Assignment(Pattern(ps.map{mapExpr(_)}), mapExpr(rhs))
  }

  def mapDiscreteAction(a: DiscreteAction) : DiscreteAction = a match {
    case Assign(lhs, rhs) => Assign(mapExpr(lhs), mapExpr(rhs))
    case Create(lhs, name, args) => Create(lhs.map{mapExpr(_)}, name, args.map{mapExpr(_)})
    case Elim(e) => Elim(mapExpr(e))
    case Move(obj, newParent) => Move(mapExpr(obj), mapExpr(newParent))
  }
}
