package acumen
package interpreters
package enclosure2015

import Common._
import Pretty.pprint

/** Return type of AST traversal (evalActions) */
case class Changeset
  ( reps:   Set[(CId,CId)]         = Set.empty // reparentings
  , dis:    Set[CollectedAction]     = Set.empty // discrete assignments
  , eqs:    Set[CollectedAction]     = Set.empty // continuous assignments / algebraic equations
  , odes:   Set[CollectedAction]     = Set.empty // ode assignments / differential equations
  , claims: Set[CollectedConstraint] = Set.empty // claims / constraints
  , hyps:   Set[CollectedHypothesis] = Set.empty // hypotheses
  ) {
  def ||(that: Changeset) =
    that match {
      case Changeset.empty => this
      case Changeset(reps1, ass1, eqs1, odes1, claims1, hyps1) =>
        Changeset(reps ++ reps1, dis ++ ass1, eqs ++ eqs1, odes ++ odes1, claims ++ claims1, hyps ++ hyps1)
    }
  override def toString =
    (reps, dis.map(d => d.selfCId + "." + pprint(d.a))
         , eqs.map(d => d.selfCId + "." + pprint(d.a))
         , odes.map(d => d.selfCId + "." + pprint(d.a))
         , claims.map(d => d.selfCId + "." + pprint(d.c))).toString
}
object Changeset {
  def combine[A](ls: Set[Changeset], rs: Set[Changeset]): Set[Changeset] =
    if (ls isEmpty) rs
    else if (rs isEmpty) ls
    // FIXME Handle Changeset.empty case correctly
    else for (l <- ls; r <- rs) yield l || r
  def combine[A](xs: Traversable[Set[Changeset]]): Set[Changeset] =
    xs.foldLeft(Set.empty[Changeset])(combine(_,_))
  def combine[A](xs: Traversable[A], f: A => Set[Changeset]): Set[Changeset] =
    combine(xs map f)
  def logReparent(o:CId, parent:CId): Set[Changeset] =
    Set(Changeset(reps = Set((o,parent))))
  def logAssign(path: Expr, o: CId, a: Action, env: Env): Set[Changeset] =
    Set(Changeset(dis = Set(CollectedAction(path,o,a,env))))
  def logEquation(path: Expr, o: CId, a: Action, env: Env): Set[Changeset] =
    Set(Changeset(eqs = Set(CollectedAction(path,o,a,env))))
  def logODE(path: Expr, o: CId, a: Action, env: Env): Set[Changeset] =
    Set(Changeset(odes = Set(CollectedAction(path,o,a,env))))
  def logClaim(o: CId, c: Expr, env: Env) : Set[Changeset] =
    Set(Changeset(claims = Set(CollectedConstraint(o,c,env))))
  def logHypothesis(o: CId, s: Option[String], h: Expr, env: Env): Set[Changeset] =
    Set(Changeset(hyps = Set(CollectedHypothesis(o,s,h,env))))
  lazy val empty = Changeset()
}
case class CollectedAction(path: Expr, selfCId: CId, a: Action, env: Env) {
  def lhs: ResolvedDot = (a: @unchecked) match {
    case Discretely(Assign(d: ResolvedDot, _))      => d
    case Continuously(EquationT(d: ResolvedDot, _)) => d
    case Continuously(EquationI(d: ResolvedDot, _)) => d
  }
  def rhs: Expr = (a: @unchecked) match {
    case Discretely(x: Assign)      => x.rhs
    case Continuously(x: EquationT) => x.rhs
    case Continuously(x: EquationI) => x.rhs
  }
  override def toString() =
    s"$selfCId.${Pretty pprint (lhs: Expr)} = ${Pretty pprint rhs}"
}
case class CollectedConstraint(selfCId: CId, c: Expr, env: Env)
case class CollectedHypothesis(selfCId: CId, s: Option[String], h: Expr, env: Env)