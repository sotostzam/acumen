package acumen

import Errors._
import util.Names._

// Simplifier: remove Intervals and replace them with the mid point.

object Simplifier {

  def simplify(p: Prog): Prog = {
    Prog(p.defs map (simplify(p, _)))
  }

  def simplify(p: Prog, c: ClassDef): ClassDef =
    c match {
      case ClassDef(cn, fs, is, b) =>
        ClassDef(cn, fs, is map {init: Init => simplify(init)}, simplify(b))
    }

  def simplify(init: Init): Init = {
    init match {case Init(x, rhs) =>
      val drhs = rhs match {
        case NewRhs(cn, es) => NewRhs(cn, es map (simplify(_)))
        case ExprRhs(e) => ExprRhs(simplify(e))
      }
      Init(x, drhs)
    }
  }
  
  def simplify(as: List[Action]) : List[Action] =
    as map {a: Action => simplify(a)}

  def simplify(a: Action): Action = {
    val dese = simplify(_: Expr)
    val desa = simplify(_: List[Action])
    val desc = simplify(_: Clause)
    val desca = simplify(_: ContinuousAction)
    val desda = simplify(_: DiscreteAction)
    a match {
      case IfThenElse(c, t, e) => IfThenElse(dese(c), desa(t), desa(e))
      case Switch(s, cls) => Switch(dese(s), cls map desc)
      case ForEach(x, e, b) => ForEach(x, dese(e), (simplify(b)))
      case Continuously(ca) => Continuously(desca(ca))
      case Discretely(da) => Discretely(desda(da))
      case Claim(c) => Claim(dese(c))
    }
  }

  def simplify(e: Expr): Expr = {
    val des = simplify(_: Expr)
    (e match {
      case Lit(gv) => e
      case Var(x) => e
      case Op(f, es) => Op(f, es map des)
      case Index(f, i) => Index(des(f), des(i))
      case Dot(o, f) => Dot(des(o), f)
      case ExprVector(es) => ExprVector(es map des)
      case Sum(e, i, col, cond) =>
        Sum(simplify(e), i, des(col), simplify(cond))
      case ExprLet(bs,e2) => ExprLet(bs map (b =>(b._1,simplify(b._2))),
                                     simplify(e2))
      case TypeOf(cn) => e
      case ExprInterval(lo, hi) => Op(Name("/",0),List(Op(Name("+",0), List(lo, hi)), Lit(GDouble(2.0))))
      case ExprIntervalM(mid, _) => mid
    }).setPos(e.pos)
  }

  def simplify(e: ContinuousAction): ContinuousAction = {
    val des = simplify(_: Expr)
    e match {
      case EquationI(lhs, rhs) => EquationI(des(lhs), des(rhs))
      case EquationT(lhs, rhs) => EquationT(des(lhs), des(rhs))
      case _ => throw new ShouldNeverHappen
    }
  }

  def simplify(e: DiscreteAction): DiscreteAction = {
    val des = simplify(_: Expr)
    e match {
      case Assign(lhs, rhs) => Assign(des(lhs), des(rhs))
      case Create(lhs, cn, args) => Create(lhs map des, cn, args map des)
      case Elim(e) => Elim(des(e))
      case Move(o, p) => Move(des(o), des(p))
    }
  }

  def simplify(e: Clause): Clause =
    e match {
      case Clause(lhs, inv, rhs) => Clause(lhs, inv, simplify(rhs))
    }

  def run(t: Prog): Prog = simplify(t)
}
