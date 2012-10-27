package acumen

import Errors._
import util.Names._

object Desugarer {

  val self = name("self")
  val parent = name("parent")
  val children = name("children")
  val classf = name("className")

  // maybe that check should be moved to a Checker module
  def check(p: Prog) = {
    val classes = p.defs map (_.name)
    def helper(m: Map[ClassName, Int], cn: ClassName) = m + ((cn, 1 + m.getOrElse(cn, 0)))
    val multiplicities = classes.foldLeft(Map.empty[ClassName, Int])(helper)
    for ((cn, m) <- multiplicities)
      if (m > 1) throw ClassDefinedTwice(cn)
  }

  def desugar(p: Prog): Prog = {
    check(p); Prog(p.defs map (desugar(p, _)))
  }

  def desugar(p: Prog, c: ClassDef): ClassDef =
    c match {
      case ClassDef(cn, fs, is, b) =>
        val (privs, dis) = desugar(p, fs, List(self), is)
        val db = desugar(p, fs ++ privs ++ List(classf, parent, children), List(self), b)
        ClassDef(cn, fs, dis, db)
    }

  def desugar(p: Prog, fs: List[Name], env: List[Name], is: List[Init]): (List[Name], List[Init]) = {
    is match {
      case Nil => (List(), Nil)
      case Init(x, rhs) :: is1 =>
        val drhs = rhs match {
          case NewRhs(cn, es) => NewRhs(cn, es map (desugar(p, fs, env, _)))
          case ExprRhs(e) => ExprRhs(desugar(p, fs, env, e))
        }
        val (xs, dis1) = desugar(p, fs, env, is1)
        (x :: xs, Init(x, drhs) :: dis1)
    }
  }

  def desugar(p: Prog, fs: List[Name], env: List[Name], as: List[Action]): List[Action] =
    (as map (desugar(p, fs, env, _))).flatten

  def desugar(p: Prog, fs: List[Name], env: List[Name], a: Action): List[Action] = {
    val dese = desugar(p, fs, env, _: Expr)
    val desa = desugar(p, fs, env, _: List[Action])
    val desc = desugar(p, fs, env, _: Clause)
    val desca = desugar(p, fs, env, _: ContinuousAction)
    val desda = desugar(p, fs, env, _: DiscreteAction)
    a match {
      case IfThenElse(c, t, e) => List(IfThenElse(dese(c), desa(t), desa(e)))
      case Switch(s, cls) => List(Switch(dese(s), cls map desc))
      case ForEach(x, e, b) => List(ForEach(x, dese(e), (desugar(p, fs, x :: env, b))))
      case Continuously(ca) => desca(ca) map Continuously
      case Discretely(da) => desda(da) map Discretely
    }
  }

  def desugar(p: Prog, fs: List[Name], env: List[Name], e: Expr): Expr = {
    val des = desugar(p, fs, env, _: Expr)
    e match {
      case Lit(gv) => Lit(gv)
      case Var(x) =>
        if (env contains x) Var(x)
        else if (fs contains x) Dot(Var(self), x)
        else throw VariableNotDeclared(x)
      case Op(f, es) => Op(f, es map des)
      case Dot(o, f) => Dot(des(o), f)
      case ExprVector(es) => ExprVector(es map des)
      case Sum(e, i, col, cond) =>
        Sum(desugar(p, fs, i :: env, e), i, des(col), desugar(p, fs, i :: env, cond))
      case TypeOf(cn) =>
        if ((p.defs map (_.name)) contains cn) TypeOf(cn)
        else throw ClassNotDefined(cn)
      case i @ ExprInterval(_, _) => i
    }
  }

  def desugar(p: Prog, fs: List[Name], env: List[Name], e: ContinuousAction): List[ContinuousAction] = {
    val des = desugar(p, fs, env, _: Expr)
    e match {
      case Equation(lhs, rhs) =>
        val dlhs = des(lhs)
        val drhs = des(rhs)
        dlhs match {
          case Dot(o, Name(f, n)) =>
            EquationT(dlhs, drhs) :: (
              for (k <- n until (0, -1))
                yield EquationI(Dot(o, Name(f, k - 1)), Dot(o, Name(f, k)))).toList
          case _ => throw BadPreLhs()
        }
      case EquationI(lhs, rhs) => List(EquationI(des(lhs), des(rhs)))
      case EquationT(lhs, rhs) => List(EquationT(des(lhs), des(rhs)))
    }
  }

  def desugar(p: Prog, fs: List[Name], env: List[Name], e: DiscreteAction): List[DiscreteAction] = {
    val des = desugar(p, fs, env, _: Expr)
    e match {
      case Assign(lhs, rhs) => List(Assign(des(lhs), des(rhs)))
      case Create(lhs, cn, args) =>
        if (!(p.defs map (_.name)).contains(cn))
          throw ClassNotDefined(cn)
        List(Create(lhs map des, cn, args map des))
      case Elim(e) => List(Elim(des(e)))
      case Move(o, p) => List(Move(des(o), des(p)))
    }
  }

  def desugar(p: Prog, fs: List[Name], env: List[Name], e: Clause): Clause =
    e match {
      case Clause(lhs, inv, rhs) => Clause(lhs, inv, desugar(p, fs, env, rhs))
    }

  def run(t: Prog): Prog = desugar(t)
}
