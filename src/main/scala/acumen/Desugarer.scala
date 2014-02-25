package acumen

import Errors._
import util.Names._

sealed abstract class ODETransformMode
case object Local extends ODETransformMode
case object TopLevel extends ODETransformMode

/**
 * @param odeTransformMode Configures the way in which higher-order continuous 
 *        assignments are expanded into systems of first-order continuous 
 *        assignments. When true, this is done where the highest-order 
 *        continuous assignment occurs. When false, it is done once for all 
 *        variables, at the top level of each class.
 */
case class Desugarer(odeTransformMode: ODETransformMode = TopLevel) {
  
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
        val topLevelODESystem = odeTransformMode match {
          case Local => Nil
          case TopLevel => highestOrderNames(fs ++ privs).map(Dot(Var(self), _))
                                                         .flatMap(firstOrderSystem)
                                                         .map(Continuously)
        }
        val db = desugar(p, fs ++ privs ++ List(classf, parent, children), List(self), b) ++ topLevelODESystem
        val cd = ClassDef(cn, fs, dis, db)
        cd
    }

  def desugar(p: Prog, fs: List[Name], env: List[Name], is: List[Init]): (List[Name], List[Init]) = {
    is match {
      case Nil => (List(), Nil)
      case Init(x, rhs) :: is1 =>
        val drhs = rhs match {
          case NewRhs(e, es) => NewRhs(desugar(p, fs, env, e), es map (desugar(p, fs, env, _)))
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
      case Claim(e) => List(Claim(dese(e)))
    }
  }

  def desugar(p: Prog, fs: List[Name], env: List[Name], e: Expr): Expr = {
    val des = desugar(p, fs, env, _: Expr)
    (e match {
      case Lit(gv) => Lit(gv)
      case Var(x) =>
        if (env.contains(x) || (p.defs map (_.name)).contains(ClassName(x.x))) Var(x)
        else if (fs contains x) Dot(Var(self), x)
        else if (Constants.predefined.contains(x.x)) Constants.predefined(x.x)
      else throw VariableNotDeclared(x).setPos(e.pos)
      case Call(f, es) =>
        def mkIndexOf(n0: Expr) = es.foldLeft(n0)((n,e) => Index(n, des(e)))
        f match {
          case Var(n) => if (env.contains(n)) mkIndexOf(f)
                         else if (fs contains n) mkIndexOf(Dot(Var(self), n))
                         else Op(n, es map des)
          case e => mkIndexOf(des(e))
        }
      case Op(f,es) => Op(f, es map des)
      case Index(e,i) => Index(des(e),des(i))
      case Dot(o, f) => Dot(des(o), f)
      case ExprVector(es) => ExprVector(es map des)
      case Sum(e, i, col, cond) =>
        Sum(desugar(p, fs, i :: env, e), i, des(col), desugar(p, fs, i :: env, cond))
      case ExprLet(bs,e2) => ExprLet(bs map (b =>(b._1,desugar(p,fs,env,b._2))),
                                     desugar(p,fs,bs.foldLeft(env)((r,b) =>  b._1::r) ,e2))
      case TypeOf(cn) =>
        if ((p.defs map (_.name)) contains cn) TypeOf(cn)
        else throw ClassNotDefined(cn).setPos(e.pos)
      case i @ ExprInterval(_, _) => i
      case i @ ExprIntervalM(_, _) => i
    }).setPos(e.pos)
  }

  def desugar(p: Prog, fs: List[Name], env: List[Name], e: ContinuousAction): List[ContinuousAction] = {
    val des = desugar(p, fs, env, _: Expr)
    e match {
      case Equation(lhs, rhs) =>
        val dlhs = des(lhs)
        val drhs = des(rhs)
        dlhs match {
          case dot: Dot =>
            EquationT(dlhs, drhs) :: 
              (odeTransformMode match { 
                case Local => firstOrderSystem(dot)
                case TopLevel => Nil })
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
      case Create(lhs, c, args) =>
        List(Create(lhs map des, desugar(p, fs, env, c), args map des))
      case Elim(e) => List(Elim(des(e)))
      case Move(o, p) => List(Move(des(o), des(p)))
    }
  }

  def desugar(p: Prog, fs: List[Name], env: List[Name], e: Clause): Clause =
    e match {
      case Clause(lhs, inv, rhs) => Clause(lhs, inv, desugar(p, fs, env, rhs))
    }
  
  def firstOrderSystem(dot: Dot): List[ContinuousAction] = dot match {
    case e@Dot(o, Name(f, n)) => 
      (for (k <- n until (0, -1))
        yield EquationI(Dot(o, Name(f, k - 1)).setPos(e.pos), Dot(o, Name(f, k)).setPos(e.pos))).toList
  }
  
  def highestOrderNames(ns: List[Name]): List[Name] =
    ns.groupBy(_.x).mapValues(_.maxBy(_.primes)).values.toList

  def run(t: Prog): Prog = desugar(t)
}
