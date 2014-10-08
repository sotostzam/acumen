package acumen

import Errors._
import util.Names._
import Pretty._
sealed abstract class ODETransformMode
case object Local extends ODETransformMode
case object LocalInline extends ODETransformMode
case object TopLevel extends ODETransformMode

object GenSym{
  private var counter = 0;
  def gensym(vs:List[Expr]):Var = {
    counter = counter + 1
    def getNames(ns:List[Expr]):String = {
      ns.map(x => x match{
        case Var(name) => name.x
        case Pattern(ls) => getNames(ls) 
      }).mkString("")
    }
   Var(Name("pattern__"+getNames(vs)+"_"+vs.length.toString+"_"+counter.toString,0)) 
  }  
}
/**
 * @param odeTransformMode Configures the way in which higher-order continuous 
 *        assignments are expanded into systems of first-order continuous 
 *        assignments. When Local, this is done where the highest-order 
 *        continuous assignment occurs. When LocalInline, it is done locally 
 *        and the RHS of the ODE is in-lined into the RHS of the emitted EquationI. 
 *        When TopLevel, it is done once for all variables, at the top level of 
 *        each class.
 */
case class Desugarer(odeTransformMode: ODETransformMode) {
  import GenSym._
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
    check(p); 
    println(pprint(p))
    Prog(p.defs map (desugar(p, _)))
  }

  def desugar(p: Prog, c: ClassDef): ClassDef ={
    // Create the initRhs value for pattern variable
    def patternInitRhs(p:Pattern):Expr = {
      val zero = Lit(GInt(0))
      ExprVector(p.ps.map(x => x match{
        case Var(_) => zero
        case pattern:Pattern => patternInitRhs(pattern)
      })) 
    }
    c match {
      case ClassDef(cn, fs, is, b) =>      
        val (privs, dis) = desugarInit(p, fs, List(self), is)
        val topLevelODESystem = odeTransformMode match {
          case Local | LocalInline => Nil
          case TopLevel => highestOrderNames(fs ++ privs).map(Dot(Var(self), _))
                                                         .flatMap(firstOrderSystem)
                                                         .map(Continuously)
        }
        val (as,newVars) = desugar(p, fs ++ privs ++ List(classf, parent, children), List(self), b)
        val newInits = for((lhs,p) <- newVars) yield 
        				 Init(lhs.name, ExprRhs(patternInitRhs(p)))
        val db = as ++ topLevelODESystem
        val cd = ClassDef(cn, fs, dis:::newInits, db)
        cd      
    }
  }

  def desugarInit(p: Prog, fs: List[Name], env: List[Name], is: List[Init]): (List[Name], List[Init]) = {
    is match {
      case Nil => (List(), Nil)
      case Init(x, rhs) :: is1 =>
        val drhs = rhs match {
          case NewRhs(e, es) => NewRhs(desugar(p, fs, env, e), es map (desugar(p, fs, env, _)))
          case ExprRhs(e) => ExprRhs(desugar(p, fs, env, e))
        }
        val (xs, dis1) = desugarInit(p, fs, env, is1)
        (x :: xs, Init(x, drhs) :: dis1)
    }
  }

  def desugar(p: Prog, fs: List[Name], env: List[Name], as: List[Action]): (List[Action], List[(Var,Pattern)]) ={
    val asls = as map (desugar(p, fs, env, _)) unzip
    val vars = asls._2.flatten
    (asls._1.flatten, vars)  
  }

  def desugar(p: Prog, fs: List[Name], env: List[Name], a: Action): (List[Action],List[(Var,Pattern)]) = {
    val dese = desugar(p, fs, env, _: Expr)
    val desa = desugar(p, fs, env, _: List[Action])
    val desc = desugar(p, fs, env, _: Clause)
    val desca = desugar(p, fs, env, _: ContinuousAction)
    val desda = desugar(p, fs, env, _: DiscreteAction)
    a match {
      case IfThenElse(c, t, e) => 
        (List(IfThenElse(dese(c), desa(t)._1, desa(e)._1)), desa(t)._2 ::: desa(e)._2) 
      case Switch(s, cls) => (List(Switch(dese(s), cls map desc)), List())
      case ForEach(x, e, b) =>
        val as = desugar(p, fs, x :: env, b)
        (List(ForEach(x, dese(e), as._1)), as._2)
      case Continuously(ca) => val temp = desca(ca); (temp._1 map Continuously, temp._2)
      case Discretely(da) => (desda(da) map Discretely, List())
      case Claim(e) => (List(Claim(dese(e))), List())
      case Hypothesis(s, e) => (List(Hypothesis(s, dese(e))), List())
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
      case Op(f, es) =>
        def mkIndexOf(n0: Expr) = es.foldLeft(n0)((n,e) => Index(n, des(e)))
        if (env.contains(f)) mkIndexOf(Var(f))
        else if (fs contains f) mkIndexOf(Dot(Var(self), f))
        else Op(f, es map des)
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

  def desugar(p: Prog, fs: List[Name], env: List[Name], e: ContinuousAction): (List[ContinuousAction],List[(Var,Pattern)]) = {
    val des = desugar(p, _:List[Name], env, _: Expr)
    def mkEquationT(lhs:Expr, rhs:Expr,newNames:List[Name]):List[ContinuousAction] = {
      val dlhs = des(fs:::newNames,lhs)
       val drhs = des(fs:::newNames,rhs)
        dlhs match {
          case dot: Dot =>
            EquationT(dlhs, drhs) :: 
              (odeTransformMode match { 
                case Local => firstOrderSystem(dot)
                case LocalInline => firstOrderSystemInline(dot,drhs)
                case TopLevel => Nil })
        }
    }
    
    def patternMatch(pattern:Pattern, e:Expr, newNames:List[Name]):(List[ContinuousAction], List[(Var,Pattern)]) = {
      pattern.ps match{
        case Var(x) :: Nil => (mkEquationT(Var(x), des(fs:::newNames,e),newNames), List())
        case ls =>
          val newVar:Var = gensym(ls)
          val lsResult = ls.map(x => x match{
            case Var(_) => patternMatch(Pattern(List(x)),Op(newVar.name,List(Lit(GInt(ls.indexOf(x))))),newVar.name::newNames)
            case p:Pattern => patternMatch(p,Op(newVar.name,List(Lit(GInt(ls.indexOf(x))))),newVar.name::newNames)
          })         
          (mkEquationT(Var(newVar.name),des(newVar.name ::fs:::newNames,e),List(newVar.name)):::lsResult.map(_._1).flatten, (newVar,pattern)::lsResult.map(_._2).flatten)       
      }
    }
    e match {
      // Pattern matching
      case Assignment(p,rhs) => patternMatch(p,rhs,List()) 
      case Equation(lhs, rhs) => (mkEquationT(lhs,rhs,List()), List())        
      case EquationI(lhs, rhs) => (List(EquationI(des(fs,lhs), des(fs,rhs))), List())
      case EquationT(lhs, rhs) => (List(EquationT(des(fs,lhs), des(fs,rhs))), List())
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
      case Clause(lhs, inv, rhs) => Clause(lhs, desugar(p, fs, env, inv), desugar(p, fs, env, rhs)._1)
    }
  
  def firstOrderSystem(dot: Dot): List[ContinuousAction] = dot match {
    case Dot(o, Name(f, n)) => 
      (for (k <- n until (0, -1))
        yield EquationI(Dot(o, Name(f, k - 1)).setPos(dot.pos), Dot(o, Name(f, k)))).toList
  }

  def firstOrderSystemInline(dot: Dot, rhs: Expr): List[ContinuousAction] = dot match {
    case Dot(o, Name(f, n)) =>
      if (n == 0) Nil
      else EquationI(Dot(o, Name(f, n-1)) setPos dot.pos, rhs) +:
        (for (k <- 0 until n-1)
          yield EquationI(Dot(o, Name(f, k)) setPos dot.pos, Dot(o, Name(f, k + 1)))).toList
  }
  
  def highestOrderNames(ns: List[Name]): List[Name] =
    ns.groupBy(_.x).mapValues(_.maxBy(_.primes)).values.toList

  def run(t: Prog): Prog = desugar(t)
}
