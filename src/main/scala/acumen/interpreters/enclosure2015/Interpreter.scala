package acumen
package interpreters
package enclosure2015

import scala.annotation.tailrec
import scala.Stream._
import scala.collection.immutable.{
  HashMap, MapProxy
}
import util.ASTUtil.{
  dots, checkContinuousAssignmentToSimulator, checkNestedHypotheses, op, substitute
}
import enclosure2015.Common._
import interpreters.Common._
import Errors._
import Pretty.pprint
import ui.tl.Console
import util.{
  Canonical, Random
}
import util.Canonical._
import util.Conversions.{
  extractDouble, extractDoubles, extractId, extractInterval, extractIntervals
}
import enclosure.{
  Box, Constant, Contract, Expression, Field, Interval, Rounding,
  Abs, Sin, Cos, Tan, ACos, ASin, ATan, Exp, Log, Log10, Sqrt, 
  Cbrt, Ceil, Floor, Sinh, Cosh, Tanh, Signum, Plus, Negate, 
  Multiply, Pow, Divide, ATan2, Min, Max
}
import acumen.interpreters.enclosure.Transcendentals
import acumen.interpreters.enclosure.ivp.{
  LohnerSolver, PicardSolver
}
import scala.util.parsing.input.Position
import scala.reflect.runtime.universe.typeTag
import acumen.interpreters.enclosure.Interval

/**
 * Reference implementation of direct enclosure semantics.
 * The CStore type is used for two purposes:
 * 
 *  1) To represent an enclosure of all variables in the model, valid over 
 *     [s.time, s.time + s.timeStep], where s is the single simulator object
 *     of the CStore.
 *  2) To represent initial conditions, valid at the start of a time interval.
 *     In this case, all variables v of the CStore, that are of enclosure type 
 *     (v is an instance of GEnclosure) have v.start = v.range = v.end.  
 *   
 * The Store type EnclosureAndBranches consists of:
 * 
 *  1) A single enclosure (this is what is actually plotted in the user interface) 
 *     and for storing simulator variables (e.g. time, timeStep).
 *  2) A List[InitialCondition], which represents the initial conditions of all 
 *     branches of the computation. 
 *     
 * @param Use Lohner-based ODE solver instead of a pure Picard solver
 */
case class Interpreter(contraction: Boolean) extends CStoreInterpreter {
  
  type Store = EnclosureAndBranches
  def repr(st:Store) = st.enclosure.cStore
  def fromCStore(st: CStore, root: CId): Store = {
    val e = solverBase(st) initializeEnclosure st
    EnclosureAndBranches(e, InitialCondition(e, Epsilon, StartTime) :: Nil) 
  }
  override val visibleParameters: Map[String, CValue] =
    enclosure2015.Common.Parameters.defaults.filter(_._2._1).map{ case (n,(_,v)) => (n,v) } 

  /* Constants */
  
  private val legacyParameters = enclosure.Parameters.default.copy(interpreter = Some(enclosure.Interpreter.EVT))
  private implicit val rnd = Rounding(legacyParameters)

  /* Types */

  sealed abstract trait InitialConditionTime
  case object StartTime extends InitialConditionTime
  case object UnknownTime extends InitialConditionTime
  case class InitialCondition(enclosure: Enclosure, evolution: Evolution, time: InitialConditionTime)
  class EnclosureAndBranches(val enclosure: Enclosure, val branches: List[InitialCondition])
  object EnclosureAndBranches{
    def apply(e: Enclosure, bs: List[InitialCondition]): EnclosureAndBranches = 
      new EnclosureAndBranches(e.countVariables, bs) 
    def apply(e: List[Enclosure], bs: List[InitialCondition]): EnclosureAndBranches = 
      new EnclosureAndBranches(e.reduce(_ /\ _).countVariables, bs)
  }
  
  /** Represents a sequence of Changesets without consecutive repeated flows. */
  case class Evolution(changes: List[Changeset]) {
    def head: Changeset = changes.head
    def nonEmpty: Boolean = changes.nonEmpty
    /** Extend this evolution by cs, unless cs is a flow and equal to changes.head. */
    def ::(cs: Changeset): Evolution = 
      if (changes.nonEmpty && isFlow(cs) && cs == changes.head) this else Evolution(cs :: changes)
  }
  val Epsilon = Evolution(Nil)
  
  /** Return type of AST traversal (evalActions) */
  case class Changeset
    ( reps:   Set[(CId,CId)]           = Set.empty // reparentings
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
  
  import Changeset._

  /* Set-up */
  
  /**
   * Lift all numeric values to ConstantEnclosures (excluding the Simulator object) 
   * and all strings to uncertain strings.
   */
  def liftToUncertain(p: Prog): Prog =
    new util.ASTMap {
      override def mapExpr(e: Expr): Expr = e match {
        case Lit(GBool(b))          => Lit(if (b) CertainTrue else CertainFalse)
        case Lit(GInt(i))           => Lit(GConstantRealEnclosure(i))
        case Lit(GDouble(d))        => Lit(GConstantRealEnclosure(d))
        case Lit(GInterval(i))      => Lit(GConstantRealEnclosure(i))
        case ExprInterval( Lit(lo@(GDouble(_)|GInt(_)))  // FIXME Add support for arbitrary expression end-points
                         , Lit(hi@(GDouble(_)|GInt(_))))    
                                    => Lit(GConstantRealEnclosure(Interval(extractDouble(lo), extractDouble(hi))))
        case ExprInterval(lo,hi)    => sys.error("Only constant interval end-points are currently supported. Offending expression: " + pprint(e))
        case ExprIntervalM(lo,hi)   => sys.error("Centered interval syntax is currently not supported. Offending expression: " + pprint(e))
        case Lit(GStr(s))           => Lit(GStrEnclosure(s))
        case _                      => super.mapExpr(e)
      }
      override def mapClause(c: Clause) : Clause = c match {
        case Clause(GBool(lhs), as, rhs) => Clause(GBoolEnclosure(lhs), mapExpr(as), mapActions(rhs))
        case Clause(GStr(lhs), as, rhs) => Clause(GStrEnclosure(lhs), mapExpr(as), mapActions(rhs))
        case Clause(GInt(lhs), as, rhs) => Clause(GConstantRealEnclosure(lhs), mapExpr(as), mapActions(rhs)) // FIXME Use discrete integer enclosure instead of Real
        case _ => super.mapClause(c)
      }
    }.mapProg(p)
    
  /* Store manipulation */
  
  /** 
   * Update simulator object of s.enclosure.
   * NOTE: Simulators objects of st.branches are not affected.
   */
  def setInSimulator(f:Name, v:CValue, s:Store): Store = {
    def helper(e: Enclosure) = e.setObjectField(e.simulatorId, f, v)
    EnclosureAndBranches(helper(s.enclosure), s.branches.map(ic => ic.copy(enclosure = helper(ic.enclosure))))
  }
    
  /** Update simulator parameters in st.enclosure with values from the code in p. */
  def updateSimulator(p: Prog, st: Store): Store = {
    val paramMap = p.defs.find(_.name == cmain).get.body.flatMap {
      case Discretely(Assign(Dot(Dot(Var(`self`), `magicf`), pn @ Name(param, _)), rhs)) => 
        List((pn, rhs match {
          case ExprVector(rhsVector) => 
            VVector(rhsVector.map{ case Lit(l) => VLit(l) })
          case Lit(v @ (GStr(_) | GBool(_) | GDouble(_) | GInt(_) | GInterval(_))) => 
            VLit(v)
          case e: Expr =>
            evalExpr(e, Env(self -> VObjId(Some(st.enclosure.mainId))), st.enclosure) match {
              case v @ VLit(ge: GEnclosure[_]) if !ge.isThin =>
                val errorMessage = s"$param evaluates to an uncertain value $ge. Please use an expression that evaluates to an exact value."
                throw new InvalidSimulatorParameterValue(
                  if (pn != timeStep) errorMessage 
                  else errorMessage + " Note that timeStep must be a power of 2.")
              case VLit(GConstantRealEnclosure(i)) => VLit(GDouble(i.loDouble))             
              case VLit(GBoolEnclosure(s)) => VLit(GBool(s.head))             
              case VLit(GStrEnclosure(s)) => VLit(GStr(s.head))             
            }
        }))
      case _ => Nil
    }.toMap
    paramMap.foldLeft(st){ case (stTmp, (p,v)) => setInSimulator(p, v, stTmp) }
  }
  
  def setTime(d:Double, s:Store) = setInSimulator(time, VLit(GDouble(d)), s)
  def setTimeStep(d:Double, s:Store) = setInSimulator(timeStep, VLit(GDouble(d)), s)
  
  def setResultType(t:ResultType, s:Store) = setInSimulator(resultType, VResultType(t), s)
    
  /** Get a fresh object id for a child of parent */
  def freshCId(parent:Option[CId], e: Enclosure) : (CId, Enclosure) = parent match {
    case None => (CId.nil, e)
    case Some(p) =>
      val VLit(GInt(n)) = e.getObjectField(p, nextChild)
      val st1 = e.setObjectField(p, nextChild, VLit(GInt(n+1)))
      (n :: p, st1)
  }
  
  /** Transfer parenthood of a list of objects, whose ids are "cs", to address p */
  def reparent(certain: Boolean, cs:List[CId], p:CId) : Set[Changeset] =
    cs.toSet flatMap (logReparent( _:CId,p))
    
  /** Create an env from a class spec and init values */
  def mkObj(c:ClassName, p:Prog, prt:Option[CId], 
            paramvs:List[CValue], st: Enclosure, childrenCounter:Int = 0) : (CId, Enclosure) = {
    val cd = classDef(c,p)
    val base = HashMap(
      (classf, VClassName(c)),
      (parent, VObjId(prt)),
      (nextChild, VLit(GInt(childrenCounter))))
    val pub = base ++ (cd.fields zip paramvs)

    // the following is just for debugging purposes:
    // the type system should ensure that property
    if (cd.fields.length != paramvs.length) 
      throw ConstructorArity(cd, paramvs.length)
  
    /* change [Init(x1,rhs1), ..., Init(xn,rhsn)]
       into   ([x1, ..., xn], [rhs1, ..., rhsn] */
    def helper(p:(List[Name],List[InitRhs]),i:Init) = 
      (p,i) match { case ((xs,rhss), Init(x,rhs)) => (x::xs, rhs::rhss) }
    val (privVars, constrs) = {
      val (xs,ys) = cd.priv.foldLeft[(List[Name],List[InitRhs])]((Nil,Nil))(helper)
      (xs.reverse, ys.reverse)
    }
       
    val (fid, st1) = freshCId(prt, st)
    val st2 = st1.setObject(fid, pub)
    val (vs, st3) = 
      constrs.foldLeft((List.empty[CValue],st2)) 
        { case ((vsTmp, stTmp), NewRhs(e,es)) =>
            val ve = evalExpr(e, Env(self -> VObjId(Some(fid))), stTmp) 
            val cn = ve match {case VClassName(cn) => cn; case _ => throw NotAClassName(ve)}
            val ves = es map (evalExpr(_, Env(self -> VObjId(Some(fid))), stTmp))
            val (oid, stTmp1) = mkObj(cn, p, Some(fid), ves, stTmp)
            (VObjId(Some(oid)) :: vsTmp, stTmp1)
          case ((vsTmp, stTmp), ExprRhs(e)) =>
            val ve = evalExpr(e, Env(self -> VObjId(Some(fid))), stTmp)
            (ve :: vsTmp, stTmp)
        }
        val priv = privVars zip vs.reverse 
        // new object creation may have changed the nextChild counter
        val newpub = st3(fid)
        val st4 = st3.setObject(fid, newpub ++ priv)
    (fid, st4)
  }

  /* Interpreter */

  /** Evaluate e in the scope of env for definitions p with current store st */
  def evalExpr(e:Expr, env:Env, enc:EStore) : CValue = {
    def eval(env:Env, e:Expr) : CValue = try {
	    e match {
  	    case Lit(i)         => VLit(i)
        case ExprInterval(lo,hi) => 
          VLit(GConstantRealEnclosure(extractInterval(evalExpr(lo, env, enc)) /\ extractInterval(evalExpr(hi, env, enc))))
        case ExprVector(l)  => VVector (l map (eval(env,_)))
        case Var(n)         => env.get(n).getOrElse(VClassName(ClassName(n.x)))
        case Index(v,i)     => evalIndexOp(eval(env, v), i.map(x => eval(env, x)))
        case Dot(o,f) =>
          val id = extractId(evalExpr(o,env,enc))
          env.get(f).getOrElse(enc.getObjectField(id, f))
        /* e.f */
        case ResolvedDot(id, o, f) =>
          if (f == Name("children",0))
          /* In order to avoid redundancy and potential inconsistencies, 
             each object has a pointer to its parent instead of having 
             each object maintain a list of its children. This is why the childrens'
             list has to be computed on the fly when requested. 
             An efficient implementation wouldn't do that. */
            VList(enc.childrenOf(id) map (c => VObjId(Some(c))))
          else
            enc.getObjectField(id, f)
        /* FIXME:
           Could && and || be expressed in term of ifthenelse ? 
           => we would need ifthenelse to be an expression  */
        /* x && y */
        case Op(Name("&&", 0), x :: y :: Nil) =>
          val VLit(vx) = eval(env, x)
          val VLit(vy) = eval(env, y)
          VLit(if      (vx == CertainFalse || vy == CertainFalse) CertainFalse
               else if (vx == CertainTrue  && vy == CertainTrue)  CertainTrue
               else                                               Uncertain)
        /* x || y */
        case Op(Name("||", 0), x :: y :: Nil) =>
          val VLit(vx) = eval(env, x)
          val VLit(vy) = eval(env, y)
          VLit(if      (vx == CertainTrue  || vy == CertainTrue)  CertainTrue
               else if (vx == CertainFalse && vy == CertainFalse) CertainFalse
               else                                               Uncertain)
        /* op(args) */
        case Op(Name(op,0),args) =>
          evalOp(op, args map (eval(env,_)))
        case TypeOf(cn) =>
          VClassName(cn)
        case ExprLet(bs,e) =>
          val eWithBindingsApplied =
            bs.foldLeft(env){
              case(r, (bName, bExpr)) =>
                r + (bName -> eval(env, bExpr))
            }
            eval(eWithBindingsApplied, e)
      }
    } catch {
      case err: PositionalAcumenError => err.setPos(e.pos); throw err
    }
    eval(env,e)
  }.setPos(e.pos)
  
  /** Purely functional operator evaluation at the values level */
  def evalOp[A](op:String, xs:List[Value[_]]) : Value[A] = {
    (op,xs) match {
       case (_, VLit(x:GNumber[A])::Nil) =>
         VLit(unaryGroundOp(op,x))
       case (_,VLit(x:GNumber[A])::VLit(y:GNumber[A])::Nil) =>  
         VLit(binGroundOp(op,x,y))
       case (_,VLit(x)::Nil) =>
         try {
           VLit(unaryGroundOp(op,x))
         }
         catch { case e =>
           e.printStackTrace
           println(xs)
           throw UnknownOperator(op)    
         }
       case (_,VLit(x)::VLit(y)::Nil) =>
         try {
           VLit(binGroundOp(op,GConstantRealEnclosure(extractInterval(x)),GConstantRealEnclosure(extractInterval(x))))
         }
         catch { case e =>
           println(xs)
           throw UnknownOperator(op)    
         }
    }
  }
  
  /** Purely functional unary operator evaluation at the ground values level */
  def unaryGroundOp[V](f:String, vx:GroundValue) = {
    def implem(f: String, x: Interval) = f match {
      case "-"      => -x
      case "abs"    => x.abs
      case "sin"    => x.sin
      case "cos"    => x.cos
      case "tan"    => x.tan
      case "acos"   => x.acos
      case "asin"   => x.asin
      case "atan"   => x.atan
      case "exp"    => x.exp
      case "log"    => x.log
      case "log10"  => x.log10
      case "sqrt"   => x.sqrt
      case "cbrt"   => x.cbrt
      case "ceil"   => x.ceil
      case "floor"  => x.floor
      case "sinh"   => x.sinh
      case "cosh"   => x.cosh
      case "tanh"   => x.tanh
      case "signum" => x.signum
      case _        => throw UnknownOperator(f)
    }
    (f, vx) match {
      case ("not", Uncertain | CertainTrue | CertainFalse) => Uncertain
      case (_, e: GRealEnclosure) => GConstantRealEnclosure(implem(f, e.range))
      case (_, GIntervalFDif(d)) => GIntervalFDif(implemUnaryReal(f, d))
      case (_, GCValueTDif(d @ TAD.TDif(vd,_))) => 
        vd.head match {
          case VLit(_:GConstantRealEnclosure) =>
            GCValueTDif(implemUnaryReal(f, d)(intervalBase.cValueTDifIsReal))
          case VLit(_:GIntervalFDif) =>
            GCValueTDif(implemUnaryReal(f, d)(fDifBase.cValueTDifIsReal))
        }
        
    }
  }
  
  /** Purely functional binary operator evaluation at the ground values level */
  def binGroundOp[V](f:String, vl:GNumber[V], vr:GNumber[V]): GroundValue = {
    def implemInterval(f:String, l:Interval, r:Interval) = f match {
      case "atan2" => Interval.atan2(l, r)
      case "min"   => Interval.min(l, r)
      case "max"   => Interval.max(l, r)
      case _ => implemBinReal(f, l, r)
    }
    // Based on implementations from acumen.interpreters.enclosure.Relation
    def implemBool(f:String, l:Interval, r:Interval): GBoolEnclosure = {
      f match {
        case "<" =>
          if (l lessThan r) // l < r holds over entire time step
            CertainTrue
          else if (r lessThanOrEqualTo l)
            CertainFalse
          else Uncertain
        case ">" =>
          implemBool("<",r,l)
        case "<=" =>
          if (l lessThanOrEqualTo r) // l <= r holds over entire time step
            CertainTrue
          else if (r lessThan l)
            CertainFalse
          else Uncertain
        case ">=" => 
          implemBool("<=",r,l)
        case "==" =>
          if (l == r && l.isThin) CertainTrue
          else if (l disjointFrom r) CertainFalse
          else Uncertain
        case "~=" =>
          if (l == r && l.isThin) CertainFalse
          else if (l disjointFrom r) CertainTrue
          else Uncertain
      }
    }
    (f, vl, vr) match {
      case ("==", lde: GDiscreteEnclosure[_], rde: GDiscreteEnclosure[_]) =>
        if (lde.range == rde.range) CertainTrue
        else if ((lde.range intersect rde.range).nonEmpty) Uncertain
        else CertainFalse
      case ("~=", lde: GDiscreteEnclosure[_], rde: GDiscreteEnclosure[_]) =>
        if (lde.range == rde.range) CertainFalse
        else if ((lde.range intersect rde.range).nonEmpty) CertainTrue
        else Uncertain
      case ("==" | "~=" | ">=" | "<=" | "<" | ">", _, _) =>
        implemBool(f, extractInterval(vl), extractInterval(vr))
      case (_, el: GRealEnclosure, er: GRealEnclosure) => 
        GConstantRealEnclosure(implemInterval(f, el.range, er.range))
      // FIXME Add special case for integer powers of TDif[Int], to avoid lifting to TDif[Double]
      case (_, GIntervalTDif(n), GIntervalTDif(m)) => GIntervalTDif(implemBinReal(f, n, m))
      case (_, GCValueTDif(dl @ TAD.TDif(vdl,_)), GCValueTDif(dr @ TAD.TDif(vdr,_))) => 
        (vdl.head, vdr.head) match {
          case (VLit(_:GConstantRealEnclosure), VLit(_:GConstantRealEnclosure)) => 
            GCValueTDif(implemBinReal(f, dl, dr)(intervalBase.cValueTDifIsReal))
          case (VLit(_:GIntervalFDif), VLit(_:GIntervalFDif)) => 
            GCValueTDif(implemBinReal(f, dl, dr)(fDifBase.cValueTDifIsReal))
      }
    }
  }

  def evalActions(certain:Boolean, path: Expr, as:List[Action], env:Env, p:Prog, st: Enclosure) : Set[Changeset] =
    if (as isEmpty) Set(Changeset.empty) 
    else combine(as, evalAction(certain, path, _: Action, env, p, st))

  def evalAction(certain:Boolean, path: Expr, a:Action, env:Env, p:Prog, st: Enclosure) : Set[Changeset] =
    a match {
      case IfThenElse(c,a1,a2) =>
        val cAndPath = op("&&", c, path)
        val notCAndPath = op("&&", op("not", c), path)
        val VLit(b) = evalExpr(c, env, st)
          b match {
          case CertainTrue => 
            evalActions(certain, cAndPath, a1, env, p, st)
          case CertainFalse => 
            evalActions(certain, notCAndPath, a2, env, p, st)
          case Uncertain =>
            evalActions(false, cAndPath, a1, env, p, st) union evalActions(false, notCAndPath, a2, env, p, st) 
          case _ => sys.error("Non-boolean expression in if statement: " + pprint(c))
        }
      case Switch(mode, cs) =>
        val modes = cs map { case Clause(lhs,claim,rhs) =>
            val (inScope, stmts) = (evalExpr(op("==", mode, Lit(lhs)), env, st): @unchecked) match {
              case VLit(Uncertain)    => (Uncertain, evalActions(false, path, rhs, env, p, st)) // FIXME Add op("==", mode, Lit(lhs)) to path
              case VLit(CertainTrue)  => (CertainTrue, evalActions(certain, path, rhs, env, p, st)) // FIXME Add op("!=", mode, Lit(lhs)) to path
              case VLit(CertainFalse) => (CertainFalse, Set(Changeset.empty))
            }
          (lhs, inScope, claim match {
            case Lit(CertainTrue) => stmts
            case _ => combine(stmts :: logClaim(selfCId(env), claim, env) :: Nil)
          })
        }
        val (allCertFalse, in, uncertain) = modes.foldLeft((true, Set.empty[Changeset], Set.empty[Changeset])) {
          case (iu @ (a, i, u), (_, gub, scs)) => (gub: @unchecked) match {
            case CertainTrue  => (false, combine(i, scs), u)
            case Uncertain    => (false, i, u union scs)
            case CertainFalse => (true && a, i, u)
          }
        }
        if (allCertFalse) {
          val VLit(gv) = evalExpr(mode, env, st)
          throw NoMatch(gv).setPos(mode.pos)
        }
        combine(in, uncertain)
      case Discretely(da) =>
        evalDiscreteAction(certain, path, da, env, p, st)
      case Continuously(ca) =>
        evalContinuousAction(certain, path, ca, env, p, st) 
      case Claim(c) =>
        logClaim(selfCId(env), c, env)
      case Hypothesis(s, e) =>
        if (path == Lit(CertainTrue)) // top level action
          logHypothesis(selfCId(env), s, e, env)
        else 
          throw internalPosError("Hypothesis statements are only allowed on the top level.", e.pos)
      case ForEach(n, col, body) => //TODO Add support for for-each statements
        throw internalPosError("For-each statements are not supported in the Enclosure 2015 semantics.", col.pos)
    }

  /** 
   * Convert Dot d to a ResolvedDot rd by looking up the CId (valid in st) 
   * corresponding to its obj in env. Substitue rd for d in rhs and return 
   * rd together with the resulting expression.  
   * 
   * Note: Checks that the access implied by d is OK. 
   */
  def resolve(d: Dot, rhs: Expr, env: Env, e: Enclosure): (ResolvedDot, Expr) = {
    val id = extractId(evalExpr(d.obj, env, e))
    e.checkAccessOk(id, env, d.obj)
    val rd = ResolvedDot(id, d.obj, d.field)
    (rd, substitute(d, rd, rhs))
  }
 
  def evalDiscreteAction(certain:Boolean, path: Expr, a:DiscreteAction, env:Env, p:Prog, st: Enclosure) : Set[Changeset] =
    a match {
      case Assign(Dot(o @ Dot(Var(`self`),`magicf`), n), e) =>
        Set.empty // TODO Ensure that this does not cause trouble down the road 
      case Assign(e @ Dot(o, _), rhs) =>
        /* Schedule the discrete assignment */
        val (rd, rRhs) = resolve(e, rhs, env, st)
        logAssign(path, rd.id, Discretely(Assign(rd,rRhs)), env)
      /* Basically, following says that variable names must be 
         fully qualified at this language level */
      case c: Create =>
        throw internalPosError("The 2015 Enclosure semantics does not support create statements in the always section.", c.pos)
      case Assign(_,_) => 
        throw BadLhs()
    }

  def evalContinuousAction(certain:Boolean, path: Expr, a:ContinuousAction, env:Env, p:Prog, st: Enclosure) : Set[Changeset] = 
    a match {
      case EquationT(Dot(e: Dot, n), rhs) =>
        val (rd, rRhs) = resolve(e, rhs, env, st)
        logEquation(path, rd.id, Continuously(EquationT(rd, rRhs)), env)
      case EquationT(e @ Dot(o, _), rhs) =>
        val (rd, rRhs) = resolve(e, rhs, env, st)
        logEquation(path, rd.id, Continuously(EquationT(rd, rRhs)), env)
      case EquationI(e @ Dot(o, _), rhs) =>
        val (rd, rRhs) = resolve(e, rhs, env, st)
        logODE(path, rd.id, Continuously(EquationI(rd, rRhs)), env)
      case _ =>
        throw ShouldNeverHappen() // FIXME: enforce that with refinement types
    }
  
  def evalStep(p:Prog, e: Enclosure, rootId:CId) : Set[Changeset] = {
    val cl = e.getCls(rootId)
    val as = classDef(cl, p).body
    val env = Env((self, VObjId(Some(rootId))))
    evalActions(true, Lit(CertainTrue), as, env, p, e)
  }

  /**
   * Outer loop. Iterates f from the root to the leaves of the tree formed 
   * by the parent-children relation.
   */
  def iterate(f: CId => Set[Changeset], root: CId, e: Enclosure): Set[Changeset] = {
    val r = f(root)
    val cs = e childrenOf root
    if (cs.isEmpty) r else combine(r, combine(cs, iterate(f, _:CId, e)))
  }
  
  /* Main simulation loop */  

  def init(prog: Prog) : (Prog, Store, Metadata) = {
    prog.defs.foreach(d => checkValidAssignments(d.body))
    checkNestedHypotheses(prog)
    checkContinuousAssignmentToSimulator(prog)
    // Initialize as affine enclosure to correctly initialize non-ODE variables.
    // This dynset will be converted to the type used by the default and 
    // user-selected integrators at the first call to continuousEncloser. 
    def initializeEnclosure = picardBase.initializeEnclosure(_)
    val cprog = CleanParameters.run(prog, CStoreInterpreterType)
    val cprog1 = makeCompatible(cprog)
    val enclosureProg = liftToUncertain(cprog1)
    val mprog = Prog(magicClass :: enclosureProg.defs)
    val (sd1,sd2) = Random.split(Random.mkGen(0))
    val (id,st1) = 
      mkObj(cmain, mprog, None, List(VObjId(Some(CId(0)))), initializeEnclosure(initStore), 1)
    val st2 = initializeEnclosure(changeParent(CId(0), id, st1.cStore))
    val hyps = st2.toList.flatMap { case (cid, co) =>
      mprog.defs.find(_.name == st2.getCls(cid)).get.body.flatMap {
        case Hypothesis(s, e) =>
          CollectedHypothesis(cid, s, e, Env(self -> VObjId(Some(cid)))) :: Nil
        case _ => Nil
    }}
    val st2E = fromCStore(st2.cStore)
    val st2U = updateSimulator(mprog, st2E)
    val md = testHypotheses(st2U.enclosure, 0, 0, mprog, NoMetadata)
    (mprog, st2U, md)
  }
  
  /** Remove unsupported declarations and statements from the AST. */
  def makeCompatible(p: Prog): Prog = {
    def action(a: Action): List[Action] = a match {
      case Continuously(EquationT(Dot(_, `_3D` | `_3DView`), _)) => Nil
      case IfThenElse(c, t, e) => IfThenElse(c, t flatMap action, e flatMap action) :: Nil
      case Switch(s, cs) => Switch(s, cs map { case Clause(l, a, r) => Clause(l, a, r flatMap action) }) :: Nil
      case ForEach(i, c, b) => ForEach(i, c, b flatMap action) :: Nil
      case _ => a :: Nil
    }
    def priv(i: Init): List[Init] = i match {
      case Init(`_3D` | `_3DView`, _) => Nil
      case _ => i :: Nil
    }
    Prog(p.defs.map(d => d.copy( priv = d.priv.flatMap(priv)
                               , body = d.body.flatMap(action))))
  }
  
  lazy val initStore = Parser.run(Parser.store, initStoreTxt.format("#0"))
  lazy val initStoreTxt: String = 
    s"""#0.0 { className = Simulator, parent = %s, nextChild = 0, variableCount = 0, 
               outputRows = "All", continuousSkip = 0, resultType = @Initial, 
               ${ Common.Parameters.defaults.map{ case (pn, (vis, pv)) => pn + "=" + pprint(pv)}.mkString(",") } }"""

  /** Updates the values of variables in xs (identified by CId and Dot.field) to the corresponding CValue. */
  def applyAssignments(xs: List[(CId, Dot, CValue)], st: Enclosure): Enclosure =
    xs.foldLeft(st)((stTmp, a: (CId, Dot, CValue)) => stTmp.setObjectField(a._1, a._2.field, a._3))

  def step(p: Prog, st: Store, md: Metadata): StepRes = {
    implicit val parameters = Common.Parameters(st.enclosure.cStore)
    if (st.enclosure.getTime >= st.enclosure.getEndTime && st.enclosure.getResultType == FixedPoint) {
      Done(md, st.enclosure.getEndTime)
    }
    else {
      val tNow = st.enclosure.getTime
      val (tNext, resType) = st.enclosure.getResultType match {
        case Continuous | Initial =>
          (tNow, FixedPoint)
        case FixedPoint =>
          (tNow + st.enclosure.getTimeStep, Continuous)
      }
      val (st1, tNextAdapted, tStepNext) = stepAdaptive(tNow, tNext, p, mergeBranchList(st.branches)) // valid enclosure over T
      val md1 = testHypotheses(st1.enclosure, tNow, tNextAdapted, p, md)
      val st2 = setResultType(resType, st1)
      val st3 = setTime(tNextAdapted, st2)
      val st4 = if (tNow == tNext) st3 else setTimeStep(tStepNext, st3)
      Data(st4, md1)
    }
  }
  
  /** Apply zero or more strategies for merging the branches */
  def mergeBranchList(branches: List[InitialCondition])(implicit parameters: Parameters): List[InitialCondition] =
    parameters.mergeBranches.foldLeft(branches) {
      case (bsTmp, `BranchMergingEvolution`) =>
        bsTmp.groupBy(ic => (ic.evolution, ic.time)).map { case ((m, t), ic) => 
          InitialCondition(ic.map(_.enclosure).reduce(_ /\ _), m, t) }.toList
      case (bsTmp, `BranchMergingOff`) => bsTmp
      case (_, s) => throw new InvalidBranchMergingStrategy(s)
    }
  
  /** Adaptive stepping strategy.
   *  
   *  Try to take a step over the time interval [L,R] = [leftEndPoint, rightEndPoint].
   *  If an event is possible in this interval then:
   *    1. Localize the event time: use bisection to find an interval 
   *       [L', R'] whose end-points are at most simulator.minTimeStep 
   *       outside the end-points of the event time interval. 
   *    2. Take a step over [L', R'].
   *    
   *  The left end-point L' is found recursing with half the time step, 
   *  that is over [L, R+(R-L)/2], until R-L < 2*simulator.minTimeStep. 
   *  
   *  Returns a triple (eab, rightEndPointTight, stepSizeNext):
   *  - eab: EnclosureAndBranches 
   *    Contains the enclosure for prog over the time interval
   *    [leftEndPoint, rightEndPointTight] and initial conditions for the time 
   *    [rightEndPointTight].
   *  - tightRightEndPoint: Double
   *    A time that is at most simulator.minTimeStep later than the event time.
   *  - stepSizeNext: Double
   *    For time intervals without events this is double the width of [L,R] 
   *    until that reaches parameters.maxTimeStep. Otherwise this is (R-L). 
   */
  def stepAdaptive(leftEndPoint: Double, rightEndPoint: Double, prog: Prog, branches: List[InitialCondition])(implicit parameters: Parameters): (EnclosureAndBranches, Double, Double) = {
    require(branches.nonEmpty, "stepAdaptive called with zero branches")
    require(branches.size < parameters.maxBranches, s"Number of branches (${branches.size}) exceeds maximum (${parameters.maxBranches}).")
    Logger.debug(s"stepAdaptive (over ${ Interval(leftEndPoint, rightEndPoint) }, ${branches.size} branches)")
    val (enclosuresNext, branchesNext, isFlow) = stepBranches(leftEndPoint, rightEndPoint, prog, branches) 
    val step = rightEndPoint - leftEndPoint
    val twiceStep = 2 * step
    if (isFlow)
      (EnclosureAndBranches(enclosuresNext, branchesNext), rightEndPoint, if (twiceStep < parameters.maxTimeStep) twiceStep else step)
    else { // possible event
      val halfStep = step / 2
      if (halfStep < parameters.minTimeStep) { // leftEndPoint is close enough to event time interval
        val (eab, timeNext) = findRightEndPoint(parameters.minTimeStep, prog, leftEndPoint, branches)
        (eab, timeNext, step)
      }
      else // get closer to event time interval by halving the step
        stepAdaptive(leftEndPoint, leftEndPoint + halfStep, prog, branches)
    }
  }
  
  /** Find a right bound for the event time interval by:
   *  1. Finding a time after the event time interval by 
   *     doubling the width.
   *  2. Refining this rough estimate using findTightRightEndPoint().
   */
  def findRightEndPoint(width: Double, prog: Prog, leftEndPoint: Double, branches: List[InitialCondition])(implicit parameters: Parameters): (EnclosureAndBranches, Double) = {
    val candidate = leftEndPoint + width
    val (candidateEnclosures, candidateBranches, _) = stepBranches(leftEndPoint, candidate, prog, branches)
    val justAfterCandidate = candidate + parameters.minTimeStep
    // Check if this is a valid right end-point
    val (afterCandidateEnclosures, afterCandidateBranches, candidateIsValid) = 
      stepBranches(candidate, justAfterCandidate, prog, candidateBranches)
    val widthTwice = width * 2
    if (candidateIsValid) { // we have found _a_ right end-point, now find a better one
      val previousCandidate = leftEndPoint + width / 2
      val (rightEndPointTightEAB, rightEndPointTight) =  
        findTightRightEndPoint(previousCandidate, candidate, prog, leftEndPoint, branches, EnclosureAndBranches(candidateEnclosures, candidateBranches))
      (rightEndPointTightEAB, rightEndPointTight)
    } else if (widthTwice >= parameters.maxTimeStep)
      (EnclosureAndBranches(candidateEnclosures, candidateBranches), candidate)
    else
      findRightEndPoint(widthTwice, prog, leftEndPoint, branches)
  }    
  
  /** Finds a time just after the event time interval. 
   *  Returns a pair (eab, rightEndPoint):
   *  - eab: EnclosureAndBranches
   *    Enclosure for prog over [loT, t]
   *  - t: Double
   *    A time at most parameters.minTimeStep after the event time interval.
   *  
   *  Invariant: [lo,hi] is always a bound of right-end point of the event time interval */
  def findTightRightEndPoint( lo: Double, hi: Double
                           , prog: Prog, loT: Double, loTBranches: List[InitialCondition], eabOfPreviousCandidate: EnclosureAndBranches
                           )( implicit parameters: Parameters
                           ): (EnclosureAndBranches, Double) = {
    val width = hi - lo
    if (width <= parameters.minTimeStep)
      (eabOfPreviousCandidate, hi)
    else {
      val midpoint = lo + width / 2
      val (esNextCandidate, bsNextCandidate, midpointHasCrossed) = stepBranches(loT, midpoint, prog, loTBranches)
      val (loNext, hiNext) = if (midpointHasCrossed) (lo, midpoint) else (midpoint, hi)
      findTightRightEndPoint(lo, midpoint, prog, loT, loTBranches, EnclosureAndBranches(esNextCandidate, bsNextCandidate))
    }
  }
  
  /** Combined result of stepping prog over [loT, hiT] for all branches. 
   *  Returns a triple (es,bs,isFlow): 
   *  - es: List[Enclosure]
   *    Enclosures for all branches valid over [loT, hiT].
   *  - bs: List[InitialCondition]
   *    New initial conditions valid at [hiT].
   *  - isFlow: Boolean
   *    True if an event is possible over [loT, hiT] for any branch.
   */
  def stepBranches(loT: Double, hiT: Double, prog: Prog, branches: List[InitialCondition])(implicit parameters: Parameters): (List[Enclosure], List[InitialCondition], Boolean) = {
    val (es, bs) = branches.map{ b =>
      val eab = hybridEncloser(Interval(loT, hiT), prog, b)
      (eab.enclosure, eab.branches)
    }.unzip
    (es, bs.flatten.distinct, es.forall(active(_, prog).size == 1))
  }

  /** Traverse the AST (p) and collect statements that are active given st. */
  def active(e: Enclosure, p: Prog): Set[Changeset] = 
    iterate(evalStep(p, e, _), e.mainId, e)

  /** Ensure that c does not contain duplicate assignments. */
  def checkValidChange(c: Set[Changeset]): Unit = c.foreach{ cs =>
    val contIds = (cs.eqs.toList ++ cs.odes.toList).map(_.lhs)
    val assIds = cs.dis.toList.map(_.lhs)
    checkDuplicateAssingments2014(contIds, DuplicateContinuousAssingment)
    checkDuplicateAssingments2014(assIds, DuplicateDiscreteAssingment)
  }
  
  /**
   * If cs is a flow, ensure that for each variable that has an ODE declared in the private section, 
   * there is an equation in scope at the current time step. This is done by checking that for each 
   * primed field name in each object in st, there is a corresponding CId-Name pair in eqs or odes.
   */
  def checkFlowDefined(prog: Prog, cs: Changeset, e: Enclosure): Unit =
    if (isFlow(cs)) {
      val contNamesActive = (cs.eqs union cs.odes).map { da => (da.lhs.id, da.lhs.field.x) }
      val odeNamesDeclared = prog.defs.map(d => (d.name, (d.fields ++ d.priv.map(_.x)).filter(_.primes > 0))).toMap
      e.foreach {
        case (o, _) =>
          if (o != e.simulatorId)
            odeNamesDeclared.get(e getCls o).map(_.foreach { n =>
              if (!contNamesActive.exists { case (ao, an) => ao == o && an == n.x })
                throw ContinuousDynamicsUndefined(o, n,None, Pretty.pprint(e.getObjectField(o, classf)), e.getTime)
            })
      }
    }
  
  /** Summarize result of evaluating the hypotheses of all objects. */
  def testHypotheses(enc: Enclosure, timeDomainLo: Double, timeDomainHi: Double, p: Prog, old: Metadata): Metadata = {
    def testHypothesesOneChangeset(hs: Set[CollectedHypothesis]) =
      if (hs isEmpty) NoMetadata
      else SomeMetadata(
        (for (CollectedHypothesis(o, s, h, env) <- hs) yield {
          lazy val counterEx = dots(h).toSet[Dot].map(d => d -> (evalExpr(d, env, enc) : GValue))
          (o, enc getCls o, s) -> ((evalExpr(h, env, enc), enc.getResultType) match {
            /* Use TestSuccess as default as it is the unit of HypothesisOutcome.pick 
             * For rigorous interpreters the momentary test is unused (middle element) */
            case (VLit(CertainTrue),  Initial) => (Some(CertainSuccess), Some(TestSuccess), CertainSuccess)
            case (VLit(CertainTrue),        _) => (Some(CertainSuccess), Some(TestSuccess), CertainSuccess)
            case (VLit(Uncertain),    Initial) => (Some(UncertainFailure(timeDomainLo, timeDomainHi, counterEx)), Some(TestSuccess), UncertainFailure(timeDomainLo, timeDomainHi, counterEx))
            case (VLit(Uncertain),          _) => (Some(CertainSuccess), Some(TestSuccess), UncertainFailure(timeDomainLo, timeDomainHi, counterEx))
            case (VLit(CertainFalse), Initial) => (Some(CertainFailure(timeDomainLo, timeDomainHi, counterEx)), Some(TestSuccess), CertainFailure(timeDomainLo, timeDomainHi, counterEx))
            case (VLit(CertainFalse),       _) => (Some(CertainSuccess), Some(TestSuccess), CertainFailure(timeDomainLo, timeDomainHi, counterEx))
          })
      }).toMap, timeDomainLo, timeDomainHi, true, None)
    old combine active(enc, p).map(c => testHypothesesOneChangeset(c.hyps))
                              .reduce[Metadata](_ combine _)
  }
  
  /** 
   * Given an initial condition (branch), computes a valid enclosure 
   * (.enclosure part of the result) for prog over T, as well as a 
   * new set of initial conditions (.branches part of the result) 
   * for the next time interval.
   */
  def hybridEncloser(T: Interval, prog: Prog, branch: InitialCondition)(implicit parameters: Parameters): EnclosureAndBranches = {

 
    // Process a list of ICs   
    @tailrec def enclose
      ( pwlW:  List[InitialCondition] // Waiting ICs, the statements that yielded them and time where they should be used
      , pwlR:  List[Enclosure]        // Enclosure (valid over all of T)
      , pwlU:  List[InitialCondition] // Branches, i.e. states possibly valid at right end-point of T (ICs for next time segment)
      , pwlP:  List[InitialCondition] // Passed states
      , iterations: Int               // Remaining iterations
      ): Store =
      // No more IC to process
      if (pwlW isEmpty)
        EnclosureAndBranches(pwlR.reduce(_ /\ _), if (T.isThin) pwlU else mergeBranchList(pwlU))      
      // Exceeding the maximum iterations per branch
      else if (iterations > parameters.maxIterationsPerBranch)
        sys.error(s"Enclosure computation over $T did not terminate in ${parameters.maxIterationsPerBranch} iterations.")
      // Processing the next IC
      else {
        Logger.trace(s"enclose (${pwlW.size} waiting initial conditions, iteration $iterations)")
        val (ic @ InitialCondition(w, q, t)) :: waiting = pwlW
        // An element may be ignored if it was passed already 
        if (isPassed(ic, pwlP))
          enclose(waiting, pwlR, pwlU, pwlP, iterations + 1)
        else {
          // The set of active ChangeSets
          val wPicard = picardEnclosureSolver convertEnclosure w
          val evolutionConstraints = deduceConstraintsFromEvolution(q, t)
          val wContractedWithEvolution = (if (evolutionConstraints.isEmpty)
            Right(wPicard)
          else
            picardBase.contract(wPicard, evolutionConstraints, prog, evalExpr))
              .fold(sys error "Empty intersection while contracting with guard. " + _, i => i)
          val hw = active(wPicard, prog) intersect active(wContractedWithEvolution, prog)
          checkValidChange(hw)
          if (q.nonEmpty && isFlow(q.head) && Set(q.head) == hw && t == UnknownTime)
            sys error "Model error!" // Repeated flow, t == UnknownTime means w was created in this time step
          // Process the active ChangeSets
          val (newW, newR, newU) = encloseHw(ic, hw)
          
          enclose(waiting ::: newW, pwlR ::: newR ::: List[Common.Enclosure](wContractedWithEvolution), pwlU ::: newU, ic :: pwlP, iterations + 1)
        }
      }
    // Process the active ChangeSets
    def encloseHw
      ( wqt: InitialCondition, hw: Set[Changeset]
      ): (List[InitialCondition], List[Enclosure], List[InitialCondition]) = {
      val InitialCondition(w, qw, t) = wqt
      hw.foldLeft((List.empty[InitialCondition], List.empty[Enclosure], List.empty[InitialCondition])) {
        case ((tmpW, tmpR, tmpU), q) =>
          // q is not a flow
          if (!isFlow(q)) 
            // we process q if T is thin
            if (T.isThin ||
            // or T is not thin, but the time is unknown    
               t == UnknownTime) {
              Logger.trace(s"encloseHw (Not a flow)")
              val wi = obtainInitialCondition(w, qw, t, q).fold(sys error "Empty intersection while contracting with guard. " + _, i => i)
              (InitialCondition(wi(q.dis, evalExpr), q :: qw, t) :: tmpW, tmpR, tmpU)
            }
            // otherwise the non-flow q is not processed
            else
              (tmpW, tmpR, tmpU)

          // q is a flow
          // q is a repeated flow
          else if (t == UnknownTime && qw.nonEmpty && qw.head == q)
            (tmpW, tmpR, tmpU)
          // T is thin => the state will be an initial condition for the next time interval
          else if (T.isThin)
            (tmpW, tmpR, InitialCondition(w, qw, StartTime) :: tmpU)
          // T is not thin, the flow is processed
          else {
            checkFlowDefined(prog, q, w)
            val wi = obtainInitialCondition(w, qw, t, q).fold(sys error "Empty intersection while contracting with guard. " + _, i => i)
            val (range, end) = continuousEncloser(q.odes, q.eqs, q.claims, T, prog, wi)
            val (newW, newU) = handleEvent(q, qw, range, if (t == StartTime) end else range)
            val rangePicard = picardEnclosureSolver convertEnclosure range
            val rangeContracted = picardBase.contract(rangePicard, deduceConstraintsFromChangeset(q), prog, evalExpr).right.get
            (newW ::: tmpW, rangeContracted :: tmpR, newU ::: tmpU)
          }
      }
    }
    
    def deduceConstraintsFromEvolution(qw: Evolution, t: InitialConditionTime): Iterable[CollectedConstraint] = 
      if (qw.nonEmpty && isFlow(qw.head) && t != UnknownTime)
        (qw.head.odes union qw.head.eqs).map(ca => CollectedConstraint(ca.selfCId, ca.path, ca.env)) union qw.head.claims
      else
        List.empty[CollectedConstraint]
    
    def deduceConstraintsFromChangeset(q: Changeset): Iterable[CollectedConstraint] =
      if (isFlow(q)) 
        (q.odes union q.eqs).map(ca => CollectedConstraint(ca.selfCId, ca.path, ca.env)) union q.claims
      else
        q.dis.map(da => CollectedConstraint(da.selfCId, da.path, da.env))
    
    def isQualifiedChange(qw: Evolution, q: Changeset): Boolean =
      (!isFlow(q)) || (!qw.nonEmpty) || (q != qw.head)
        
    def obtainInitialCondition(w: Enclosure, qw: Evolution, t: InitialConditionTime, q: Changeset): Either[String, Enclosure] =  
      if (parameters.intersectWithGuardBeforeReset && (isQualifiedChange(qw, q) || solverBase(w.cStore) == picardBase)) {
   
        val wPicard = picardEnclosureSolver convertEnclosure w
        val evolutionConstraints = deduceConstraintsFromEvolution(qw, t)
        val changesetConstraints = deduceConstraintsFromChangeset(q)
      
        val wContractedWithEvolution = 
          if (evolutionConstraints.isEmpty)
            Right(wPicard)
          else
            picardBase.contract(wPicard, evolutionConstraints, prog, evalExpr)
          
        wContractedWithEvolution match {
          case Right(wContracted) => picardBase.contract(wContracted, changesetConstraints, prog, evalExpr)
          case _ => wContractedWithEvolution
        }
        
      } else Right(w)

    def handleEvent(q: Changeset, qw: Evolution, r: Enclosure, u: Enclosure): (List[InitialCondition], List[InitialCondition]) = {
      val hr = active(r, prog)
      val hu = active(u, prog)
      val changesetConstraints = deduceConstraintsFromChangeset(q)
      val uPicard = picardEnclosureSolver convertEnclosure u
      val uContracted = picardBase.contract(uPicard, changesetConstraints, prog, evalExpr)
      val newQw = q :: qw
      if (noEvent(q, hr, uContracted)) { // no event
        Logger.trace("handleEvent (No event)")
        (Nil, InitialCondition(u, newQw, StartTime) :: Nil)
      } else if (certainEvent(q, hr, hu, uContracted)) { // certain event
        Logger.trace("handleEvent (Certain event)")
        (InitialCondition(r, newQw, UnknownTime) :: Nil, Nil)
      } else { // possible event
        Logger.trace(s"handleEvent (Possible event, |hr| = ${hr.size}, q.ass = {${q.dis.map(Pretty pprint _.a).mkString(", ")}})")
        Logger.trace(s"handleEvent hr.odes = ${hr.map{cs => cs.odes.map(Pretty pprint _.a).mkString(", ")}}, hr.dis = ${hr.map{cs => cs.dis.map(Pretty pprint _.a).mkString(", ")}}")
        (InitialCondition(r, newQw, UnknownTime) :: Nil, InitialCondition(u, newQw, StartTime) :: Nil)
      }
    }
    enclose(branch :: Nil, Nil, Nil, Nil, 0)
  }
  
  /**
   * Returns true if there is only a single change set is active over the current time segment, this 
   * change set contains no discrete assignments and the claim in q is not violated entirely by u.
   */
  def noEvent(q: Changeset, hr: Set[Changeset], uContracted: Either[String,Enclosure]) =
    hr.size == 1 && q.dis.isEmpty && (uContracted match {
      case Left(s) => sys.error("Inconsistent model. A claim was invalidated without any event taking place. " + s) 
      case Right(_) => true
    })
    
  /** Returns true if both:
   *  1) More than one change set is active over the current time segment or q contains an assignment
   *  2) The change sets active at the end-point of the current time segment do not contain q or the  
   *     claim in q violates the enclosure at the end-point of the current time segment. */
  def certainEvent(q: Changeset, hr: Set[Changeset], hu: Set[Changeset], uContracted: Either[String,Enclosure]): Boolean = {
    (hr.size > 1 || q.dis.nonEmpty) && // Some event is possible 
      (!(hu contains q) || uContracted.isLeft) // Some possible event is certain 
  }
  
  /** Returns true if P contains an ic1:  
   *  1) That has the same dynamics (head of evolution) as ic and 
   *  2) Whose enclosure contains that of ic and 
   *  3) Whose time certainty flag is identical. */
  def isPassed(ic: InitialCondition, P: List[InitialCondition]) = 
    P exists (ic1 => ic.evolution.changes.headOption == ic1.evolution.changes.headOption && 
                       ic1.enclosure.contains(ic.enclosure) && 
                         ic1.time == ic.time)
  
  /** Returns true if the discrete assignments in cs are empty or have no effect on s. */
  def isFlow(cs: Changeset) = cs.dis.isEmpty

  /**
   * Compute enclosures (range and end-time) for the system of equations 
   * corresponding to odes and eqs put together.
   * 
   * NOTE: In the process, eqs are in-lined into odes in order to obtain
   *       an explicit system of ODEs. Values for the LHS of eqs are then
   *       obtained by evaluating the RHS in the ODE solutions over T.
   *       This approach to supporting mixed differential and algebraic
   *       equations implies that algebraic loops in the combined system
   *       are not allowed.
   */
  def continuousEncloser
    ( odes: Set[CollectedAction]
    , eqs: Set[CollectedAction]
    , claims: Set[CollectedConstraint]
    , T: Interval
    , p: Prog
    , enc: Enclosure
    )( implicit parameters: Parameters
    ): (Enclosure, Enclosure) = {
    solverBase(enc.cStore).solver.solve(odes, eqs, claims, T, p, enc, evalExpr)
  }

  /**
   * Reject invalid models. The following rules are checked:
   *  - RHS of a continuous assignment to a primed variable must not contain the LHS.
   */
  def checkValidAssignments(as: Seq[Action]): Boolean = {
    //TODO Add checking of discrete assignments 
    val highestDerivatives = as.flatMap{
      case Continuously(EquationT(d@Dot(_,Name(_,n)), _)) if n > 0 => List(d)
      case _ => Nil
    }.groupBy(a => (a.obj, a.field.x)).values.map(_.sortBy(_.field.primes).head).toList
    as.forall(_ match {
      case Continuously(EquationT(_, rhs: Expr)) =>
        val badDots = dots(rhs) intersect highestDerivatives
        if (badDots nonEmpty)
          throw new BadRhs("The use of ODE LHSs in the RHS of an equation is not supported: " + pprint(badDots.head: Expr)).setPos(rhs.pos)
        else
          true
      case Continuously(EquationI(_, _)) => true
      case IfThenElse(_, tb, eb) => checkValidAssignments(tb) && checkValidAssignments(eb)
      case Switch(_, cs) => cs.forall(c => checkValidAssignments(c.rhs))
      case _ => true
    })
  }
  
}
