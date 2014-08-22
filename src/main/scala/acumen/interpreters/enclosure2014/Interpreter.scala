package acumen
package interpreters
package enclosure2014

import scala.annotation.tailrec
import scala.Stream._
import scala.collection.immutable.{
  HashMap, MapProxy
}
import util.ASTUtil.{
  dots, checkNestedHypotheses, op
}
import Common._
import Errors.{
  BadLhs, BadRhs, ConstructorArity, DuplicateContinuousAssingment, 
  DuplicateDiscreteAssingment, HypothesisFalsified, NotAClassName, NotAnObject, 
  PositionalAcumenError, ShouldNeverHappen, UnknownOperator
}
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
  Abs, Box, Constant, Contract, Cos, Divide, Expression, Field, 
  Interval, Negate, Parameters, Rounding, Sin, Sqrt
}
import enclosure.Types.VarName
import acumen.interpreters.enclosure.Transcendentals

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
 */
object Interpreter extends CStoreInterpreter {
  
  type Store = EnclosureAndBranches
  def repr(st:Store) = st.enclosure
  def fromCStore(st: CStore, root: CId): Store = EnclosureAndBranches(st, (st, Epsilon, StartTime) :: Nil) 
  
  /* Initial values */

  // FIXME Get parameter value from model
  private val bannedFieldNames = List(self, parent, classf, nextChild, seed1, seed2, magicf)
  private implicit val rnd = Rounding(Parameters.default)
  private val contractInstance = new Contract{}
  private val transcendentals = new Transcendentals(Parameters.default)

  /* Types */

  sealed abstract trait InitialConditionTime
  case object StartTime extends InitialConditionTime
  case object UnknownTime extends InitialConditionTime
  type InitialCondition = (Enclosure, Evolution, InitialConditionTime)
  case class EnclosureAndBranches(val enclosure: Enclosure, branches: List[InitialCondition])
  
  /** Represents a sequence of Changesets without consecutive repeated flows. */
  case class Evolution(changes: List[Changeset]) {
    def head: Changeset = changes.head
    def nonEmpty: Boolean = changes.nonEmpty
    /** Extend this evolution by cs, unless cs is a flow and equal to changes.head. */
    def ::(cs: Changeset): Evolution = 
      if (changes.nonEmpty && isFlow(cs) && cs == changes.head) this else Evolution(cs :: changes)
  }
  val Epsilon = Evolution(Nil)
  
  type Enclosure = CStore
  case class EnclosureOps(e: Enclosure) {
    /** Take the meet, or g.l.b., of e and that Enclosure. */
    def /\(that: Enclosure): Enclosure = e meet that
    def /\(that: Option[Enclosure]): Enclosure = if (that isDefined) this meet that.get else e 
    /** Take the meet, or g.l.b., of e and that Enclosure. */
    def meet (that: Enclosure): Enclosure = merge(that: Enclosure, (l: GEnclosure[_], r: GEnclosure[_]) => (l,r) match {
      case (le: Real, re: Real) => Some(le /\ re)
      case (ls: GStrEnclosure, rs: GStrEnclosure) => Some(GStrEnclosure(ls.start union rs.start, ls.range union rs.range, ls.end union rs.end))
      case (ls: GIntEnclosure, rs: GIntEnclosure) => Some(GIntEnclosure(ls.start union rs.start, ls.range union rs.range, ls.end union rs.end))
    }).get
    /** Merge e and that Enclosure using ce to combine scalar enclosure values. */
    def merge(that: Enclosure, ce: (GEnclosure[_], GEnclosure[_]) => Option[GEnclosure[_]]): Option[Enclosure] = {
      require(e.keySet == that.keySet, "Can not merge enclosures with differing object sets.") // TODO Update for dynamic objects
      try {
        Some(for ((cid, co) <- e) yield (cid, {
          val tco = that(cid)
          require(co.keySet == tco.keySet, "Can not merge objects with differing name sets.") // TODO Update for dynamic objects
          require(classOf(co) == classOf(tco), s"Can not merge objects of differing classes (${classOf(co).x}, ${classOf(tco).x}).")
          if (classOf(co) == cmagic)
            co // FIXME Sanity check this (that it is OK to merge Enclosures with different simulator objects)
          else
            for ((n, v) <- co) yield (n,
              (v, tco(n)) match {
                case (VLit(l: GEnclosure[_]), VLit(r: GEnclosure[_])) => VLit(ce(l, r).getOrElse(sys.error("Error when merging $cid.$n.")))
                case (l, r) if l == r                                 => l
              })
        }))
      } catch { case e: Throwable => None } // FIXME Use Either to propagate error information 
    }
    /** Returns a copy of e with das applied to it. */
    def apply(das: Set[DelayedAction]): Enclosure = 
      update(das.map(d => (d.selfCId, d.lhs.field) -> evalExpr(d.rhs, d.env, this.e)).toMap)
    /** Update e with respect to u. */
    def update(u: Map[(CId, Name), CValue]) =
      for { (cid, co) <- e }
      yield (cid,
        for { (n, v) <- co }
        yield (n, u getOrElse ((cid, n), v)))
    /** Field-wise containment. */
    def contains(that: Enclosure): Boolean = { // TODO Update for dynamic objects
      def containsCObject(lo: CObject, ro: CObject): Boolean =
        lo.forall {
          case (n, v) =>
            if (bannedFieldNames contains n) true
            else {
              val rv = ro.get(n)
              rv.isDefined && ((v, rv.get) match {
                case (VLit(l: Real), VLit(r: Real)) => l contains r
                case (VLit(l: GStrEnclosure), VLit(r: GStrEnclosure)) => l contains r
                case (VLit(l: GIntEnclosure), VLit(r: GIntEnclosure)) => l contains r
                case (VLit(GStr(_)) | VResultType(_) | VClassName(_), tv @ (VLit(GStr(_)) | VResultType(_) | VClassName(_))) => v == tv
                case (VObjId(Some(o1)), VObjId(Some(o2))) => containsCObject(e(o1), that(o2))
                case (_, tv) => sys.error(s"Contains not applicable to $n: $v, $tv in " + lo(self))
              })}}
        e.forall { case (cid, co) => 
          if (classOf(co) == cmagic) true // FIXME Sanity check this
          else containsCObject(co, that(cid)) 
        }
      }
    /** Take the intersection of e and that Object. */
    def intersect(that: Enclosure): Option[Enclosure] = merge(that, (l: GroundValue, r: GroundValue) => ((l,r): @unchecked) match {
      case (le: Real, re: Real) => le intersect re 
      case (ls: GStrEnclosure, rs: GStrEnclosure) => (ls.start intersect rs.start, ls.enclosure intersect rs.enclosure, ls.end intersect rs.end) match {
        case (start, enclosure, end) if start.nonEmpty && enclosure.nonEmpty && end.nonEmpty => Some(GStrEnclosure(start,enclosure,end))
        case _ => sys.error(s"Empty intersection between $ls and $rs") // FIXME Use Either to propagate error information 
      }
      case (ls: GIntEnclosure, rs: GIntEnclosure) => (ls.start intersect rs.start, ls.enclosure intersect rs.enclosure, ls.end intersect rs.end) match {
        case (start, enclosure, end) if start.nonEmpty && enclosure.nonEmpty && end.nonEmpty => Some(GIntEnclosure(start, enclosure, end))
        case _ => sys.error(s"Empty intersection between $ls and $rs") // FIXME Use Either to propagate error information 
      }
    })
    /** Field-wise projection. Replaces each enclosure with a new one corresponding to its end-time interval. */
    def endTimeEnclosure(): Enclosure = map{ 
      case ce: Real => Real(ce.end) 
      case ui: GIntEnclosure => GIntEnclosure(ui.end, ui.end, ui.end) 
      case us: GStrEnclosure => GStrEnclosure(us.end, us.end, us.end) 
    }
    /** Field-wise range. */
    def range(): Enclosure = map{ 
      case ce: Real => Real(ce.enclosure) 
      case ui: GIntEnclosure => GIntEnclosure(ui.range, ui.range, ui.range) 
      case us: GStrEnclosure => GStrEnclosure(us.range, us.range, us.range) 
    }
    /** Returns a copy of this where f has been applied to all enclosure fields. */
    def map(f: GEnclosure[_] => GEnclosure[_]) =
      e.mapValues(_.mapValues{
        case VLit(ce: GEnclosure[_]) => VLit(f(ce))
        case field => field
      })
    /** Use f to reduce this enclosure to a value of type A. */
    def foldLeft[A](z: A)(f: (A, Real) => A): A =
      this.flatten.foldLeft(z) { case (r, (id, n, e)) => f(r, e) }
    /** Returns iterable of all Reals contained in this object and its descendants. */
    def flatten(): Iterable[(CId, Name, Real)] =
      e.flatMap{ case (cid, co) => co.flatMap{ 
        case (n, VLit(ce:Real)) => List((cid, n, ce))
        case _ => Nil
      }}
  }
  implicit def liftEnclosure(st: CStore): EnclosureOps = EnclosureOps(st)
  
  /** Return type of AST traversal (evalActions) */
  case class Changeset
    ( reps:   Set[(CId,CId)]         = Set.empty // reparentings
    , ass:    Set[DelayedAction]     = Set.empty // discrete assignments
    , odes:   Set[DelayedAction]     = Set.empty // ode assignments / differential equations
    , claims: Set[DelayedConstraint] = Set.empty // claims / constraints
    , hyps:   Set[DelayedHypothesis] = Set.empty // hypotheses
    ) {
    def ||(that: Changeset) =
      that match {
        case Changeset.empty => this
        case Changeset(reps1, ass1, odes1, claims1, hyps1) =>
          Changeset(reps ++ reps1, ass ++ ass1, odes ++ odes1, claims ++ claims1, hyps ++ hyps1)
      }
    override def toString =
      (reps, ass.map(d => Pretty.pprint(d.a)), ass.map(d => Pretty.pprint(d.a)), odes.map(d => Pretty.pprint(d.a)), claims.map(d => Pretty.pprint(d.c))).toString
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
    def logAssign(path: Expr, o: CId, d: Dot, a: Action, e: Expr, env: Env): Set[Changeset] =
      Set(Changeset(ass = Set(DelayedAction(path,o,a,env))))
    def logODE(path: Expr, o: CId, d: Dot, a: Action, e: Expr, env: Env): Set[Changeset] =
      Set(Changeset(odes = Set(DelayedAction(path,o,a,env))))
    def logClaim(o: CId, c: Expr) : Set[Changeset] =
      Set(Changeset(claims = Set(DelayedConstraint(o,c))))
    def logHypothesis(o: CId, s: Option[String], h: Expr, env: Env): Set[Changeset] =
      Set(Changeset(hyps = Set(DelayedHypothesis(o,s,h,env))))
    val empty = Changeset()
  }
  case class DelayedAction(path: Expr, selfCId: CId, a: Action, env: Env) {
    def lhs: Dot = (a: @unchecked) match {
      case Discretely(Assign(dot: Dot, _))      => dot
      case Continuously(EquationI(dot: Dot, _)) => dot
    }
    def rhs: Expr = (a: @unchecked) match {
      case Discretely(x: Assign)      => x.rhs
      case Continuously(x: EquationI) => x.rhs
    }
  }
  case class DelayedConstraint(selfCId: CId, c: Expr)
  case class DelayedHypothesis(selfCId: CId, s: Option[String], h: Expr, env: Env)

  import Changeset._

  case class GConstantRealEnclosure(start: Interval, enclosure: Interval, end: Interval) extends GRealEnclosure {
    require((enclosure contains start) && (enclosure contains end), 
      s"Enclosure must be valid over entire domain. Invalid enclosure: GConstantGConstantRealEnclosureEnclosure($start,$enclosure,$end)")
    override def apply(t: Interval): Interval = enclosure
    override def range: Interval = enclosure 
    override def isThin: Boolean = start.isThin && enclosure.isThin && end.isThin
    override def show: String = enclosure.toString
    def contains(that: GConstantRealEnclosure): Boolean =
      (start contains that.start) && (enclosure contains that.enclosure) && (end contains that.end)
    def /\ (that: GConstantRealEnclosure): GConstantRealEnclosure =
      GConstantRealEnclosure(start /\ that.start, enclosure /\ that.enclosure, end /\ that.end)
    def intersect(that: GConstantRealEnclosure): Option[GConstantRealEnclosure] =
      for {
        si <- start     intersect that.start
        e  <- enclosure intersect that.enclosure
        ei <- end       intersect that.end
      } yield GConstantRealEnclosure(si, e, ei)
  }
  object GConstantRealEnclosure {
    def apply(i: Interval): GConstantRealEnclosure = GConstantRealEnclosure(i,i,i)
    def apply(d: Double)(implicit rnd: Rounding): GConstantRealEnclosure = GConstantRealEnclosure(Interval(d))
    def apply(i: Int)(implicit rnd: Rounding): GConstantRealEnclosure = GConstantRealEnclosure(Interval(i))
  }
  type Real = GConstantRealEnclosure
  object Real {
    def apply(start: Interval, enclosure: Interval, end: Interval): Real = GConstantRealEnclosure(start,enclosure,end)
    def apply(i: Interval): Real = GConstantRealEnclosure(i,i,i)
    def apply(d: Double)(implicit rnd: Rounding): Real = GConstantRealEnclosure(Interval(d))
    def apply(i: Int)(implicit rnd: Rounding): Real = GConstantRealEnclosure(Interval(i))
  }
  abstract class GConstantDiscreteEncosure[T](val start: Set[T], val enclosure: Set[T], val end: Set[T]) extends GDiscreteEnclosure[T] {
    def apply(t: Interval) = range
    def range = start union enclosure union end
    def startTimeValue = start
    def endTimeValue = end
    def isThin = start.size == 1 && enclosure.size == 1 && end.size == 1
    def show = s"{${start.mkString(",")}}/{${enclosure.mkString(",")}}/{${end.mkString(",")}}"
    def contains(that: GConstantDiscreteEncosure[T]): Boolean =
      (that.start subsetOf this.start) && (that.enclosure subsetOf this.enclosure) && (that.end subsetOf this.end)
  }
  case class GStrEnclosure(override val start: Set[String], override val enclosure: Set[String], override val end: Set[String]) 
    extends GConstantDiscreteEncosure[String](start, enclosure, end)
  object GStrEnclosure {
    def apply(s: String): GStrEnclosure = GStrEnclosure(Set(s), Set(s), Set(s))
  }
  case class GIntEnclosure(override val start: Set[Int], override val enclosure: Set[Int], override val end: Set[Int]) 
    extends GConstantDiscreteEncosure[Int](start, enclosure, end)
  
  /* Set-up */
  
  /**
   * Lift all numeric values to ConstantEnclosures (excluding the Simulator object) 
   * and all strings to uncertain strings.
   */
  def liftToUncertain(p: Prog): Prog =
    new util.ASTMap {
      override def mapClassDef(c: ClassDef): ClassDef =
        if (c.name == cmagic) c else super.mapClassDef(c)
      override def mapExpr(e: Expr): Expr = e match {
        case Lit(GBool(b))          => Lit(if (b) CertainTrue else CertainFalse)
        case Lit(GInt(i))           => Lit(Real(i))
        case Lit(GDouble(d))        => Lit(Real(d))
        case Lit(GInterval(i))      => Lit(Real(i))
        case ExprInterval(Lit(lo@(GDouble(_)|GInt(_)))  // FIXME Add support for arbitrary expression end-points
                         ,Lit(hi@(GDouble(_)|GInt(_))))    
                                    => Lit(Real(Interval(extractDouble(lo), extractDouble(hi))))
        case ExprInterval(lo,hi)    => sys.error("Only constant interval end-points are currently supported. Offending expression: " + Pretty.pprint(e))
        case ExprIntervalM(lo,hi)   => sys.error("Centered interval syntax is currently not supported. Offending expression: " + Pretty.pprint(e))
        case Lit(GStr(s))           => Lit(GStrEnclosure(s))
        case _                      => super.mapExpr(e)
      }
      override def mapDiscreteAction(a: DiscreteAction) : DiscreteAction = a match {
        case Assign(lhs @ Dot(Dot(Var(Name(self,0)),Name(simulator,0)), _), rhs) => Assign(mapExpr(lhs), super.mapExpr(rhs))
        case _ => super.mapDiscreteAction(a)
      }
      override def mapContinuousAction(a: ContinuousAction) : ContinuousAction = a match {
        case EquationT(lhs @ Dot(Dot(Var(Name(self,0)),Name(simulator,0)), _), rhs) => sys.error("Only discrete assingments to simulator parameters are supported. Offending statement: " + a)
        case _ => super.mapContinuousAction(a)
      }
      override def mapClause(c: Clause) : Clause = c match {
        case Clause(GStr(lhs), as, rhs) => Clause(GStrEnclosure(lhs), mapExpr(as), mapActions(rhs))
        case _ => super.mapClause(c)
      }
    }.mapProg(p)
    
  /* Store manipulation */
  
  /** 
   * Update simulator object of s.enclosure.
   * NOTE: Simulators objects of st.branches are not affected.
   */
  def setInSimulator(f:Name, v:CValue, s:Store): Store = {
    def helper(e: Enclosure) = setObjectField(magicId(e), f, v, e)
    EnclosureAndBranches(helper(s.enclosure), s.branches.map { case (e, ev, t) => (helper(e), ev, t) })
  }
    
  /** Update simulator parameters in st.enclosure with values from the code in p. */
  def updateSimulator(p: Prog, st: Store): Store = {
    val paramMap = p.defs.find(_.name == cmain).get.body.flatMap {
      case Discretely( Assign(Dot(Dot(Var(Name(self, 0)), Name(simulator, 0)), param)
                     , Lit(rhs @ (GInt(_) | GDouble(_) | GInterval(_))))) => 
        List((param, VLit(rhs)))
      case _ => Nil
    }.toMap
    paramMap.foldLeft(st){ case (stTmp, (p,v)) => setInSimulator(p, v, stTmp) }
  }
  
  def setTime(d:Double, s:Store) = setInSimulator(time, VLit(GDouble(d)), s)
  
  def setResultType(t:ResultType, s:Store) = setInSimulator(resultType, VResultType(t), s)
    
  /** Get a fresh object id for a child of parent */
  def freshCId(parent:Option[CId], st: CStore) : (CId, CStore) = parent match {
    case None => (CId.nil, st)
    case Some(p) =>
      val VLit(GInt(n)) = getObjectField(p, nextChild, st)
      val st1 = setObjectField(p, nextChild, VLit(GInt(n+1)), st)
      (n :: p, st1)
  }
  
  /** Splits self's seed into (s1,s2), assigns s1 to self and returns s2 */
  def getNewSeed(self:CId, st: Enclosure) : ((Int,Int), Enclosure) = {
    val (s1,s2) = Random.split(getSeed(self,st))
	  val st1 = changeSeed(self, s1, st)
    (s2, st1)
  }
  
  /** Transfer parenthood of a list of objects, whose ids are "cs", to address p */
  def reparent(certain: Boolean, cs:List[CId], p:CId) : Set[Changeset] =
    cs.toSet flatMap (logReparent( _:CId,p))
    
  /** Create an env from a class spec and init values */
  def mkObj(c:ClassName, p:Prog, prt:Option[CId], seed:(Int,Int),
            paramvs:List[CValue], st: Enclosure, childrenCounter:Int = 0) : (CId, Enclosure) = {
    val cd = classDef(c,p)
    val base = HashMap(
      (classf, VClassName(c)),
      (parent, VObjId(prt)),
      (seed1, VLit(GInt(seed._1))),
      (seed2, VLit(GInt(seed._2))),
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
    val st2 = setObject(fid, pub, st1)
    val (vs, st3) = 
      constrs.foldLeft((List.empty[CValue],st2)) 
        { case ((vsTmp, stTmp), NewRhs(e,es)) =>
            val ve = evalExpr(e, Map(self -> VObjId(Some(fid))), stTmp) 
            val cn = ve match {case VClassName(cn) => cn; case _ => throw NotAClassName(ve)}
            val ves = es map (evalExpr(_, Map(self -> VObjId(Some(fid))), stTmp))
            val (nsd, stTmp1) = getNewSeed(fid, stTmp)
            val (oid, stTmp2) = mkObj(cn, p, Some(fid), nsd, ves, stTmp1)
            (VObjId(Some(oid)) :: vsTmp, stTmp2)
          case ((vsTmp, stTmp), ExprRhs(e)) =>
            val ve = evalExpr(e, Map(self -> VObjId(Some(fid))), stTmp)
            (ve :: vsTmp, stTmp)
        }
        val priv = privVars zip vs.reverse 
        // new object creation may have changed the nextChild counter
        val newpub = deref(fid,st3)
        val st4 = setObject(fid, newpub ++ priv, st3)
    (fid, st4)
  }

  /* Interpreter */

  /** Evaluate e in the scope of env for definitions p with current store st */
  def evalExpr(e:Expr, env:Env, st:Enclosure) : CValue = {
    def eval(env:Env, e:Expr) : CValue = try {
	    e match {
  	    case Lit(i)         => VLit(i)
        case ExprInterval(lo,hi) => 
          VLit(GConstantRealEnclosure(extractInterval(evalExpr(lo, env, st)) /\ extractInterval(evalExpr(hi, env, st))))
        case ExprVector(l)  => VVector (l map (eval(env,_)))
        case Var(n)         => env.get(n).getOrElse(VClassName(ClassName(n.x)))
        case Index(v,i)     => evalIndexOp(eval(env, v), eval(env, i))
        case Dot(o,Name("children",0)) =>
          /* In order to avoid redundancy en potential inconsistencies, 
             each object has a pointer to its parent instead of having 
             each object maintain a list of its children. This is why the childrens'
             list has to be computed on the fly when requested. 
             An efficient implementation wouldn't do that. */
          val id = extractId(eval(env,o))
          checkAccessOk(id, env, st, o)
          VList(childrenOf(id,st) map (c => VObjId(Some(c))))
        /* e.f */
        case Dot(e,f) =>
          val id = extractId(eval(env, e))
          checkAccessOk(id, env, st, e)
          if (id == selfCId(env))
            env.get(f).getOrElse(getObjectField(id, f, st))
          else
            getObjectField(id, f, st)
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
  }
  
  /** Purely functional operator evaluation at the values level */
  def evalOp[A](op:String, xs:List[Value[_]]) : Value[A] = {
    (op,xs) match {
       case (_, VLit(x)::Nil) =>
         VLit(unaryGroundOp(op,x))
       case (_,VLit(x)::VLit(y)::Nil) =>  
         VLit(binGroundOp(op,x,y))
       case _ =>
         throw UnknownOperator(op)    
    }
  }
  
  /** Purely functional unary operator evaluation at the ground values level */
  def unaryGroundOp(f:String, vx:GroundValue) = {
    def implem(f: String, x: Interval) = f match {
      case "-"   => -x
      case "sin" => transcendentals.sin(x)
      case "cos" => transcendentals.sin(x)
      case _     => throw UnknownOperator(f)
    }
    (f, vx) match {
      case ("not", Uncertain)    => Uncertain
      case ("abs", GInterval(i)) => Real(i.abs)
      case ("-",   GInterval(i)) => Real(-i)
      case _                     => Real(implem(f, extractInterval(vx)))
    }
  }
  
  /** Purely functional binary operator evaluation at the ground values level */
  def binGroundOp(f:String, vl:GroundValue, vr:GroundValue): GroundValue = {
    lazy val ivl = extractInterval(vl)
    lazy val ivr = extractInterval(vr)
    lazy val (startL: Interval, endL: Interval) = vl match {
      case ce: Real => (ce.start, ce.end)
      case _ => (ivl,ivl)
    }
    lazy val (startR: Interval, endR: Interval) = vr match {
      case ce: Real => (ce.start, ce.end)
      case _ => (ivr,ivr)
    }
    def implemInterval(f:String, l:Interval, r:Interval) = f match {
      case "+" => l + r
      case "-" => l - r
      case "*" => l * r
      case "/" => l / r
    }
    // Based on implementations from acumen.interpreters.enclosure.Relation
    def implemBool(f:String, l:GroundValue, r:GroundValue): GUncertainBool = f match {
      case "<" =>
        if ((extractInterval(l) lessThan extractInterval(r)) || // l < r holds over entire time step
            // l < r holds for some time in this step due to intermediate value theorem
            (((startL lessThan startR) && (endR lessThan endL)) ||
             ((startR lessThan startL) && (endL lessThan endR))))
          CertainTrue
        else if (extractInterval(r) lessThanOrEqualTo extractInterval(l))
          CertainFalse
        else Uncertain
      case ">" =>
        implemBool("<",r,l)
      case "<=" =>
        if ((extractInterval(l) lessThanOrEqualTo extractInterval(r)) || // l <= r holds over entire time step
            // l <= r holds for some time in this step due to intermediate value theorem
            (((startL lessThanOrEqualTo startR) && (endR lessThanOrEqualTo endL)) ||
             ((startR lessThanOrEqualTo startL) && (endL lessThanOrEqualTo endR))))
          CertainTrue
        else if (extractInterval(r) lessThan extractInterval(l))
          CertainFalse
        else Uncertain
      case ">=" => 
        implemBool("<=",r,l)
      case "==" =>
        val left = extractInterval(l)
        val right = extractInterval(r)
        if (left == right && left.isThin) CertainTrue
        else if (left disjointFrom right) CertainFalse
        else Uncertain
      case "~=" =>
        val left = extractInterval(l)
        val right = extractInterval(r)
        if (left == right && left.isThin) CertainFalse
        else if (left disjointFrom right) CertainTrue
        else Uncertain
    }
    f match {
      case "=="|"~="|">="|"<="|"<"|">" => (f, vl,vr) match {
        case ("==",sl: GStr,sr:GStr) => if(sl == sr) CertainTrue else CertainFalse
        case ("~=",sl: GStr,sr:GStr) => if(sl != sr) CertainTrue else CertainFalse
        // TODO Check if access to start-time values changes the definitions of == and ~=
        case ("==", GStrEnclosure(sls, sl,sle), GStrEnclosure(srs, sr, sre)) => 
          if (sls == srs && sl == sr && sle == sre) CertainTrue 
          else if ((sl intersect sr).nonEmpty) Uncertain
          else CertainFalse
        case ("~=", GStrEnclosure(sls, sl,sle), GStrEnclosure(srs, sr, sre)) =>
          if (sls == srs && sl == sr && sle == sre) CertainFalse 
          else if ((sl intersect sr).isEmpty) CertainTrue
          else Uncertain
        case _ => implemBool(f,vl,vr)
      }
      case "+"|"-"|"*"|"/" => Real(implemInterval(f,ivl,ivr)) // FIXME Improve these w.r.t. end-time interval
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
          case _ => sys.error("Non-boolean expression in if statement: " + Pretty.pprint(c))
        }
      case Switch(mode, cs) =>
        val modes = cs map { case Clause(lhs,claim,rhs) =>
            val (inScope, stmts) = (evalExpr(op("==", mode, Lit(lhs)), env, st): @unchecked) match {
              case VLit(Uncertain)    => (Uncertain, evalActions(false, path, rhs, env, p, st)) // FIXME Add op("==", mode, Lit(lhs)) to path
              case VLit(CertainTrue)  => (CertainTrue, evalActions(certain, path, rhs, env, p, st)) // FIXME Add op("!=", mode, Lit(lhs)) to path
              case VLit(CertainFalse) => (CertainFalse, Set(Changeset.empty))
            }
          (inScope, claim match {
            case Lit(GBool(true)) => stmts
            case _ =>
              combine(stmts :: logClaim(selfCId(env), claim) :: Nil)
          })
        }
        val (in, uncertain) = modes.foldLeft((Set.empty[Changeset], Set.empty[Changeset])) {
          case (iu @ (i, u), (gub, scs)) => (gub: @unchecked) match {
            case CertainTrue  => (combine(i, scs), u)
            case Uncertain    => (i, u union scs)
            case CertainFalse => iu
          }
        }
        combine(in, uncertain)
      case Discretely(da) =>
        evalDiscreteAction(certain, path, da, env, p, st)
      case Continuously(ca) =>
        evalContinuousAction(certain, path, ca, env, p, st) 
      case Claim(c) =>
        logClaim(selfCId(env), c)
      case Hypothesis(s, e) =>
        if (path == Lit(CertainTrue)) // top level action
          logHypothesis(selfCId(env), s, e, env)
        else 
          throw new PositionalAcumenError{ val mesg = "Hypothesis statements are only allowed on the top level." }.setPos(e.pos)
      case ForEach(n, col, body) => //TODO Add support for for-each statements
        throw new PositionalAcumenError{ val mesg = "For-each statements are not supported in the Enclosure 2014 semantics." }.setPos(col.pos)
    }
 
  def evalDiscreteAction(certain:Boolean, path: Expr, a:DiscreteAction, env:Env, p:Prog, st: Enclosure) : Set[Changeset] =
    a match {
      case Assign(Dot(o @ Dot(Var(self),Name(simulator,0)), n), e) =>
        Set.empty // TODO Ensure that this does not cause trouble down the road 
      case Assign(d@Dot(o,x),rhs) => 
        /* Schedule the discrete assignment */
        val id = evalExpr(o, env, st) match {
          case VObjId(Some(id)) => checkAccessOk(id, env, st, o); id
          case v => throw NotAnObject(v).setPos(o.pos)
        }
        logAssign(path, id, d, Discretely(a), rhs, env)
      /* Basically, following says that variable names must be 
         fully qualified at this language level */
      case Assign(_,_) => 
        throw BadLhs()
    }

  def evalContinuousAction(certain:Boolean, path: Expr, a:ContinuousAction, env:Env, p:Prog, st: Enclosure) : Set[Changeset] = 
    a match {
      case EquationT(d@Dot(e,Name(_,primes)),rhs) =>
        if (primes == 0) sys.error("Continuous assignments to unprimed variables is not supported.") // TODO Add some level of support for equations
        else Set()
      case EquationI(d@Dot(e,_),rhs) =>
        val id = extractId(evalExpr(e, env, st))
        logODE(path , id, d, Continuously(a), e, env)
      case _ =>
        throw ShouldNeverHappen() // FIXME: enforce that with refinement types
    }
  
  def evalStep(p:Prog, st: Enclosure, rootId:CId) : Set[Changeset] = {
    val cl = getCls(rootId, st)
    val as = classDef(cl, p).body
    val env = HashMap((self, VObjId(Some(rootId))))
    evalActions(true, Lit(CertainTrue), as, env, p, st)
  }

  /**
   * Outer loop. Iterates f from the root to the leaves of the tree formed 
   * by the parent-children relation.
   */
  def iterate(f: CId => Set[Changeset], root: CId, st: Enclosure): Set[Changeset] = {
    val r = f(root)
    val cs = childrenOf(root, st)
    if (cs.isEmpty) r else combine(r, combine(cs, iterate(f, _:CId, st)))
  }
  
  /* Main simulation loop */  

  def init(prog: Prog) : (Prog, Store, Metadata) = {
    checkValidAssignments(prog.defs.flatMap(_ body))
    checkNestedHypotheses(prog)
    val cprog = CleanParameters.run(prog, CStoreInterpreterType)
    val cprog1 = makeCompatible(cprog)
    val enclosureProg = liftToUncertain(cprog1)
    val mprog = Prog(magicClass :: enclosureProg.defs)
    val (sd1,sd2) = Random.split(Random.mkGen(0))
    val (id,st1) = 
      mkObj(cmain, mprog, None, sd1, List(VObjId(Some(CId(0)))), initStore, 1)
    val st2 = changeParent(CId(0), id, st1)
    val st3 = changeSeed(CId(0), sd2, st2)
    (mprog, fromCStore(st3), NoMetadata)
  }
  
  /** Remove unsupported declarations and statements from the AST. */
  def makeCompatible(p: Prog): Prog = {
    def action(a: Action): List[Action] = a match {
      case Continuously(EquationT(Dot(_, `_3D`), _)) => Nil
      case IfThenElse(c, t, e) => IfThenElse(c, t flatMap action, e flatMap action) :: Nil
      case Switch(s, cs) => Switch(s, cs map { case Clause(l, a, r) => Clause(l, a, r flatMap action) }) :: Nil
      case ForEach(i, c, b) => ForEach(i, c, b flatMap action) :: Nil
      case _ => a :: Nil
    }
    def priv(i: Init): List[Init] = i match {
      case Init(`_3D`, _) => Nil
      case _ => i :: Nil
    }
    Prog(p.defs.map(d => d.copy( priv = d.priv.flatMap(priv)
                               , body = d.body.flatMap(action))))
  }
  
  lazy val initStore = Parser.run(Parser.store, initStoreTxt.format("#0"))
  val initStoreTxt = 
  """#0.0 { className = Simulator, parent = %s, nextChild = 0, seed1 = 0, seed2 = 0, 
            time = 0.0, endTime = 10.0, timeStep = 0.015625,
            outputRows = "All", continuousSkip = 0, resultType = @Discrete, 
            maxIterationsPerBranch = 1000, maxBranches = 100 }"""

  /** Updates the values of variables in xs (identified by CId and Dot.field) to the corresponding CValue. */
  def applyAssignments(xs: List[(CId, Dot, CValue)], st: Enclosure): Enclosure =
    xs.foldLeft(st)((stTmp, a: (CId, Dot, CValue)) => setObjectField(a._1, a._2.field, a._3, stTmp))

  def varNameToFieldIdMap(st: Enclosure): Map[VarName,(CId,Name)] =
    st.flatMap{ case (cid, co) =>
      if (classOf(co) == cmagic) Map[String,(CId,Name)]()
      else co.filter{
        case (n,_) if bannedFieldNames contains n => false
        case (_, VLit(_: GStr) | VLit(_: GStrEnclosure) | _:VObjId[_]) => false
        case (_, VLit(_:Real)) => true
        case f => sys.error("Usupported field type for: " + f)
      }.map{ case (n,v) => (fieldIdToName(cid,n), (cid, n)) } 
    }
  
  def step(p: Prog, st: Store, md: Metadata): Option[(Store, Metadata)] = {
    val st1 = updateSimulator(p, st)
    if (getTime(st1.enclosure) >= getEndTime(st1.enclosure)) {
      Console.logHypothesisReport(md, 0, getEndTime(st1.enclosure))
      None
    }
    else {
      Some(getResultType(st1.enclosure) match {
        case Continuous =>
          (setResultType(Discrete, st1), md)
        case Discrete =>
          (setResultType(FixedPoint, st1), md)
        case FixedPoint => // Do hybrid step
          val tNow = getTime(st1.enclosure)
          val tNext = tNow + getTimeStep(st1.enclosure)
          val T = Interval(tNow, tNext)
          val st2 = hybridEncloser(T, p, st1) // valid enclosure over T
          val md1 = testHypotheses(st2.enclosure, (tNow, tNext), p, md)
          val st3 = setResultType(Continuous, st2)
          val st4 = setTime(tNext, st3)
          (st4, md1)
      }) 
    }
  }

  /** Traverse the AST (p) and collect statements that are active given st. */
  def active(st: Enclosure, p: Prog): Set[Changeset] = {
    val a = iterate(evalStep(p, st, _), mainId(st), st)
    a.foreach{ changeset =>
      val odeIds = changeset.odes.toList.map(da => (da.selfCId, da.lhs))
      val assIds = changeset.ass.toList.map(da => (da.selfCId, da.lhs))
      checkContinuousDynamicsAlwaysDefined(p, odeIds, st)
      checkDuplicateAssingments(odeIds, DuplicateContinuousAssingment)
      checkDuplicateAssingments(assIds, DuplicateDiscreteAssingment)
    }
    a
  }
  
  /** Summarize result of evaluating the hypotheses of all objects. */
  def testHypotheses(st: Enclosure, timeDomain: (Double,Double), p: Prog, old: Metadata): Metadata = {
    def testHypothesesOneChangeset(hs: Set[DelayedHypothesis]) = SomeMetadata(
      (for (DelayedHypothesis(o, s, h, env) <- hs) yield {
        lazy val counterEx = dots(h).toSet[Dot].map(d => d -> evalExpr(d, env, st))
        (o, getCls(o,st), s) -> (evalExpr(h, env, st) match {
          case VLit(CertainTrue)  => CertainSuccess
          case VLit(Uncertain)    => UncertainFailure(timeDomain, counterEx)
          case VLit(CertainFalse) => CertainFailure(timeDomain, counterEx)
        })
      }).toMap, (getTime(st), getTime(st) + getTimeStep(st)), true)
    old combine active(st, p).map(c => testHypothesesOneChangeset(c.hyps))
                             .reduce[Metadata](_ combine _)
  }
  
  /** 
   * Given a set of initial conditions (st.branches), computes a valid 
   * enclosure (.enclosure part of the result) for prog over T, as well
   * as a new set of initial conditions (.branches part of the result) 
   * for the next time interval.
   */
  def hybridEncloser(T: Interval, prog: Prog, st: EnclosureAndBranches): EnclosureAndBranches = {
    val VLit(GInt(maxBranches)): CValue = getInSimulator("maxBranches", st.enclosure)
    val VLit(GInt(maxIterationsPerBranch)): CValue = getInSimulator("maxIterationsPerBranch", st.enclosure)
    require(st.branches.nonEmpty, "hybridEncloser called with zero branches")
    require(st.branches.size < maxBranches, s"Number of branches (${st.branches.size}) exceeds maximum ($maxBranches).")
    def mergeBranches(ics: List[InitialCondition]): List[InitialCondition] =
      ics.groupBy(ic => (ic._2, ic._3)).map { case ((m, t), ic) => (ic.map(_._1).reduce(_ /\ _), m, t) }.toList
    @tailrec def enclose
      ( pwlW:  List[InitialCondition] // Waiting ICs, the statements that yielded them and time where they should be used
      , pwlR:  List[Enclosure] // Enclosure (valid over all of T)
      , pwlU:  List[InitialCondition] // Branches, i.e. states possibly valid at right end-point of T (ICs for next time segment)
      , pwlP:  List[Enclosure] // Passed states at initial time
      , pwlPs: List[Enclosure] // Passed states at uncertain time in T
      , iterations: Int // Remaining iterations
      ): Store =
      if (pwlW isEmpty) {
        EnclosureAndBranches((pwlR union pwlP union pwlPs).reduce(_ /\ _), mergeBranches(pwlU))        
      }
      else if (iterations / st.branches.size > maxIterationsPerBranch)
        sys.error(s"Enclosure computation over $T did not terminate in ${st.branches.size * maxIterationsPerBranch} iterations.")
      else {
        val (w, q, t) :: waiting = pwlW
        if (isPassed(w, t, pwlP, pwlPs))
          enclose(waiting, pwlR, pwlU, pwlP, pwlPs, iterations + 1)
        else {
          val (newP, newPs) = if (t == StartTime) (w :: pwlP, pwlPs) else (pwlP, w :: pwlPs)
          val hw = active(w, prog)
          if (q.nonEmpty && isFlow(q.head) && Set(q.head) == hw && t == UnknownTime)
            sys error "Model error!" // Repeated flow, t == UnknownTime means w was created in this time step
          val (newW, newR, newU) = encloseHw(waiting, pwlR, pwlU, (w, q, t), hw)
          enclose(newW, newR, newU, newP, newPs, iterations + 1)
        }
      }
    def encloseHw
      ( pwlW: List[InitialCondition], pwlR: List[Enclosure], pwlU: List[InitialCondition]
      , wqt: InitialCondition, hw: Set[Changeset]
      ) : (List[InitialCondition], List[Enclosure], List[InitialCondition]) = {
      val (w, qw, t) = wqt
      hw.foldLeft((pwlW, pwlR, pwlU)) {
        case ((tmpW, tmpR, tmpU), q) =>
          if (!isFlow(q)) {
            val wIntersectedWithGuard = contract(w, q.ass.map(da => DelayedConstraint(da.selfCId,da.path)), prog) match {
              case Right(r) => r
              case Left(s) => sys.error("Empty intersection while contracting with guard. " + s)  
            }
            ((wIntersectedWithGuard(q.ass), q :: qw, t) :: tmpW, tmpR, tmpU)
          }
          else if ((t == UnknownTime && qw.nonEmpty && qw.head == q) || T.isThin)
            (tmpW, tmpR, tmpU)
          else {
            val s = continuousEncloser(q.odes, q.claims, T, prog, w)
            val r = s.range
            val rp = contract(r, q.claims, prog).right.get
            val (newW, newU) = handleEvent(q, qw, r, rp, if (t == StartTime) s.endTimeEnclosure else r)
            (newW ::: tmpW, rp :: tmpR, newU ::: tmpU)
          }
      }
    }
    def handleEvent(q: Changeset, past: Evolution, r: Enclosure, rp: Enclosure, u: Enclosure): (List[InitialCondition], List[InitialCondition]) = {
      val hr = active(r, prog)
      val hu = active(u, prog) 
      val up = contract(u, q.claims, prog)
      val e = q :: past
      if (noEvent(q, hr, hu, up)) // no event
        (Nil, (u, e, StartTime) :: Nil)
      else if (certainEvent(q, hr, hu, up)) // certain event
        ((rp, e, UnknownTime) :: Nil, Nil)
      else // possible event
        ((rp, e, UnknownTime) :: Nil, (contract(u, q.claims, prog).right.get, e, StartTime) :: Nil)
    }
    enclose(st.branches, Nil, Nil, Nil, Nil, 0)
  }
  
  /**
   * Returns true if there is only a single change set is active over the current time segment, this 
   * change set contains no discrete assignments and the claim in q is not violated entirely by u.
   */
  def noEvent(q: Changeset, hr: Set[Changeset], hu: Set[Changeset], up: Either[String,Enclosure]) =
    hr.size == 1 && q.ass.isEmpty && (up match {
      case Left(s) => sys.error("Inconsistent model. A claim was invalidated without any event taking place. " + s) 
      case Right(_) => true
    })
    
  /** Returns true if both:
   *  1) More than one change set is active over the current time segment or q contains an assignment
   *  2) The change sets active at the end-point of the current time segment do not contain q or the  
   *     claim in q violates the enclosure at the end-point of the current time segment. */
  def certainEvent(q: Changeset, hr: Set[Changeset], hu: Set[Changeset], up: Either[String,Enclosure]): Boolean = {
    val qIsElementOfHu = hu exists (_ == q) 
    (hr.size > 1 || q.ass.nonEmpty) && // Some event is possible 
      (!qIsElementOfHu || up.isLeft) // Some possible event is certain 
  }
  
  /** Returns true if some element of passed contains s */
  def isPassed(s: Enclosure, t: InitialConditionTime, P: List[Enclosure], P1: List[Enclosure]) =
    (if (t == StartTime) P else P1) exists (_ contains s)
  
  /** Returns true if the discrete assignments in cs are empty or have no effect on s. */
  def isFlow(cs: Changeset) = cs.ass.isEmpty

  /**
   * Contract st based on all claims.
   * NOTE: Returns Left if the support of any of the claims has an empty intersection with st,
   *       or if some other exception is thrown by contract.
   */
  def contract(st: Enclosure, claims: Iterable[DelayedConstraint], prog: Prog): Either[String, Enclosure] = {
    /**
     * Given a predicate p and store st, removes that part of st for which p does not hold.
     * NOTE: The range of st is first computed, as contraction currently only works on intervals.  
     */
    def contract(st: Enclosure, p: Expr, prog: Prog, selfCId: CId): Option[Enclosure] = {
      val box = envBox(p, selfCId, st, prog)
      val varNameToFieldId = varNameToFieldIdMap(st)
      p match {
        case Lit(CertainTrue | Uncertain) => Some(st)
        case Lit(CertainFalse) => None
        case Op(Name("&&",0), List(l,r)) => 
          (contract(st,l,prog,selfCId), contract(st,r,prog,selfCId)) match {
            case (Some(pil),Some(pir)) => pil intersect pir
            case _ => None
          }
        case Op(Name(op,0), List(l,r)) =>
          val le = acumenExprToExpression(l,selfCId,st,prog)
          val re = acumenExprToExpression(r,selfCId,st,prog)
          val smallerBox = op match {
            case "<=" | "<" => contractInstance.contractLeq(le,re)(box)
            case ">=" | ">" => contractInstance.contractLeq(re,le)(box)
            case "==" => contractInstance.contractEq(le,re)(box)
            case "~=" => contractInstance.contractNeq(le,re)(box)
          } 
          val smallerBoxMap = smallerBox.map{ case (k, v) => (varNameToFieldId(k), VLit(Real(v))) }
          Some(st update smallerBoxMap)
      }
    }
    claims.foldLeft(Right(st): Either[String, Enclosure]) {
      case (res, claim) => res match {
        case Right(r) => 
          try {
            contract(r, claim.c, prog, claim.selfCId) map 
              (Right(_)) getOrElse Left("Empty enclosure after applying claim " + Pretty.pprint(claim.c))
          } catch { case e: Throwable => Left("Error while applying claim " + Pretty.pprint(claim.c) + ": " + e.getMessage) }
        case _ => res
      }
    }
  }
  
  /** Box containing the values of all variables (Dots) that occur in it. */
  def envBox(e: Expr, selfCId: CId, st: Enclosure, prog: Prog): Box = {
    new Box(dots(e).map{ case d@Dot(obj,n) =>
      val VObjId(Some(objId)) = evalExpr(obj, prog, selfCId, st) 
      (fieldIdToName(objId.cid,n), extractInterval(evalExpr(d, prog, selfCId, st)))
    }.toMap)
  }

  /** Evaluate expression in object with CId selfCId. Note: Can assumes that selfCId is not a simulator object. */
  def evalExpr(e: Expr, p: Prog, selfCId: CId, st: Enclosure): CValue =
    evalExpr(e,Map(Name("self", 0) -> VObjId(Some(selfCId))), st)

  val solver = new acumen.interpreters.enclosure.ivp.PicardSolver {}
  val extract = new acumen.interpreters.enclosure.Extract{}

  /**
   * Solve the equations as a simultaneous system.
   * NOTE: Currently, only a system of EquationI is supported.
   */
  def continuousEncloser
    ( odes: Set[DelayedAction] // Set of delayed ContinuousAction
    , claims: Set[DelayedConstraint]
    , T: Interval
    , p: Prog
    , st: Enclosure
    )(implicit rnd: Rounding): Enclosure = {
    val ic = contract(st.endTimeEnclosure, claims, p) match {
      case Left(_) => sys.error("Initial condition violates claims {" + claims.map(c => Pretty.pprint(c.c)).mkString(", ") + "}.")
      case Right(r) => r
    }
    val varNameToFieldId = varNameToFieldIdMap(ic)
    val F = getFieldFromActions(odes.flatMap(da => List((da.selfCId, da.a))).toList, ic, p)
    val stateVariables = varNameToFieldId.keys.toList
    val A = new Box(stateVariables.flatMap{ v => 
      val (o,n) = varNameToFieldId(v)
      (ic(o)(n): @unchecked) match {
        case VLit(ce:Real) => (v, ce.end) :: Nil
        case VObjId(_) => Nil
        case e => sys.error((o,n) + " ~ " + e.toString)
      }
    }.toMap)
    val solutions = solver.solveIVP(F, T, A, delta = 0, m = 0, n = 200, degree = 1)
    val solutionMap = solutions._1.apply(T).map{
      case (k, v) => (varNameToFieldId(k), VLit(Real(A(k), v, solutions._2(k))))
    }
    ic update solutionMap
  }

  /**
   * Reject invalid models. The following rules are checked:
   *  - RHS of a continuous assignment to a primed variable must not contain the LHS.
   */
  def checkValidAssignments(as: Seq[Action]): Boolean =
    //TODO Add checking of discrete assignments 
    //TODO Update this to check for valid ODEs
    as.forall(_ match {
      case Continuously(EquationT(d@Dot(_,Name(_,n)), rhs: Expr)) if n > 0 => 
        if (rhs.toString contains d.toString)
          throw new BadRhs(pprint(rhs) + " contains " + pprint(d:Expr))
        else
          true
      case Continuously(EquationI(_, _)) => true
      case IfThenElse(_, tb, eb) => checkValidAssignments(tb) && checkValidAssignments(eb)
      case Switch(_, cs) => cs.forall(c => checkValidAssignments(c.rhs))
      case _ => true
    })
  
  def getFieldFromActions(as: List[(CId,Action)], st: Enclosure, p: Prog)(implicit rnd: Rounding): Field = {
    val highestDerivatives = as.flatMap {
      case (cid, Continuously(EquationT(Dot(_, n), rhs))) => List(((cid,n), rhs))
      case _ => List()
    }.filter { case ((cid,n), _) => n.primes != 0 }.toMap
    Field(as.map {
      case (cid, Continuously(EquationI(Dot(_, n), rhs))) =>
        val rhsExpr = highestDerivatives.getOrElse((cid, Name(n.x, n.primes + 1)), rhs) // Inline RHS of highest derivative 
        (fieldIdToName(cid, n), acumenExprToExpression(rhsExpr, cid, st, p))
      case (cid, Continuously(EquationT(Dot(_, n), _))) =>
        (fieldIdToName(cid, n), Constant(0))
    }.toMap)
  }
  
  def acumenExprToExpression(e: Expr, selfCId: CId, st: Enclosure, p: Prog)(implicit rnd: Rounding): Expression = e match {
    case Lit(v) if v.eq(Constants.PI) => Constant(Interval.pi) // Test for reference equality not structural equality
    case Lit(GInt(d))                 => Constant(d)
    case Lit(GDouble(d))              => Constant(d)
    case Lit(e:Real)                  => Constant(e.enclosure) // FIXME Over-approximation of end-time interval!
    case ExprInterval(lo, hi)         => Constant(extract.foldConstant(lo).value /\ extract.foldConstant(hi).value)
    case ExprIntervalM(mid0, pm0)     => val mid = extract.foldConstant(mid0).value
                                         val pm = extract.foldConstant(pm0).value
                                         Constant((mid - pm) /\ (mid + pm))
    case Var(n)                       => fieldIdToName(selfCId, n)
    case Dot(objExpr, n) => 
      val VObjId(Some(obj)) = evalExpr(objExpr, p, selfCId, st) 
      fieldIdToName(obj, n)
    case Op(Name("-", 0), List(x))    => Negate(acumenExprToExpression(x,selfCId,st,p))
    case Op(Name("abs", 0), List(x))  => Abs(acumenExprToExpression(x,selfCId,st,p))
    case Op(Name("cos", 0), List(x))  => Cos(acumenExprToExpression(x,selfCId,st,p))
    case Op(Name("sin", 0), List(x))  => Sin(acumenExprToExpression(x,selfCId,st,p))
    case Op(Name("sqrt", 0), List(x)) => Sqrt(acumenExprToExpression(x,selfCId,st,p))
    case Op(Name("-", 0), List(l, r)) => acumenExprToExpression(l,selfCId,st,p) - acumenExprToExpression(r,selfCId,st,p)
    case Op(Name("+", 0), List(l, r)) => acumenExprToExpression(l,selfCId,st,p) + acumenExprToExpression(r,selfCId,st,p)
    case Op(Name("/", 0), List(l, r)) => Divide(acumenExprToExpression(l,selfCId,st,p), acumenExprToExpression(r,selfCId,st,p))
    case Op(Name("*", 0), List(l, r)) => acumenExprToExpression(l,selfCId,st,p) * acumenExprToExpression(r,selfCId,st,p)
    case _                            => sys.error("Handling of expression " + e + " not implemented!")
  }
  
  /* Utilities */
  
  def fieldIdToName(o: CId, n: Name): String = s"$n@$o"
  
}
