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
import Common._
import Errors.{
  AcumenError, BadLhs, BadRhs, ConstructorArity, ContinuousDynamicsUndefined, 
  DuplicateAssingment, DuplicateContinuousAssingment, DuplicateDiscreteAssingment,  
  HypothesisFalsified, NotAClassName, NotAnObject, NoMatch,
  PositionalAcumenError, ShouldNeverHappen, UnknownOperator, UnsupportedTypeError
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
import util.DebugUtil.{
  asProgram
}
import enclosure.{
  Box, Constant, Contract, Expression, Field, Interval, Parameters, Rounding,
  Abs, Sin, Cos, Tan, ACos, ASin, ATan, Exp, Log, Log10, Sqrt, 
  Cbrt, Ceil, Floor, Sinh, Cosh, Tanh, Signum, Plus, Negate, 
  Multiply, Pow, Divide, ATan2, Min, Max
}
import enclosure.Types.VarName
import acumen.interpreters.enclosure.Transcendentals
import acumen.interpreters.enclosure.ivp.{
  LohnerSolver, PicardSolver
}
import scala.util.parsing.input.Position

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
  def repr(st:Store) = st.enclosure
  def fromCStore(st: CStore, root: CId): Store = EnclosureAndBranches(st, (st, Epsilon, StartTime) :: Nil)
  override def visibleParameters: Map[String, CValue] = 
    Map(ParamTime, ParamEndTime, ParamTimeStep, 
        ParamMaxBranches, ParamMaxIterationsPerBranch, ParamIntersectWithGuardBeforeReset, ParamHypothesisReport) 

  /* Constants */
  
  private val ParamTime                          = "time"                          -> VLit(GDouble( 0.0))
  private val ParamEndTime                       = "endTime"                       -> VLit(GDouble(10.0))
  private val ParamTimeStep                      = "timeStep"                      -> VLit(GDouble( 0.015625))
  private val ParamMaxBranches                   = "maxBranches"                   -> VLit(GInt(100))                  
  private val ParamMaxIterationsPerBranch        = "maxIterationsPerBranch"        -> VLit(GInt(1000))    
  private val ParamIntersectWithGuardBeforeReset = "intersectWithGuardBeforeReset" -> VLit(GBool(true))
  private val ParamHypothesisReport              = "hypothesisReport"              -> VLit(GStr("Comprehensive"))
  
  private val legacyParameters = Parameters.default.copy(interpreter = Some(enclosure.Interpreter.EVT))
  private implicit val rnd = Rounding(legacyParameters)
  private val bannedFieldNames = List(self, parent, classf, nextChild, seed1, seed2, magicf)
  private val contractInstance = new Contract{}

  /* Types */

  sealed abstract trait InitialConditionTime
  case object StartTime extends InitialConditionTime
  case object UnknownTime extends InitialConditionTime
  type InitialCondition = (Enclosure, Evolution, InitialConditionTime)
  class EnclosureAndBranches(val enclosure: Enclosure, val branches: List[InitialCondition])
  object EnclosureAndBranches{
    def apply(e: Enclosure, bs: List[InitialCondition]): EnclosureAndBranches = 
      new EnclosureAndBranches(countVariables(e), bs) 
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
      case (ls: GBoolEnclosure, rs: GBoolEnclosure) => Some(GBoolEnclosure(ls.start union rs.start, ls.range union rs.range, ls.end union rs.end))
    }).get
    /** Merge e and that Enclosure using ce to combine scalar enclosure values. */
    def merge(that: Enclosure, ce: (GEnclosure[_], GEnclosure[_]) => Option[GEnclosure[_]]): Option[Enclosure] = {
      require(e.keySet == that.keySet, "Can not merge enclosures with differing object sets.") // TODO Update for dynamic objects
      try {
        Some(for ((cid, co) <- e) yield (cid, {
          val tco = that(cid)
          require(co.keySet == tco.keySet, "Can not merge objects with differing name sets.") // TODO Update for dynamic objects
          require(classOf(co) == classOf(tco), s"Can not merge objects of differing models (${classOf(co).x}, ${classOf(tco).x}).")
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
    def apply(das: Set[CollectedAction]): Enclosure = 
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
        if (classOf(lo) == cmagic && classOf(ro) == cmagic){
          if (lo(time) == ro(time) && lo(timeStep) == ro(timeStep))
            true
          else 
            throw internalError("Attempt to check containment of enclosures with incompatoble time domains: " +
              s" [${lo(time)},${extractDouble(lo(time)) + extractDouble(lo(timeStep))}]" +
              s", [${ro(time)},${extractDouble(ro(time)) + extractDouble(ro(timeStep))}]") 
        }
        else lo.forall {
          case (n, v) =>
            if (bannedFieldNames contains n) true
            else {
              ((v, ro get n) match {
                case (VLit(l: Real), Some(VLit(r: Real))) => 
                  l contains r
                case (VLit(l: GStrEnclosure), Some(VLit(r: GStrEnclosure))) => 
                  l contains r
                case (VLit(l: GIntEnclosure), Some(VLit(r: GIntEnclosure))) => 
                  l contains r
                case (VLit(l: GBoolEnclosure), Some(VLit(r: GBoolEnclosure))) => 
                  l contains r
                case (VLit(_:GStr) | _:VResultType | _:VClassName, tv @ Some(VLit(_:GStr) | _:VResultType | _:VClassName)) => 
                  v == tv
                case (VObjId(Some(o1)), Some(VObjId(Some(o2)))) => 
                  containsCObject(e(o1), that(o2))
                case (_, Some(tv)) => 
                  throw internalError(s"Contains not applicable to ${pprint(n)}: ${pprint(v)}, ${pprint(tv)}")
                
              })}}
        e.forall { case (cid, co) => containsCObject(co, that(cid)) }
      }
    /** Take the intersection of e and that Object. */
    def intersect(that: Enclosure): Option[Enclosure] = merge(that, (l: GroundValue, r: GroundValue) => ((l,r): @unchecked) match {
      case (le: Real, re: Real) => le intersect re 
      case (ls: GStrEnclosure, rs: GStrEnclosure) => (ls.start intersect rs.start, ls.enclosure intersect rs.enclosure, ls.end intersect rs.end) match {
        case (start, enclosure, end) if start.nonEmpty && enclosure.nonEmpty && end.nonEmpty => Some(GStrEnclosure(start,enclosure,end))
        case _ => sys.error(s"Empty intersection between string enclosures $ls and $rs") // FIXME Use Either to propagate error information 
      }
      case (ls: GIntEnclosure, rs: GIntEnclosure) => (ls.start intersect rs.start, ls.enclosure intersect rs.enclosure, ls.end intersect rs.end) match {
        case (start, enclosure, end) if start.nonEmpty && enclosure.nonEmpty && end.nonEmpty => Some(GIntEnclosure(start, enclosure, end))
        case _ => sys.error(s"Empty intersection between integer enclosures $ls and $rs") // FIXME Use Either to propagate error information 
      }
      case (ls: GBoolEnclosure, rs: GBoolEnclosure) => (ls.start intersect rs.start, ls.enclosure intersect rs.enclosure, ls.end intersect rs.end) match {
        case (start, enclosure, end) if start.nonEmpty && enclosure.nonEmpty && end.nonEmpty => Some(GBoolEnclosure(start, enclosure, end))
        case _ => sys.error(s"Empty intersection between boolean enclosures $ls and $rs") // FIXME Use Either to propagate error information 
      }
    })
    /** Field-wise projection. Replaces each enclosure with a new one corresponding to its start-time interval. */
    def start(): Enclosure = map{ 
      case ce: Real => Real(ce.start) 
      case ui: GIntEnclosure => GIntEnclosure(ui.start, ui.start, ui.start) 
      case us: GStrEnclosure => GStrEnclosure(us.start, us.start, us.start) 
      case us: GBoolEnclosure => GBoolEnclosure(us.start, us.start, us.start) 
    }
    /** Field-wise projection. Replaces each enclosure with a new one corresponding to its end-time interval. */
    def end(): Enclosure = map{ 
      case ce: Real => Real(ce.end) 
      case ui: GIntEnclosure => GIntEnclosure(ui.end, ui.end, ui.end) 
      case us: GStrEnclosure => GStrEnclosure(us.end, us.end, us.end) 
      case us: GBoolEnclosure => GBoolEnclosure(us.end, us.end, us.end) 
    }
    /** Field-wise range. */
    def range(): Enclosure = map{ 
      case ce: Real => Real(ce.enclosure) 
      case ui: GIntEnclosure => GIntEnclosure(ui.range, ui.range, ui.range) 
      case us: GStrEnclosure => GStrEnclosure(us.range, us.range, us.range) 
      case us: GBoolEnclosure => GBoolEnclosure(us.range, us.range, us.range) 
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
  
  import Changeset._

  case class GConstantRealEnclosure(start: Interval, enclosure: Interval, end: Interval) extends GRealEnclosure {
    require(enclosure contains end, // Enclosure may not contain start (initial condition), when the Lohner IVP solver is used 
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
    def apply(d: Double): GConstantRealEnclosure = GConstantRealEnclosure(Interval(d))
    def apply(i: Int): GConstantRealEnclosure = GConstantRealEnclosure(Interval(i))
  }
  type Real = GConstantRealEnclosure
  object Real {
    def apply(start: Interval, enclosure: Interval, end: Interval): Real = GConstantRealEnclosure(start,enclosure,end)
    def apply(i: Interval): Real = GConstantRealEnclosure(i,i,i)
    def apply(d: Double): Real = GConstantRealEnclosure(Interval(d))
    def apply(i: Int): Real = GConstantRealEnclosure(Interval(i))
  }
  abstract class GConstantDiscreteEnclosure[T](val start: Set[T], val enclosure: Set[T], val end: Set[T]) extends GDiscreteEnclosure[T] {
    def apply(t: Interval) = range
    def range = start union enclosure union end
    def isThin = start.size == 1 && enclosure.size == 1 && end.size == 1
    def show = s"{${enclosure mkString ","}}"
    def contains(that: GConstantDiscreteEnclosure[T]): Boolean =
      (that.start subsetOf this.start) && (that.enclosure subsetOf this.enclosure) && (that.end subsetOf this.end)
  }
  case class GStrEnclosure(override val start: Set[String], override val enclosure: Set[String], override val end: Set[String]) 
    extends GConstantDiscreteEnclosure[String](start, enclosure, end)
  object GStrEnclosure {
    def apply(ss: Set[String]): GStrEnclosure = GStrEnclosure(ss, ss, ss)
    def apply(s: String): GStrEnclosure = GStrEnclosure(Set(s))
  }
  case class GIntEnclosure(override val start: Set[Int], override val enclosure: Set[Int], override val end: Set[Int]) 
    extends GConstantDiscreteEnclosure[Int](start, enclosure, end)
  case class GBoolEnclosure(override val start: Set[Boolean], override val enclosure: Set[Boolean], override val end: Set[Boolean])  
    extends GConstantDiscreteEnclosure[Boolean](start,enclosure,end)
  object GBoolEnclosure {
    def apply(ss: Set[Boolean]): GBoolEnclosure = GBoolEnclosure(ss, ss, ss)
    def apply(s: Boolean): GBoolEnclosure = GBoolEnclosure(Set(s))
  }
  val CertainTrue = GBoolEnclosure(Set(true), Set(true), Set(true))
  val CertainFalse = GBoolEnclosure(Set(false), Set(false), Set(false))
  val Uncertain = GBoolEnclosure(Set(true, false), Set(true, false), Set(true, false))

  /* Set-up */
  
  /**
   * Lift all numeric values to ConstantEnclosures (excluding the Simulator object) 
   * and all strings to uncertain strings.
   */
  def liftToUncertain(p: Prog): Prog =
    new util.ASTMap {
      val magicDot = Dot(Var(self),magicf)
      override def mapClassDef(c: ClassDef): ClassDef =
        if (c.name == cmagic) c else super.mapClassDef(c)
      override def mapExpr(e: Expr): Expr = e match {
        case Lit(GBool(b))          => Lit(if (b) CertainTrue else CertainFalse)
        case Lit(GInt(i))           => Lit(Real(i))
        case Lit(GDouble(d))        => Lit(Real(d))
        case Lit(GInterval(i))      => Lit(Real(i))
        case ExprInterval( Lit(lo@(GDouble(_)|GInt(_)))  // FIXME Add support for arbitrary expression end-points
                         , Lit(hi@(GDouble(_)|GInt(_))))    
                                    => Lit(Real(Interval(extractDouble(lo), extractDouble(hi))))
        case ExprInterval(lo,hi)    => sys.error("Only constant interval end-points are currently supported. Offending expression: " + pprint(e))
        case ExprIntervalM(lo,hi)   => sys.error("Centered interval syntax is currently not supported. Offending expression: " + pprint(e))
        case Lit(GStr(s))           => Lit(GStrEnclosure(s))
        case _                      => super.mapExpr(e)
      }
      override def mapDiscreteAction(a: DiscreteAction) : DiscreteAction = a match {
        case Assign(lhs @ Dot(`magicDot`, _), rhs) => Assign(mapExpr(lhs), super.mapExpr(rhs))
        case _ => super.mapDiscreteAction(a)
      }
      override def mapClause(c: Clause) : Clause = c match {
        case Clause(GStr(lhs), as, rhs) => Clause(GStrEnclosure(lhs), mapExpr(as), mapActions(rhs))
        case Clause(GInt(lhs), as, rhs) => Clause(Real(lhs), mapExpr(as), mapActions(rhs)) // FIXME Use discrete integer enclosure instead of Real
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
                     , Lit(rhs @ (GBool(_) | GDouble(_) | GInt(_) | GInterval(_))))) => 
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
        case Index(v,i)     => evalIndexOp(eval(env, v), i.map(x => eval(env, x)))
        case Dot(o,f) =>
          val id = extractId(evalExpr(o,env,st))
          env.get(f).getOrElse(getObjectField(id, f, st))
        /* e.f */
        case ResolvedDot(id, o, f) =>
          if (f == Name("children",0))
          /* In order to avoid redundancy and potential inconsistencies, 
             each object has a pointer to its parent instead of having 
             each object maintain a list of its children. This is why the childrens'
             list has to be computed on the fly when requested. 
             An efficient implementation wouldn't do that. */
            VList(childrenOf(id,st) map (c => VObjId(Some(c))))
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
  }.setPos(e.pos)
  
  /** Purely functional operator evaluation at the values level */
  def evalOp[A](op:String, xs:List[Value[_]]) : Value[A] = {
    (op,xs) match {
       case (_, VLit(x:GEnclosure[A])::Nil) =>
         VLit(unaryGroundOp(op,x))
       case (_,VLit(x:GEnclosure[A])::VLit(y:GEnclosure[A])::Nil) =>  
         VLit(binGroundOp(op,x,y))
       case _ =>
         throw UnknownOperator(op)    
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
      case (_, e: GRealEnclosure) => Real(implem(f, e.start), implem(f, e.range), implem(f, e.end))
    }
  }
  
  /** Purely functional binary operator evaluation at the ground values level */
  def binGroundOp[V](f:String, vl:GEnclosure[V], vr:GEnclosure[V]): GroundValue = {
    def implemInterval(f:String, l:Interval, r:Interval) = f match {
      case "+"     => l + r
      case "-"     => l - r
      case "*"     => if (l equalTo r) l.square else l * r
      case "^"     => l.pow(r)
      case "/"     => l / r
      case "atan2" => Interval.atan2(l, r)
      case "min"   => Interval.min(l, r)
      case "max"   => Interval.max(l, r)
    }
    // Based on implementations from acumen.interpreters.enclosure.Relation
    def implemBool(f:String, l:Interval, r:Interval): GBoolEnclosure = {
      lazy val (startL: Interval, endL: Interval) = vl match {
        case ce: Real => (ce.start, ce.end)
        case _ => (l,l)
      }
      lazy val (startR: Interval, endR: Interval) = vr match {
        case ce: Real => (ce.start, ce.end)
        case _ => (r,r)
      }
      f match {
        case "<" =>
          if ((l lessThan r) || // l < r holds over entire time step
              // l < r holds for some time in this step due to intermediate value theorem
              (((startL lessThan startR) && (endR lessThan endL)) ||
               ((startR lessThan startL) && (endL lessThan endR))))
            CertainTrue
          else if (r lessThanOrEqualTo l)
            CertainFalse
          else Uncertain
        case ">" =>
          implemBool("<",r,l)
        case "<=" =>
          if ((l lessThanOrEqualTo r) || // l <= r holds over entire time step
              // l <= r holds for some time in this step due to intermediate value theorem
              (((startL lessThanOrEqualTo startR) && (endR lessThanOrEqualTo endL)) ||
               ((startR lessThanOrEqualTo startL) && (endL lessThanOrEqualTo endR))))
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
      case ("==", sl: GStr, sr: GStr) => if (sl == sr) CertainTrue else CertainFalse
      case ("~=", sl: GStr, sr: GStr) => if (sl != sr) CertainTrue else CertainFalse
      // TODO Check if access to start-time values changes the definitions of == and ~=
      case ("==", GStrEnclosure(sls, sl, sle), GStrEnclosure(srs, sr, sre)) =>
        if (sls == srs && sl == sr && sle == sre) CertainTrue
        else if ((sl intersect sr).nonEmpty) Uncertain
        else CertainFalse
      case ("~=", GStrEnclosure(sls, sl, sle), GStrEnclosure(srs, sr, sre)) =>
        if (sls == srs && sl == sr && sle == sre) CertainFalse
        else if ((sl intersect sr).isEmpty) CertainTrue
        else Uncertain
      case ("==" | "~=" | ">=" | "<=" | "<" | ">", _, _) =>
        implemBool(f, extractInterval(vl), extractInterval(vr))
      case ("+" | "-" | "*" | "/", el: GRealEnclosure, er: GRealEnclosure) => 
        Real(implemInterval(f, el.start, er.start), implemInterval(f, el.range, er.range), implemInterval(f, el.end, er.end))
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
  def resolve(d: Dot, rhs: Expr, env: Env, st: CStore): (ResolvedDot, Expr) = {
    val id = extractId(evalExpr(d.obj, env, st))
    checkAccessOk(id, env, st, d.obj)
    val rd = ResolvedDot(id, d.obj, d.field)
    (rd, substitute(d, rd, rhs))
  }
 
  def evalDiscreteAction(certain:Boolean, path: Expr, a:DiscreteAction, env:Env, p:Prog, st: Enclosure) : Set[Changeset] =
    a match {
      case Assign(Dot(o @ Dot(Var(self),Name(simulator,0)), n), e) =>
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
    prog.defs.foreach(d => checkValidAssignments(d.body))
    checkNestedHypotheses(prog)
    checkContinuousAssignmentToSimulator(prog)
    val cprog = CleanParameters.run(prog, CStoreInterpreterType)
    val cprog1 = makeCompatible(cprog)
    val enclosureProg = liftToUncertain(cprog1)
    val mprog = Prog(magicClass :: enclosureProg.defs)
    val (sd1,sd2) = Random.split(Random.mkGen(0))
    val (id,st1) = 
      mkObj(cmain, mprog, None, sd1, List(VObjId(Some(CId(0)))), initStore, 1)
    val st2 = changeParent(CId(0), id, st1)
    val st3 = changeSeed(CId(0), sd2, st2)
    val hyps = st3.toList.flatMap { case (cid, co) =>
      mprog.defs.find(_.name == getCls(cid, st3)).get.body.flatMap {
        case Hypothesis(s, e) =>
          CollectedHypothesis(cid, s, e, Map(self -> VObjId(Some(cid)))) :: Nil
        case _ => Nil
    }}
    val md = testHypotheses(fromCStore(st3).enclosure, 0, 0, mprog, NoMetadata) 
    (mprog, fromCStore(st3), md)
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
  val initStoreTxt: String = 
    s"""#0.0 { className = Simulator, parent = %s, nextChild = 0, seed1 = 0, seed2 = 0, variableCount = 0, 
               outputRows = "All", continuousSkip = 0, resultType = @Initial, 
               ${visibleParameters.map(p => p._1 + "=" + pprint(p._2)).mkString(",")} }"""

  /** Updates the values of variables in xs (identified by CId and Dot.field) to the corresponding CValue. */
  def applyAssignments(xs: List[(CId, Dot, CValue)], st: Enclosure): Enclosure =
    xs.foldLeft(st)((stTmp, a: (CId, Dot, CValue)) => setObjectField(a._1, a._2.field, a._3, stTmp))

  def varNameToFieldIdMap(st: Enclosure): Map[VarName,(CId,Name)] =
    st.flatMap{ case (cid, co) =>
      if (classOf(co) == cmagic) Map[String,(CId,Name)]()
      else co.filter{
        case (n,_) if bannedFieldNames contains n => false
        case (_, VLit(_: GStr) | VLit(_: GStrEnclosure) | VLit(_: GBoolEnclosure) | _:VObjId[_]) => false
        case (_, VLit(_: Real)) => true
        case (n,v) =>
          val typ = "type " + v.getClass.getSimpleName
          throw new UnsupportedTypeError(typ, s"(${cid.cid.toString}:${getCls(cid, st).x}).${pprint(n)}", v)
      }.map{ case (n,v) => (fieldIdToName(cid,n), (cid, n)) } 
    }
  
  def step(p: Prog, st: Store, md: Metadata): StepRes = {
    val st1 = updateSimulator(p, st)
    if (getTime(st1.enclosure) >= getEndTime(st1.enclosure) && getResultType(st.enclosure) == FixedPoint) {
      Done(md, getEndTime(st1.enclosure))
    }
    else {
      val tNow = getTime(st1.enclosure)
      val (tNext, resType) = getResultType(st1.enclosure) match {
        case Continuous | Initial =>
          (tNow, FixedPoint)
        case FixedPoint =>
          (tNow + getTimeStep(st1.enclosure), Continuous)
      }
      val st2 = hybridEncloser(Interval(tNow, tNext), p, st1) // valid enclosure over T
      val md1 = testHypotheses(st2.enclosure, tNow, tNext, p, md)
      val st3 = setResultType(resType, st2)
      val st4 = setTime(tNext, st3)
      Data(st4, md1)
    }
  }

  /** Traverse the AST (p) and collect statements that are active given st. */
  def active(st: Enclosure, p: Prog): Set[Changeset] = 
    iterate(evalStep(p, st, _), mainId(st), st)

  /** Ensure that c does not contain duplicate assignments. */
  def checkValidChange(c: Set[Changeset]): Unit = c.foreach{ cs =>
    val contIds = (cs.eqs.toList ++ cs.odes.toList).map(_.lhs)
    val assIds = cs.dis.toList.map(_.lhs)
    checkDuplicateAssingments(contIds, DuplicateContinuousAssingment)
    checkDuplicateAssingments(assIds, DuplicateDiscreteAssingment)
  }
  
  /**
   * If cs is a flow, ensure that for each variable that has an ODE declared in the private section, 
   * there is an equation in scope at the current time step. This is done by checking that for each 
   * primed field name in each object in st, there is a corresponding CId-Name pair in eqs or odes.
   */
  def checkFlowDefined(prog: Prog, cs: Changeset, st: Enclosure): Unit =
    if (isFlow(cs)) {
      val contNamesActive = (cs.eqs union cs.odes).map { da => (da.lhs.id, da.lhs.field.x) }
      val odeNamesDeclared = prog.defs.map(d => (d.name, (d.fields ++ d.priv.map(_.x)).filter(_.primes > 0))).toMap
      st.foreach {
        case (o, _) =>
          if (o != magicId(st))
            odeNamesDeclared.get(getCls(o, st)).map(_.foreach { n =>
              if (!contNamesActive.exists { case (ao, an) => ao == o && an == n.x })
                throw ContinuousDynamicsUndefined(o, n, Pretty.pprint(getObjectField(o, classf, st)), getTime(st))
            })
      }
    }
  
  /** Summarize result of evaluating the hypotheses of all objects. */
  def testHypotheses(st: Enclosure, timeDomainLo: Double, timeDomainHi: Double, p: Prog, old: Metadata): Metadata = {
    def testHypothesesOneChangeset(hs: Set[CollectedHypothesis]) =
      if (hs isEmpty) NoMetadata
      else SomeMetadata(
        (for (CollectedHypothesis(o, s, h, env) <- hs) yield {
          lazy val counterEx = dots(h).toSet[Dot].map(d => d -> (evalExpr(d, env, st) : GValue))
          (o, getCls(o,st), s) -> ((evalExpr(h, env, st), getResultType(st)) match {
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
    val VLit(GInt(maxBranches))                    = getInSimulator(ParamMaxBranches._1,                   st.enclosure)
    val VLit(GInt(maxIterationsPerBranch))         = getInSimulator(ParamMaxIterationsPerBranch._1,        st.enclosure)
    val VLit(GBool(intersectWithGuardBeforeReset)) = getInSimulator(ParamIntersectWithGuardBeforeReset._1, st.enclosure)
    require(st.branches.nonEmpty, "hybridEncloser called with zero branches")
    require(st.branches.size < maxBranches, s"Number of branches (${st.branches.size}) exceeds maximum ($maxBranches).")
    Logger.debug(s"hybridEncloser (over $T, ${st.branches.size} branches)")
    
    // Strategy for merging the branches
    def mergeBranches(ics: List[InitialCondition]): List[InitialCondition] =
      ics.groupBy(ic => (ic._2, ic._3)).map { case ((m, t), ic) => (ic.map(_._1).reduce(_ /\ _), m, t) }.toList
 
    // Process a list of ICs   
    @tailrec def enclose
      ( pwlW:  List[InitialCondition] // Waiting ICs, the statements that yielded them and time where they should be used
      , pwlR:  List[Enclosure]        // Enclosure (valid over all of T)
      , pwlU:  List[InitialCondition] // Branches, i.e. states possibly valid at right end-point of T (ICs for next time segment)
      , pwlP:  List[Enclosure]        // Passed states
      , iterations: Int               // Remaining iterations
      ): Store =
      // No more IC to process
      if (pwlW isEmpty) {
        EnclosureAndBranches((pwlR union pwlP).reduce(_ /\ _), if (T.isThin) pwlU else mergeBranches(pwlU))      
      }
      // Exceeding the maximum iterations per branch
      else if (iterations / st.branches.size > maxIterationsPerBranch)
        sys.error(s"Enclosure computation over $T did not terminate in ${st.branches.size * maxIterationsPerBranch} iterations.")
      // Processing the next IC
      else {
        Logger.trace(s"enclose (${pwlW.size} waiting initial conditions, iteration $iterations)")
        val (w, q, t) :: waiting = pwlW
        // An element may be ignored if it was passed already 
        if (isPassed(w, pwlP))
           enclose(waiting, pwlR, pwlU, pwlP, iterations + 1)
        else {
          // The set of active ChangeSets
          val hw = active(w, prog)
          checkValidChange(hw)
          if (q.nonEmpty && isFlow(q.head) && Set(q.head) == hw && t == UnknownTime)
            sys error "Model error!" // Repeated flow, t == UnknownTime means w was created in this time step
          // Process the active ChangeSets
          val (newW, newR, newU) = encloseHw((w, q, t), hw)
          
          enclose(waiting ::: newW, pwlR ::: newR, pwlU ::: newU, w :: pwlP, iterations + 1)
        }
      }
    // Process the active ChangeSets
    def encloseHw
      ( wqt: InitialCondition, hw: Set[Changeset]
      ): (List[InitialCondition], List[Enclosure], List[InitialCondition]) = {
      val (w, qw, t) = wqt
      hw.foldLeft((List.empty[InitialCondition], List.empty[Enclosure], List.empty[InitialCondition])) {
        case ((tmpW, tmpR, tmpU), q) =>
          // q is not a flow
          if (!isFlow(q)) 
            // we process q if T is thin
            if (T.isThin ||
            // or T is not thin, but the time is unknown    
               t == UnknownTime) {
              Logger.trace(s"encloseHw (Not a flow)")
              val wi = 
                if (intersectWithGuardBeforeReset)
                  contract(w, q.dis.map(da => CollectedConstraint(da.selfCId, da.path, da.env)), prog)
                    .fold(sys error "Empty intersection while contracting with guard. " + _, i => i)
                else w
              ((wi(q.dis), q :: qw, t) :: tmpW, tmpR, tmpU)
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
            (tmpW, tmpR, (w, qw, StartTime) :: tmpU)
          // T is not thin, the flow is processed
          else {
            checkFlowDefined(prog, q, w)
            val s = continuousEncloser(q.odes, q.eqs, q.claims, T, prog, w)
            val r = s.range
            val rp = contract(r, q.claims, prog).right.get
            val (newW, newU) = handleEvent(q, qw, r, rp, if (t == StartTime) s.end else r)
            (newW ::: tmpW, rp :: tmpR, newU ::: tmpU)
          }
      }
    }
    def handleEvent(q: Changeset, past: Evolution, r: Enclosure, rp: Enclosure, u: Enclosure): (List[InitialCondition], List[InitialCondition]) = {
      val hr = active(r, prog)
      val hu = active(u, prog) 
      val up = contract(u, q.claims, prog)
      val e = q :: past
      if (noEvent(q, hr, up)) { // no event
        Logger.trace("handleEvent (No event)")
        (Nil, (u, e, StartTime) :: Nil)
      } else if (certainEvent(q, hr, hu, up)) { // certain event
        Logger.trace("handleEvent (Certain event)")
        ((rp, e, UnknownTime) :: Nil, Nil)
      } else { // possible event
        Logger.trace(s"handleEvent (Possible event, |hr| = ${hr.size}, q.ass = {${q.dis.map(Pretty pprint _.a).mkString(", ")}})")
        Logger.trace(s"handleEvent hr.odes = ${hr.map{cs => cs.odes.map(Pretty pprint _.a).mkString(", ")}}, hr.dis = ${hr.map{cs => cs.dis.map(Pretty pprint _.a).mkString(", ")}}")
        ((rp, e, UnknownTime) :: Nil, (up.right.get, e, StartTime) :: Nil)
      }
    }
    enclose(st.branches, Nil, Nil, Nil, 0)
  }
  
  /**
   * Returns true if there is only a single change set is active over the current time segment, this 
   * change set contains no discrete assignments and the claim in q is not violated entirely by u.
   */
  def noEvent(q: Changeset, hr: Set[Changeset], up: Either[String,Enclosure]) =
    hr.size == 1 && q.dis.isEmpty && (up match {
      case Left(s) => sys.error("Inconsistent model. A claim was invalidated without any event taking place. " + s) 
      case Right(_) => true
    })
    
  /** Returns true if both:
   *  1) More than one change set is active over the current time segment or q contains an assignment
   *  2) The change sets active at the end-point of the current time segment do not contain q or the  
   *     claim in q violates the enclosure at the end-point of the current time segment. */
  def certainEvent(q: Changeset, hr: Set[Changeset], hu: Set[Changeset], up: Either[String,Enclosure]): Boolean = {
    (hr.size > 1 || q.dis.nonEmpty) && // Some event is possible 
      (!(hu contains q) || up.isLeft) // Some possible event is certain 
  }
  
  /** Returns true if some element of passed contains s */
  def isPassed(s: Enclosure, P: List[Enclosure]) = P exists (_ contains s)
  
  /** Returns true if the discrete assignments in cs are empty or have no effect on s. */
  def isFlow(cs: Changeset) = cs.dis.isEmpty

  /**
   * Contract st based on all claims.
   * NOTE: Returns Left if the support of any of the claims has an empty intersection with st,
   *       or if some other exception is thrown by contract.
   */
  def contract(st: Enclosure, claims: Iterable[CollectedConstraint], prog: Prog): Either[String, Enclosure] = {
    /**
     * Given a predicate p and store st, removes that part of st for which p does not hold.
     * NOTE: The range of st is first computed, as contraction currently only works on intervals.  
     */
    def contract(st: Enclosure, p: Expr, prog: Prog, env: Env, selfCId: CId): Option[Enclosure] = {
      lazy val box = envBox(p, env, st)
      val varNameToFieldId = varNameToFieldIdMap(st)
      val noUpdate = Map[(CId,Name), CValue]()
      def toAssoc(b: Box) = b.map{ case (k, v) => (varNameToFieldId(k), VLit(Real(v))) }
      p match {
        case Lit(CertainTrue | Uncertain) => Some(st)
        case Lit(CertainFalse) => None
        case Op(Name("||",0), _) => Some(st)
        case Op(Name("&&",0), List(l,r)) => 
          (contract(st,l,prog,env,selfCId), contract(st,r,prog,env,selfCId)) match {
            case (Some(pil),Some(pir)) => pil intersect pir
            case _ => None
          }
        case Op(Name(op,0), List(l,r)) =>
          val lv = evalExpr(l, env, st)
          val rv = evalExpr(r, env, st)
          lazy val le = acumenExprToExpression(l,selfCId,env,st,prog)
          lazy val re = acumenExprToExpression(r,selfCId,env,st,prog)
          /** Based on acumen.interpreters.enclosure.Contract.contractEq */
          def eq[T](lde: GDiscreteEnclosure[T], rde: GDiscreteEnclosure[T]) = {
            val i = lde.range intersect rde.range
            def mapping(obj: Expr, n: Name) = {
              val VObjId(Some(objId)) = evalExpr(obj, env, st)
              (objId.cid, n) -> VLit((lde, rde) match {
                case (_: GStrEnclosure, _: GStrEnclosure)   => GStrEnclosure(i)
                case (_: GBoolEnclosure, _: GBoolEnclosure) => GBoolEnclosure(i)
              })
            }
            Map((l, r) match {
              case (Dot(obj, ln), _) => mapping(obj, ln)
              case (_, Dot(obj, rn)) => mapping(obj, rn)
              case _ => sys.error(s"Can not apply '$op' to operands (${Pretty pprint l}, ${Pretty pprint r})")
            })
          }
          /** Based on acumen.interpreters.enclosure.Contract.contractNeq */
          def neq[T](lvs: GDiscreteEnclosure[T], rvs: GDiscreteEnclosure[T]) = {
            val ranl = lvs.range
            val ranr = rvs.range
            if (!(ranl.size == 1) || !(ranr.size == 1) || !(ranl == ranr)) noUpdate
            else sys.error("contracted to empty box") // only when both lhs and rhs are thin can equality be established
          }
          val smallerBox = (op,lv,rv) match {
            case ("==", VLit(lvgv: GStrEnclosure),  VLit(rvgv: GStrEnclosure))  => eq(lvgv, rvgv)
            case ("==", VLit(lvgv: GBoolEnclosure), VLit(rvgv: GBoolEnclosure)) => eq(lvgv, rvgv)
            case ("~=", VLit(lvgv: GStrEnclosure),  VLit(rvgv: GStrEnclosure))  => neq(lvgv, rvgv)
            case ("~=", VLit(lvgv: GBoolEnclosure), VLit(rvgv: GBoolEnclosure)) => neq(lvgv, rvgv)
            case ("==", _, _)       => toAssoc(contractInstance.contractEq(le, re)(box))
            case ("~=", _, _)       => toAssoc(contractInstance.contractNeq(le, re)(box))
            case ("<=" | "<", _, _) => toAssoc(contractInstance.contractLeq(le, re)(box))
            case (">=" | ">", _, _) => toAssoc(contractInstance.contractLeq(re, le)(box))

          } 
          Some(st update smallerBox)
        case _ => 
          Some(st) // Do not contract
      }
    }
    claims.foldLeft(Right(st): Either[String, Enclosure]) {
      case (res, claim) => res match {
        case Right(r) => 
          try {
            contract(r, claim.c, prog, claim.env, claim.selfCId) map 
              (Right(_)) getOrElse Left("Empty enclosure after applying claim " + pprint(claim.c))
          } catch {
            case e: Throwable =>
              Logger.trace(Pretty pprint asProgram(st, prog))
              if (e.isInstanceOf[AcumenError]) throw e
              else Left("Error while applying claim " + pprint(claim.c) + ": " + e.getMessage) 
          }
        case _ => res
      }
    }
  }
  
  /** Box containing the values of all variables (Dots) that occur in it. */
  def envBox(e: Expr, env: Env, st: Enclosure): Box = {
    new Box(dots(e).flatMap{ case d@Dot(obj,n) =>
      val VObjId(Some(objId)) = evalExpr(obj, env, st) 
      evalExpr(d, env, st) match {
        case VLit(r: Real) => (fieldIdToName(objId.cid,n), r.range) :: Nil
        case VLit(r: GStrEnclosure) => Nil
      }
    }.toMap)
  }

  val solver = if (contraction) new LohnerSolver {} else new PicardSolver {}
  val extract = new acumen.interpreters.enclosure.Extract{}

  /**
   * Compute an enclosure for the system of equations corresponding to
   * odes and eqs put together.
   * 
   * NOTE: In the process, eqs are in-lined into odes in order to obtain
   *       an explicit system of ODEs. Values for the LHS of eqs are then
   *       obtained by evaluating the RHS in the ODE solutions over T.
   *       This approach to supporting mixed differential and algebraic
   *       equations implies that algebraic loops in the combined system
   *       are not allowed.
   */
  def continuousEncloser
    ( odes: Set[CollectedAction] // Set of delayed ContinuousAction
    , eqs: Set[CollectedAction]
    , claims: Set[CollectedConstraint]
    , T: Interval
    , p: Prog
    , st: Enclosure
    ): Enclosure = {
    val ic = contract(st.end, claims, p) match {
      case Left(_) => sys.error("Initial condition violates claims {" + claims.map(c => pprint(c.c)).mkString(", ") + "}.")
      case Right(r) => r
    }
    val varNameToFieldId = varNameToFieldIdMap(ic)
    val F = getFieldFromActions(inline(eqs, odes, st), ic, p) // in-line eqs to obtain explicit ODEs
    val stateVariables = varNameToFieldId.keys.toList
    val A = new Box(stateVariables.flatMap{ v => 
      val (o,n) = varNameToFieldId(v)
      (ic(o)(n): @unchecked) match {
        case VLit(ce:Real) => (v, ce.end) :: Nil
        case VObjId(_) => Nil
        case e => sys.error((o,n) + " ~ " + e.toString)
      }
    }.toMap)
    val solutions = solver.solveIVP(F, T, A, delta = 0, m = 0, n = 200, degree = 1) // FIXME Expose as simulator parameters
    val solutionMap = solutions._1.apply(T).map{
      case (k, v) => (varNameToFieldId(k), VLit(Real(A(k), v, solutions._2(k))))
    }
    val odeSolutions = ic update solutionMap
    val equationsMap = inline(eqs, eqs, st).map { // LHSs of EquationsTs, including highest derivatives of ODEs
      case CollectedAction(_, cid, Continuously(EquationT(ResolvedDot(_, _, n), rhs)), env) =>
        (cid, n) -> evalExpr(rhs, env, odeSolutions)
    }.toMap
    odeSolutions update equationsMap
  }

  /** Convert odes into a Field compatible with acumen.interpreters.enclosure.ivp.IVPSolver. */
  def getFieldFromActions(odes: Set[CollectedAction], st: Enclosure, p: Prog): Field =
    Field(odes.map { case CollectedAction(_, cid, Continuously(EquationI(ResolvedDot(_, _, n), rhs)), env) =>
      (fieldIdToName(cid, n), acumenExprToExpression(rhs, cid, env, st, p))
    }.toMap)
  
  /**
   * In-line the variables defined by the equations in as into the equations in bs. 
   * This means that, in the RHS of each element of b, all references to variables that are 
   * defined by an equation in a.
   * 
   * NOTE: This implies a restriction on the programs that are accepted: 
   *       If the program contains an algebraic loop, an exception will be thrown.
   */
  def inline(as: Set[CollectedAction], bs: Set[CollectedAction], st: CStore): Set[CollectedAction] = {
    val resolvedDotToDA = as.map(da => da.lhs -> da).toMap
    def inline(hosts: Map[CollectedAction,List[CollectedAction]]): Set[CollectedAction] = {
      val next = hosts.map {
        case (host, inlined) =>
          dots(host.rhs).foldLeft((host, inlined)) {
          case (prev@(hostPrev, ildPrev), d) =>
              resolvedDotToDA.get(resolveDot(d, host.env, st)) match {
              case None => prev // No assignment is active for d
              case Some(inlineMe) => 
                if (inlineMe.lhs.obj          == hostPrev.lhs.obj && 
                    inlineMe.lhs.field.x      == hostPrev.lhs.field.x &&
                    inlineMe.lhs.field.primes == hostPrev.lhs.field.primes + 1)
                  hostPrev -> inlined // Do not in-line derivative equations
                else {
                  val daNext = hostPrev.copy(
                    a = hostPrev.a match {
                      case Continuously(e: EquationI) =>
                        val dot = Dot(inlineMe.lhs.obj,inlineMe.lhs.field)
                        Continuously(e.copy(rhs = substitute(dot, inlineMe.rhs, e.rhs)))
                      case Continuously(e: EquationT) =>
                        val dot = Dot(inlineMe.lhs.obj,inlineMe.lhs.field)
                        Continuously(e.copy(rhs = substitute(dot, inlineMe.rhs, e.rhs)))
                    }, 
                    env = hostPrev.env ++ inlineMe.env)
                  daNext -> (inlineMe :: inlined)
                }
            }
        }
      }
      val reachedFixpoint = next.forall{ case (da, inlined) =>
        val dupes = inlined.groupBy(identity).collect{ case(x,ds) if ds.length > 1 => x }.toList
        lazy val loop = inlined.reverse.dropWhile(!dupes.contains(_))
        if (dupes isEmpty)
          hosts.get(da) == Some(inlined)
        else 
          throw internalPosError("Algebraic loop detected: " + loop.map(a => pprint (a.lhs: Expr)).mkString(" -> "), loop.head.lhs.pos)
      }
      if (reachedFixpoint) next.keySet
      else inline(next)
    }
    inline(bs.map(_ -> Nil).toMap)
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
  
  def acumenExprToExpression(e: Expr, selfCId: CId, env: Env, st: Enclosure, p: Prog): Expression = {
    def convert(x: Expr) = acumenExprToExpression(x, selfCId, env, st, p)
    import util.Names.op
    e match {
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
        val VObjId(Some(obj)) = evalExpr(objExpr, env, st) 
        fieldIdToName(obj, n)
      case ResolvedDot(obj,_,n) => 
        fieldIdToName(obj, n)
      case Op(Name("-"      , 0), List(x))    => Negate(convert(x))
      case Op(Name("abs"    , 0), List(x))    => Abs(convert(x))
      case Op(Name("sin"    , 0), List(x))    => Sin(convert(x))
      case Op(Name("cos"    , 0), List(x))    => Cos(convert(x))
      case Op(Name("tan"    , 0), List(x))    => Tan(convert(x))
      case Op(Name("acos"   , 0), List(x))    => ACos(convert(x))
      case Op(Name("sqrt"   , 0), List(x))    => Sqrt(convert(x))
      case Op(Name("tan"    , 0), List(x))    => Tan(convert(x))
      case Op(Name("asin"   , 0), List(x))    => ASin(convert(x))  
      case Op(Name("atan"   , 0), List(x))    => ATan(convert(x))  
      case Op(Name("exp"    , 0), List(x))    => Exp(convert(x))   
      case Op(Name("log"    , 0), List(x))    => Log(convert(x))   
      case Op(Name("log10"  , 0), List(x))    => Log10(convert(x)) 
      case Op(Name("sqrt"   , 0), List(x))    => Sqrt(convert(x))  
      case Op(Name("cbrt"   , 0), List(x))    => Cbrt(convert(x))  
      case Op(Name("ceil"   , 0), List(x))    => Ceil(convert(x))  
      case Op(Name("floor"  , 0), List(x))    => Floor(convert(x)) 
      case Op(Name("sinh"   , 0), List(x))    => Sinh(convert(x))  
      case Op(Name("cosh"   , 0), List(x))    => Cosh(convert(x))  
      case Op(Name("tanh"   , 0), List(x))    => Tanh(convert(x))  
      case Op(Name("signum" , 0), List(x))    => Signum(convert(x)) 
      case Op(Name("-"      , 0), List(l, r)) => convert(l) - convert(r)
      case Op(Name("+"      , 0), List(l, r)) => convert(l) + convert(r)
      case Op(Name("*"      , 0), List(l, r)) => convert(l) * convert(r)
      case Op(Name("^"      , 0), List(l, r)) => Pow(convert(l), convert(r))
      case Op(Name("/"      , 0), List(l, r)) => Divide(convert(l), convert(r))
      case Op(Name("atan2"  , 0), List(l, r)) => ATan2(convert(l), convert(r))
      case Op(Name("min"    , 0), List(l, r)) => Min(convert(l), convert(r))
      case Op(Name("max"    , 0), List(l, r)) => Max(convert(l), convert(r))
      case _                            => sys.error("Handling of expression " + e + " not implemented!")
    }
  }
  
  /* Utilities */
  
  def fieldIdToName(o: CId, n: Name): String = s"$n@$o"
  
  def internalError(s: String): AcumenError = new AcumenError {
    def mesg = s 
  }

  def internalPosError(s: String, p: Position): PositionalAcumenError = new PositionalAcumenError {
    def mesg = s
  }.setPos(p)
    
  
}
