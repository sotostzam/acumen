package acumen
package interpreters
package optimized2015

import scala.collection.immutable.HashMap
import scala.collection.immutable.Map
import scala.collection.mutable.ArrayBuffer
import scala.util.parsing.input.{Positional,Position,NoPosition}

import acumen.Errors._
import acumen.Pretty._
import acumen.util.Conversions._
import acumen.util.Random
import acumen.interpreters.Common._
import acumen.util.Canonical.{
  childrenOf, 
  classf,
  cmain,
  cmagic,
  endTime,
  nextChild,
  parentOf,
  parent,
  seedOf,
  seed1,
  seed2,
  self,
  resultType,
  stateVars,
  time,
  timeStep
}
import acumen.util.ASTUtil.dots
import scala.annotation.tailrec

object Common {
  type Store = Object
  type ObjId = Object
  type Val = Value[ObjId]


  /** The environment for the ODE solver
   *  @param odeVals    The store for the ode solver.  The corresponding equations are in PhaseParms#odes.
   *  @param assignVals Assignment value cache to avoid repeated evaluation and cache algebraic loops.
   */
  case class OdeEnv(
    odeVals: IndexedSeq[Val], 
    assignVals: Array[AssignVal],
    simulator: Object ) 
  {
    /** Create an empty assignment cache the same size of the existing one */
    def emptyAssignVals() = Array.fill[AssignVal](assignVals.length)(Unknown)
  }

  case class Env(env: Map[Name, Val], odeEnv: Option[OdeEnv] = None) extends Environment[Val] {
    override def apply(n: Name) = env.apply(n)
    override def get(n: Name) = env.get(n)
    override def +(v: (Name,Val)) = Env(env + v, odeEnv)
    override def ++(v: Map[Name,Val]) = Env(env ++ v, odeEnv)
    override def empty = Env.empty
  }
  object Env {
    val empty = new Env(Map.empty)
    def apply(nvs: (Name, Val)*): Env = new Env(Map(nvs:_*), None)
  }
  
  type MMap[A, B] = scala.collection.mutable.Map[A, B]
  val MMap = scala.collection.mutable.Map

  /** The representation of a value.
   *
   * When lastUpdated == PhaseParms#curIter the value was set in the current
   * iteration otherwise it has not been set yet.
   *
   */
  class ValVal(initVal: Val = VObjId(None)) {
    /** The previous set value. */
    var prevSetVal : Val = VObjId(None)
    /** The last set value. */
    var lastSetVal : CurVal = NormalVal(initVal)
    /** The line and column number the value was last set. */
    var lastSetPos : Position = NoPosition
    /** The iteration number (from PhaseParms#curIter) the lastSetVal was last updated on. */
    var lastUpdated : Int = -1 
  }

  /** A representation of the last set value */
  sealed abstract class CurVal {
    /* Gets the current value.
     *
     * Should only be called when it is known that the value is set to a
     * NormalVal. */
    def asVal : Val = throw ShouldNeverHappen();
  }
  case class NormalVal(v: Val) extends CurVal {override def asVal = v;}
  case class OdeLookup(idx: Int) extends CurVal
  case class AssignLookup(idx: Int) extends CurVal

  sealed abstract class AssignVal;
  case class KnownVal(v: Val) extends AssignVal
  case object Unknown extends AssignVal
  case object Evaluating extends AssignVal

  sealed abstract class EvalMode;
  case object Ignore     extends EvalMode;
  case object Now        extends EvalMode;
  case object Gather     extends EvalMode;
  case object Preserve   extends EvalMode;
  case object CreateOnly extends EvalMode;
  case object Structural extends EvalMode;

  /** Phase parameters and common state shared by all objects. */
  class PhaseParms {
    /** The current iteration number, used by ValVal to determine the
      * current and previous value. */
    var curIter : Int = 0;
    /** If set than use the value of the previous iteration */
    var usePrev = true;
    /** How to handle Discrete Assignments */
    var doEquationD : EvalMode = Ignore;
    /** How to handle EquationT's */
    var doEquationT : EvalMode = Ignore;
    /** How to handle EquationI's */
    var doEquationI : EvalMode = Ignore;
    /** Flag for Hypothesis check*/
    var doHypothesis : Boolean = false
    /** Left endpoint of the timeDomain where hypotheses are checked */
    var hypTimeDomainLeft : Double = 0
    // don't go inside conditionals when doing Hypotheses as all
    // Hypotheses are suppose to be at the top level
    def doConditionals = !doHypothesis
    /** Gathered equations for the ODE solver */
    var odes = new ArrayBuffer[Equation];
    /** Gathered continuous assignments */
    var assigns = new ArrayBuffer[(Equation, Position)];
    /** Gathered discrete assignments */
    var das = new ArrayBuffer[Assignment];
    //** Gathered VObjIds of newly created objects */
    var creates = new ArrayBuffer[Any];
    /** Metadata */
    var metaData : Metadata = NoMetadata
    /** Hypotheses results accumulated during one state */
    type HypResult = Map[ (CId, ClassName, Option[String]), (Option[HypothesisOutcome], Option[HypothesisOutcome], HypothesisOutcome) ]
    var stepHypothesisResults : HypResult = Map.empty
    
    def reset(doD : EvalMode, doT: EvalMode, doI: EvalMode) {
      usePrev = true
      doEquationD = doD
      doEquationT = doT
      doEquationI = doI
      doHypothesis = false
      odes.clear()
      doD match {
        case Ignore | Gather => 
          das.clear()
          creates.clear()
        case _ =>
      }
      doT match {
        case Ignore | Gather => assigns.clear()
        case _ =>
      }
    }
  }

  case class Assignment(id: ObjId, field: Name, v: Val, pos: Position)
  case class Equation(id: ObjId, field: Name, rhs: Expr, env: Map[Name, Val])
    
  case class Object(
      val id: CId,
      var fields: MMap[Name, ValVal],
      var phaseParms: PhaseParms,
      var parent: Option[Object],
      var ccounter: Int,
      var seed: (Int, Int),
      var children: Vector[Object]) extends GId {
    override def cid = id
    override def hashCode = System.identityHashCode(this)
    def className = 
      if (fields contains classf) pprint(fields(classf).lastSetVal.asVal)
      else "?"
    override def toString = {
      className + "@" + hashCode
    }
    override def equals(o: Any) = { eq(o.asInstanceOf[AnyRef]) }
    val fieldsCur = new Iterable[(Name, GValue)] {
      def iterator = new Iterator[(Name, GValue)] {
        val orig = fields.iterator
        override def hasNext = orig.hasNext
        override def next() = {val v = orig.next; (v._1, v._2.lastSetVal.asVal)}
      }
    }
  }

  val emptyStore: CStore = Map.empty

  def repr(st: Store): CStore = {
    def convertValue(v: Val): CValue =
      v match {
        case VObjId(None)    => VObjId(None)
        case VObjId(Some(t)) => VObjId(Some(t.id))
        case VList(vs)       => VList(vs map (convertValue(_)))
        case VVector(vs)     => VVector(vs map (convertValue(_)))
        case VLit(l)         => VLit(l)
        case VClassName(cn)  => VClassName(cn)
        case VResultType(st) => VResultType(st)
      }
    def convertObject(o: Object): CObject = {
      val p = VObjId(o.parent match {
        case None    => None
        case Some(p) => Some(p.id)
      })
      val fields = Map.empty ++ o.fields
      (fields mapValues {v => convertValue(v.lastSetVal.asVal)}) +
        ((parent, p), (nextChild, VLit(GInt(o.ccounter))), (seed1, VLit(GInt(o.seed._1))), (seed2, VLit(GInt(o.seed._2))))
    }
    def convertStore(st: Store): CStore = {
      val set: scala.collection.mutable.Set[Store] =
        scala.collection.mutable.Set.empty
      def helper(s: Store): CStore = {
        if (set contains s)
          emptyStore
        else {
          set += s
          val cs = s.children map (helper(_))
          val c = cs.foldLeft(emptyStore)((_ ++ _))
          c + ((s.id, convertObject(s)))
        }
      }
      helper(st)
    }
    convertStore(st)
  }

  def fromCStore(st: CStore, root: CId): Store = {
    val addresses: scala.collection.mutable.HashMap[CId, Object] =
      scala.collection.mutable.HashMap.empty
    def treated(id: CId) = addresses contains id
    def concat(bs: Iterable[Set[CId]]) =
      bs.foldLeft(Set.empty[CId])(_ ++ _)
    def referenced(o: CObject): Set[CId] = {
      def helper(v: CValue): Set[CId] =
        v match {
          case VList(l)        => concat(l map helper)
          case VVector(l)      => concat(l map helper)
          case VObjId(Some(a)) => Set(a)
          case _               => Set.empty[CId]
        }
      concat(o.values map helper)
    }
    def convertVal(v: Value[CId]): Val =
      v match {
        case VLit(gv)        => VLit(gv)
        case VList(l)        => VList(l map convertVal)
        case VVector(l)      => VVector(l map convertVal)
        case VObjId(Some(a)) => VObjId(Some(addresses(a)))
        case VObjId(None)    => VObjId(None)
        case VClassName(cn)  => VClassName(cn)
        case VResultType(s)  => VResultType(s)
      }
    def convertId(id: CId): Unit = {
      if (!treated(id)) {
        val res = Object(id, null, null, null, 0, null, null)
        addresses += ((id, res))
        val o = st(id)
        for (r <- referenced(o)) convertId(r)
        val cs = childrenOf(id, st)
        for (c <- cs) convertId(c)
        res.fields = MMap.empty ++ ((o - parent) mapValues {v => new ValVal(convertVal(v))})
        res.parent = parentOf(o) map (addresses(_))
        res.children = Vector.empty ++ (cs map (addresses(_)))
        res.seed = seedOf(o)
        val VLit(GInt(cc)) = o(nextChild)
        res.ccounter = cc
      }
    }
    for (id <- st.keys) convertId(id)
    val phaseParms = new PhaseParms
    addresses.foreach{case (_,obj) => obj.phaseParms = phaseParms}
    addresses(root)
  }

  /* return type of computation */
  sealed abstract class Changeset {
    def ||(that: Changeset): Changeset
  }
  case class NoChange() extends Changeset {
    def ||(that: Changeset) = that
  }
  case class SomeChange(dead: Set[ObjId], rps: Set[(ObjId, ObjId)]) extends Changeset {
    def ||(that: Changeset) =
      that match {
        case NoChange() => this
        case SomeChange(dead1, rps1) =>
          SomeChange(dead ++ dead1, rps ++ rps1)
      }
  }

  def noChange = NoChange()
  def logDead(o: ObjId) = SomeChange(Set(o), Set.empty)
  def logReparent(o: ObjId, p: ObjId) = SomeChange(Set.empty, Set((o, p)))
  def logModified = SomeChange(Set.empty, Set.empty)

  def asObjId(v: Val) = v match {
    case VObjId(Some(id)) => id
    case _ => throw NotAnObject(v)
  }
  def evalToObjId(e: Expr, p: Prog, env: Env) = evalExpr(e,p,env) match {
    case VObjId(Some(id)) => checkAccessOk(id, env, e); id
    case v => throw NotAnObject(v).setPos(e.pos)
  }

  /* get self reference in an env */
  def selfObjId(e: Env): ObjId =
    e(self) match {
      case VObjId(Some(a)) => a
      case _               => throw ShouldNeverHappen()
    }

  /** Get a value for a field, value must be a NormalVal
   *
   * Safe to use for discrete variables since they never have a special value. */
  def getField(o: Object, f: Name) = {
    val vv = o.fields(f)
    vv.lastSetVal match {
      case NormalVal(cv) => 
        if (o.phaseParms.usePrev && vv.lastUpdated == o.phaseParms.curIter) vv.prevSetVal
        else cv
      case _ =>
        throw ShouldNeverHappen()
    }
  }

  /** Get a value for a field by any means required.
   *
   * SIDE EFFECTS when handling AssignLookup
   *
   * If the current value is ToEval then the value will be evaluated and
   * the field updated with the computed value. */
  def getField(o: Object, f: Name, p: Prog, env: Env, pos: Position = NoPosition) = {
    try {
      val vv = o.fields(f)
      vv.lastSetVal match {
        case NormalVal(cv) => 
          if (o.phaseParms.usePrev && vv.lastUpdated == o.phaseParms.curIter) vv.prevSetVal
          else cv
        case OdeLookup(idx) => env.odeEnv match {
          case Some(odeEnv) => odeEnv.odeVals(idx)
          case None => vv.prevSetVal
        }
        case AssignLookup(idx) => env.odeEnv match {
          case Some(odeEnv) => odeEnv.assignVals(idx) match {
            case KnownVal(v) => v
            case Unknown => 
              val (a, _) = o.phaseParms.assigns(idx)
              odeEnv.assignVals(idx) = Evaluating
              val v = evalExpr(a.rhs, p, Env(a.env, env.odeEnv))
              odeEnv.assignVals(idx) = KnownVal(v)
              v
          case Evaluating =>
            //throw AlgebraicLoop(ObjField(o.id,o.className,f))
            throw AlgebraicLoop(ObjField(o.id,o.className,f), 
                                posIsSetPoint = true).setPos(vv.lastSetPos)
          }
          case None => vv.prevSetVal
        }
      }
    } catch {
      case e:AlgebraicLoop => throw e.addToChain(ObjField(o.id,o.className,f),pos)
    }
  }

  /* SIDE EFFECT */
  def setField(o: Object, f: Name, newVal: CurVal, pos0: Position, update: Boolean): Changeset = {
    var pos = pos0
    if (o.fields contains f) {
      val v = o.fields(f)
      if (update) {
        assert(v.lastUpdated == o.phaseParms.curIter)
        v.lastSetVal = newVal
        if (pos == NoPosition) pos = v.lastSetPos
      } else {
        if (v.lastUpdated == o.phaseParms.curIter) {
          throw DuplicateAssingmentUnspecified(f).setPos(pos).setOtherPos(v.lastSetPos)
        } else {
          v.prevSetVal = v.lastSetVal.asVal; v.lastSetVal = newVal; v.lastUpdated = o.phaseParms.curIter
        }
        v.lastSetPos = pos
      }
      newVal match {
        case NormalVal(nv) => 
          val oldVal = v.prevSetVal
          if (oldVal == nv) noChange
          // _3D and _3DView are special objects, not interferring with reaching a FixedPoint
          else if (f.x != "_3D" && f.x != "_3DView") {
            if (oldVal.yieldsPlots != nv.yieldsPlots)
              throw new UnsupportedTypeChangeError(f, o.id, getClassOf(o), oldVal, nv, 
                                                   "These values require a different number of plots")
            logModified
          } else noChange
        case _ => 
          noChange
      }
    }
    else throw VariableNotDeclared(f).setPos(pos)
  }

  def setField(o: Object, f: Name, newVal: CurVal, pos: Position): Changeset =
    setField(o, f, newVal, pos, false)
 
  def setField(o: Object, f: Name, newVal: Val, pos: Position = NoPosition): Changeset =
    setField(o, f, NormalVal(newVal), pos, false)
 
  def updateField(o: Object, f: Name, newVal: Val) =
    setField(o, f, NormalVal(newVal), NoPosition, true)

  /* get the class associated to an object */
  def getClassOf(o: Object): ClassName = {
    val VClassName(cn) = getField(o, classf)
    cn
  }

  /* retrieve Simulator when passed Main */
  def getSimulator(main: Object) =
    (main.children find (getField(_, classf) == VClassName(cmagic))) match {
      case Some(o) => o
      case None    => throw NoInstanceFound(cmagic)
    }

  /* */
  def getMetadata(main: Object) = main.phaseParms.metaData
  def setMetadata(main: Object, md: Metadata) = main.phaseParms.metaData = md

  /* fetch values in magic */
  def getTime(magic: Object) = extractDouble(getField(magic, time))
  def getTimeStep(magic: Object) = extractDouble(getField(magic, timeStep))
  def getEndTime(magic: Object) = extractDouble(getField(magic, endTime))
  def getResultType(magic: Object) = { val VResultType(t) = getField(magic, resultType); t }

  /* write in magic */
  /* SIDE EFFECT */
  def setTime(magic: Object, d: Double) = setField(magic, time, VLit(GDouble(d)))
  /* SIDE EFFECT */
  def setResultType(magic: Object, t: ResultType) = setField(magic, resultType, VResultType(t))
  /* SIDE EFFECT */
  def setVarNum(magic: Object, count: Int) = setField(magic, stateVars, VLit(GInt(count)))

  /* SIDE EFFECT */
  def changeParent(o: Object, p: Object): Unit = {
    o.parent match {
      case Some(op) => op.children = op.children diff Seq(o)
      case None     => ()
    }
    o.parent = Some(p)
    o.phaseParms = p.phaseParms
    p.children = p.children :+ o
  }

  /* SIDE EFFECT */
  def getNewSeed(o: Object): (Int, Int) = {
    val (sd1, sd2) = Random.split(o.seed)
    o.seed = sd1
    sd2
  }

  /* evaluate e in the scope of env 
   * for definitions p with current store st */
  def evalExpr(e: Expr, p: Prog, env: Env): Val = {
    def eval(env: Env, e: Expr): Val = try {
      e match {
        case Lit(i)        => VLit(i)
        case ExprVector(l) => VVector(l map (eval(env, _)))
        case Var(n)        => env.get(n).getOrElse(VClassName(ClassName(n.x)))
        case Input(s,i)    => Devices.getDeviceInput(extractInt(eval(env, s)), i)
        case Index(v,i)    => evalIndexOp(eval(env, v), i.map(x => eval(env, x)))
        case Dot(v, Name("children", 0)) =>
          val id = evalToObjId(v,p,env)
          VList((id.children map (c => VObjId(Some(c)))).toList)
        /* e.f */
        case Dot(e0, f) =>
          val id = evalToObjId(e0,p,env)
          getField(id, f, p, env, e.pos)
        /* x && y */
        case Op(Name("&&", 0), x :: y :: Nil) =>
          val VLit(GBool(vx)) = eval(env, x)
          if (!vx) VLit(GBool(false))
          else eval(env, y)
        /* x || y */
        case Op(Name("||", 0), x :: y :: Nil) =>
          val VLit(GBool(vx)) = eval(env, x)
          if (vx) VLit(GBool(true))
          else eval(env, y)
        /* op(args) */
        case Op(Name(op, 0), args) =>
          evalOp(op, args map (eval(env, _)))
        /* sum e for i in c st t */
        case Sum(e, i, c, t) =>
          def helper(acc: Val, v: Val) = {
            val VLit(GBool(b)) = eval(env + ((i, v)), t)
            if (b) {
              val ev = eval(env + ((i, v)), e)
              evalOp("+", List(acc, ev))
            }
            else acc
          }
          val vc = eval(env, c)
          val vs = vc match {
            case VList(vs)   => vs
            case VVector(vs) => vs
            case _           => throw NotACollection(vc)
          }
          vs.foldLeft(VLit(GDouble(0)): Val)(helper)
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
    eval(env, e)
  }.setPos(e.pos)

  sealed abstract class ParentParm
  case object IsMain extends ParentParm
  case class ParentIs(id: ObjId) extends ParentParm
  /* create an env from a class spec and init values */
  def mkObj(c: ClassName, p: Prog, prt: ParentParm, 
            sd: (Int, Int), v: List[Val], magic: Object, childrenCounter: Int = 0): Object = {
    val cd = classDef(c, p)
    val base = MMap((classf, new ValVal(VClassName(c))))
    val pub = base ++ (cd.fields zip v.map{new ValVal(_)})

    /* change [Init(x1,rhs1), ..., Init(xn,rhsn)]
       into   ([x1, ..., xn], [rhs1, ..., rhsn] */
    def helper(p: (List[Name], List[InitRhs]), i: Init) =
      (p, i) match { case ((xs, rhss), Init(x, rhs)) => (x :: xs, rhs :: rhss) }
    val (privVars, ctrs) = {
      val (xs, ys) = cd.priv.foldLeft[(List[Name], List[InitRhs])]((Nil, Nil))(helper)
      (xs.reverse, ys.reverse)
    }

    val res = prt match {
      case IsMain =>
        Object(CId.nil, pub, magic.phaseParms, None, childrenCounter, sd, Vector(magic))
      case ParentIs(p) =>
        val counter = p.ccounter
        p.ccounter += 1
        Object(counter :: p.id, pub, p.phaseParms, Some(p), childrenCounter, sd, Vector.empty)
    }

    val vs = ctrs map {
      case NewRhs(e, es) =>
        val cn = evalExpr(e, p, Env(self -> VObjId(Some(res)))) match {
          case VClassName(cn) => cn
          case v => throw NotAClassName(v).setPos(e.pos)
        }
        val ves = es map (evalExpr(_, p, Env(self -> VObjId(Some(res)))))
        val nsd = getNewSeed(res)
        VObjId(Some(mkObj(cn, p, ParentIs(res), nsd, ves, magic)))
      case ExprRhs(e) =>
        evalExpr(e, p, Env(self -> VObjId(Some(res))))
    }
    val priv = privVars zip vs.map{new ValVal(_)}
    res.fields = pub ++ priv
    prt match {
      case ParentIs(resp) =>
        resp.children = resp.children :+ res
      case IsMain => ()
    }
    res
  }

  def evalDiscreteAction(a: DiscreteAction, env: Env, p: Prog, magic: Object): Changeset =
    a match {
      case Assign(d@Dot(e, x), t) => 

        // "Now": Not used, kept for legacy
        if (magic.phaseParms.doEquationD == Now) { 
          val id = evalToObjId(e, p, env)
          val vt = evalExpr(t, p, env)
          setField(id, x, vt, d.pos)

        // "Gather": Collecting discrete assignments
        } else if (magic.phaseParms.doEquationD == Gather) {
          val id = evalToObjId(e, p, env)
          val vt = evalExpr(t, p, env)

          // Log the assignment
          magic.phaseParms.das.append(Assignment(id,x,vt,d.pos))

          noChange
        } else noChange

      case Assign(lhs, _) =>
        throw BadLhs().setPos(lhs.pos)
      case Create(lhs, e, es) =>

        // "Now": Not used, kept for legacy
        if (magic.phaseParms.doEquationD == Now) {

          val c = evalExpr(e, p, env) match {
            case VClassName(cn) => cn
            case v => throw NotAClassName(v).setPos(e.pos)
          }
          val ves = es map (evalExpr(_, p, env))
          val self = selfObjId(env)
          val sd = getNewSeed(self)

          val fa =  mkObj(c, p, ParentIs(self), sd, ves, magic)

          lhs match {
            case None => logModified
            case Some(d@Dot(e, x)) => 
              val id = evalToObjId(e, p, env)
              logModified || setField(id, x, VObjId(Some(fa)), d.pos)
            case Some(e) => throw BadLhs().setPos(e.pos)
          }
        
        // "Gather": Collecting discrete assignments
        } else if (magic.phaseParms.doEquationD == CreateOnly) {

          val self = selfObjId(env)
          val c = evalExpr(e, p, env) match {
            case VClassName(cn) => cn
            case v => throw NotAClassName(v).setPos(e.pos)
          }
          val ves = es map (evalExpr(_, p, env))
          val sd = getNewSeed(self)

          // Create the object
          val fa = mkObj(c, p, ParentIs(self), sd, ves, magic)

          // Log the new object
          magic.phaseParms.creates.append(fa)
          
          lhs match {
            case None => 
              logModified
            case Some(d@Dot(e, x)) => 
              val id = evalToObjId(e, p, env)
              
              // Log the assignment that makes the object accessible.
              magic.phaseParms.das.append(Assignment(id,x,VObjId(Some(fa)),d.pos))

              logModified // The || setField from "Now" would at most add another logModified so can be safely ignored 

            case Some(e) => throw BadLhs().setPos(e.pos)
          }
        } else noChange

      case Elim(e) => if (magic.phaseParms.doEquationD == Structural) {
        val id = evalToObjId(e, p, env)
        logDead(id)
       } else noChange
      case Move(Dot(o1, x), o2) => if (magic.phaseParms.doEquationD == Structural) {
        val o1Id = evalToObjId(o1, p, env)
        val xId = asObjId(getField(o1Id, x))
        checkIsChildOf(xId, o1Id, o1)
        val o2Id = evalToObjId(o2, p, env)
        logReparent(xId, o2Id)
       } else noChange
      case Move(obj, _) =>
        throw BadMove().setPos(obj.pos)
    }

  def evalContinuousAction(a: ContinuousAction, env: Env, p: Prog, magic: Object): Changeset =
    a match {
      case EquationT(d, t) => d match {
        case Dot(e, x) =>
          // "Now": Not used, kept for legacy
          if (magic.phaseParms.doEquationT == Now) {
            val id = evalToObjId(e, p, env)
            val vt = evalExpr(t, p, env)
            setField(id, x, vt, d.pos)

            // "Gather": Collecting continuous assignments
          } else if (magic.phaseParms.doEquationT == Gather) {
            val id = evalToObjId(e, p, env)

            // Log the assignment
            magic.phaseParms.assigns.append((Equation(id, x, t, env.env), d.pos))

            noChange
          } else noChange  
      }
      case EquationI(d@Dot(e, x), t) =>

        // "Now": Not used, kept for legacy
        if (magic.phaseParms.doEquationI == Now) { 
          val dt = getTimeStep(magic)
          val id = evalToObjId(e, p, env)
          val vt = evalExpr(t, p, env)
          val lhs = getField(id, x, p, env)
          setField(id, x, lhs match {
            case VLit(d) =>
              VLit(GDouble(extractDouble(d) + extractDouble(vt) * dt))
            case VVector(u) =>
              val us = extractDoubles(u)
              val ts = extractDoubles(vt)
              VVector((us, ts).zipped map ((a, b) => VLit(GDouble(a + b * dt))))
            case _ =>
              throw BadLhs()
          },d.pos)

        // "Gather": Collecting differential equations
        } else if (magic.phaseParms.doEquationI == Gather) { 
          val id = evalToObjId(e, p, env)
          val idx = magic.phaseParms.odes.length

          // Mark the field to be updated by an ode
          setField(id, x, OdeLookup(idx), d.pos)
          
          // Log the ode
          magic.phaseParms.odes.append(Equation(id,x,t,env.env))
          
          noChange
        } else noChange

      case _ =>
        throw ShouldNeverHappen() // FIXME: fix that with refinement types
    }

  def evalHypothesis(s: Option[String], e: Expr, env: Env, p: Prog, magic: Object) = {
    val self = selfObjId(env)
    val time = getTime(magic)
    val hypRes = computeHypothesisOutcomes(
      evalExpr(e, p, env), time, getResultType(magic),
      dots(e).toSet[Dot].map(d => d -> (evalExpr(d, p, env) : GValue)))
    val md = Map(((self.cid, getClassOf(self), s), hypRes))
    magic.phaseParms.stepHypothesisResults = magic.phaseParms.stepHypothesisResults ++ md
    noChange
  }

  def combine[A](xs: Traversable[A], f: A => Changeset): Changeset = {
    var res: Changeset = noChange
    for (x <- xs) res = res || f(x)
    res
  }

  def combine[A](xs: Traversable[Changeset]): Changeset = {
    var res: Changeset = noChange
    for (x <- xs) res = res || x
    res
  }

  def evalActions(as: List[Action], env: Env, p: Prog, magic: Object): Changeset = {
    combine(as, evalAction(_: Action, env, p, magic))
  }

  def evalAction(a: Action, env: Env, p: Prog, magic: Object): Changeset = {
    def vListToPattern(ls: List[Value[_]]): GPattern = 
      GPattern(ls.map(x => x match {
        case VLit(n) => n
        case VVector(nls) => vListToPattern(nls)            
      }))
    val pp = magic.phaseParms
    a match {
      case IfThenElse(c, a1, a2) => if (pp.doConditionals) {
        val VLit(GBool(b)) = evalExpr(c, p, env)
        if (b) evalActions(a1, env, p, magic)
        else evalActions(a2, env, p, magic)
      } else noChange
      case ForEach(i, l, b) => if (pp.doConditionals) {
        val seq = evalExpr(l, p, env)
        val vs = seq match {
          case VList(vs)   => vs
          case VVector(vs) => vs
          case _           => throw NotACollection(seq).setPos(l.pos)
        }
        combine(vs, ((v: Val) => evalActions(b, env + ((i, v)), p, magic)))
      } else noChange
      case Switch(s, cls) => if (pp.doConditionals) { s match {     
        case ExprVector(_) =>           
          val VVector(ls) = evalExpr(s, p, env)
            val gp = vListToPattern(ls)
            (cls find (_.lhs == gp)) match {
              case Some(c) => evalActions(c.rhs, env, p, magic)
              case None    => throw NoMatch(gp)
            }
        case _ => 
          val VLit(gv) = evalExpr(s, p, env)
          (cls find (_.lhs == gv)) match {
            case Some(c) => evalActions(c.rhs, env, p, magic)
            case None    => throw NoMatch(gv).setPos(s.pos)
          }
      }} else noChange
      case Discretely(da) =>
        // The object is not created within the same step
        if (!magic.phaseParms.creates.contains(selfObjId(env)) &&
            // We are checking for discrete actions.
            (magic.phaseParms.doEquationD == Gather || 
            magic.phaseParms.doEquationD == CreateOnly ||
            magic.phaseParms.doEquationD == Structural))
          evalDiscreteAction(da, env, p, magic)
        else
          noChange
      case Continuously(ca) =>
        if (magic.phaseParms.doEquationT == Gather)
          evalContinuousAction(ca, env, p, magic)
        else
          noChange
      case Claim(_) =>
        noChange
      case Hypothesis(s, e) => 
        if (pp.doHypothesis) evalHypothesis(s, e, env, p, magic)
        else noChange
    }
  }

  def evalStep(p: Prog, magic: Object)(o: Object): Changeset = {
    val as = classDef(getClassOf(o), p).body
    val env = HashMap((self, VObjId(Some(o))))
    assert(o.phaseParms eq magic.phaseParms)
    evalActions(as, Env(env), p, magic)
  }

  def traverse(f: ObjId => Changeset, root: ObjId): Changeset = {
    val r = f(root)
    val cs = root.children
    if (cs.isEmpty) r else r || combine(cs, traverse(f, _: ObjId))
  }

  /* runtime checks, should be disabled once we have type safety */

  def checkAccessOk(id:ObjId, env:Env, context: Expr) : Unit = {
    val sel = selfObjId(env)
    if (sel != id && ! (sel.children contains id)) 
      throw AccessDenied(id,sel,sel.children.toList).setPos(context.pos)
  }

  def checkIsChildOf(child:ObjId, parent:ObjId, context: Expr) : Unit = {
    if (! (parent.children contains child)) throw NotAChildOf(child,parent).setPos(context.pos)
  }

  /* IVP */

  case class FieldImpl(odes: ArrayBuffer[Equation], p: Prog) extends Field[OdeEnv,ObjId] {
    override def apply(s: OdeEnv) = 
      OdeEnv(odes.map{e => evalExpr(e.rhs, p, Env(e.env,Some(s)))}, s.emptyAssignVals, s.simulator)
    override def variables(s: OdeEnv): List[(ObjId, Name)] =
      odes.toList.map { da => (da.id, da.field) }
    override def map(nm: Name => Name, em: Expr => Expr) = 
      FieldImpl(odes.map(eqn => eqn.copy(field = nm(eqn.field), rhs = em(eqn.rhs))), p)
  }
  
  case class RichStoreImpl(s: OdeEnv) extends RichStore[OdeEnv,ObjId] {
    override def +++(that: OdeEnv) = OdeEnv((this.s.odeVals,that.odeVals).zipped.map{(a,b) => evalOp("+", List(a,b))},
                                            s.emptyAssignVals, s.simulator)
    override def ***(that: Double) = OdeEnv(this.s.odeVals.map{a => evalOp("*", List(a,VLit(GDouble(that))))},
                                            s.emptyAssignVals, s.simulator)
    override def map(m: Val => Val) = 
      OdeEnv( s.odeVals.map(m)
            , s.assignVals.map{ case KnownVal(v) => KnownVal(m(v)); case v => v }
            , s.simulator )
    lazy val odeValsLookup = s.simulator.phaseParms.odes.zipWithIndex.
      map{ case (o,i) => (o.id, o.field) -> i }.toMap
    override def apply(id: ObjId, n: Name): Val = s.odeVals(odeValsLookup(id,n))
    override def updated(id: ObjId, n: Name, v: Val) = s.copy(odeVals = s.odeVals.updated(odeValsLookup(id,n), v))
    override def getInSimulator(variable: String) = getField(s.simulator, Name(variable, 0))
  }
  
  implicit def liftStore(s: OdeEnv)(implicit field: FieldImpl): RichStoreImpl = RichStoreImpl(s)

  /**
   * Ensure that for each variable that has an ODE declared in the private section, there is 
   * an equation in scope at the current time step. This is done by checking that for each 
   * field name with a single prime the corresponding unprimed variable is updated.
   */
  def checkContinuousDynamicsAlwaysDefined(o: ObjId, magic: ObjId) : Unit = {
    val pp = o.phaseParms
    o.fields.foreach{case (n,v) => 
      if (n.primes == 1 && o.fields(Name(n.x, 0)).lastUpdated != pp.curIter)
          throw ContinuousDynamicsUndefined(o.id, n,None, Pretty.pprint(getField(o, classf)), getTime(magic));
      }
    o.children.foreach{child => checkContinuousDynamicsAlwaysDefined(child, magic)}
  }
  
}
