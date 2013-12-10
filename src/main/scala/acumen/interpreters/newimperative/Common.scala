package acumen
package interpreters
package newimperative

import scala.collection.immutable.HashMap

import acumen.Errors._
import acumen.Pretty._
import acumen.util.Conversions._
import acumen.util.Random
import acumen.interpreters.Common.{ classDef, evalOp, evalIndexOp }
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
  time,
  timeStep
}
import scala.annotation.tailrec

object Common {
  type Store = Object
  type ObjId = Object
  type Val = Value[ObjId]
  type Env = Map[Name, Val]
  
  type MMap[A, B] = scala.collection.mutable.Map[A, B]
  val MMap = scala.collection.mutable.Map

  class ValVal(v: Val = VObjId(None)) {
    var prevVal : Val = VObjId(None)
    var curVal : Val = v
    var lastUpdated : Int = -1
  }

  class PhaseParms {
    var curIter : Int = 0;
    var delayUpdate = false;
    var doDiscrete = false;
    var doEquationT = false;
    var doEquationI = false;
  }

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
    override def toString = {
      val cn =
        if (fields contains classf) pprint(fields(classf).curVal)
        else "?"
      cn + "@" + hashCode
    }
    override def equals(o: Any) = { eq(o.asInstanceOf[AnyRef]) }
    val fieldsCur = new Iterable[(Name, GValue)] {
      def iterator = new Iterator[(Name, GValue)] {
        val orig = fields.iterator
        override def hasNext = orig.hasNext
        override def next() = {val v = orig.next; (v._1, v._2.curVal)}
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
      (fields mapValues {v => convertValue(v.curVal)}) +
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

  /* get self reference in an env */
  def selfObjId(e: Env): ObjId =
    e(self) match {
      case VObjId(Some(a)) => a
      case _               => throw ShouldNeverHappen()
    }

  /* objects fields setters and getters */
  def getField(o: Object, f: Name) = {
    val v = o.fields(f)
    if (o.phaseParms.delayUpdate && v.lastUpdated == o.phaseParms.curIter) v.prevVal
    else v.curVal
  }

  /* SIDE EFFECT */
  def setField(o: Object, f: Name, newVal: Val): Changeset =
    if (o.fields contains f) {
      val oldVal = getField(o, f)
      val v = o.fields(f)
      //println(f + " oldVal:" + oldVal + "  prevVal: " + v.prevVal + "  curVal: " + v.curVal + "  newVal: " + newVal + "  lastUpdated: " + v.lastUpdated + "  curIter: " + o.phaseParms.curIter + "  delayUpdate: " + o.phaseParms.delayUpdate)
      if (v.lastUpdated == o.phaseParms.curIter) {
        if (o.phaseParms.delayUpdate && v.curVal != newVal) throw DuplicateAssingmentUnspecified(f)
        v.curVal = newVal
      } else {
        v.prevVal = v.curVal; v.curVal = newVal; v.lastUpdated = o.phaseParms.curIter
      }
      if (oldVal == newVal) noChange
      else logModified
    }
    else throw VariableNotDeclared(f)

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

  /* SIDE EFFECT 
     NOT THREAD SAFE */
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
    def eval(env: Env, e: Expr): Val = {
      e match {
        case Lit(i)        => VLit(i)
        case ExprVector(l) => VVector(l map (eval(env, _)))
        case Var(n)        => env.get(n).getOrElse(VClassName(ClassName(n.x)))
        case Index(v,i)    => evalIndexOp(eval(env, v), eval(env, i))
        case Dot(v, Name("children", 0)) =>
          val VObjId(Some(id)) = eval(env, v)
          //id synchronized { VList((id.children map VObjId[ObjId]).toList) }
          VList((id.children map (c => VObjId(Some(c)))).toList)
        /* e.f */
        case Dot(e, f) =>
          val VObjId(Some(id)) = eval(env, e)
          getField(id, f)
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
    }
    eval(env, e)
  }

  /* create an env from a class spec and init values */
  def mkObj(c: ClassName, p: Prog, prt: Option[ObjId], sd: (Int, Int),
            v: List[Val], magic: Object, childrenCounter: Int = 0): Object = {
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
      case None =>
        Object(CId.nil, pub, new PhaseParms, prt, childrenCounter, sd, Vector.empty)
      case Some(p) =>
        val counter = p.ccounter
        p.ccounter += 1
        Object(counter :: p.id, pub, p.phaseParms, prt, childrenCounter, sd, Vector.empty)
    }

    val vs = ctrs map {
      case NewRhs(e, es) =>
        val cn = evalExpr(e, p, Map(self -> VObjId(Some(res)))) match {
          case VClassName(cn) => cn
          case v => throw NotAClassName(v)
        }
        val ves = es map (evalExpr(_, p, Map(self -> VObjId(Some(res)))))
        val nsd = getNewSeed(res)
        VObjId(Some(mkObj(cn, p, Some(res), nsd, ves, magic)))
      case ExprRhs(e) =>
        evalExpr(e, p, Map(self -> VObjId(Some(res))))
    }
    val priv = privVars zip vs.map{new ValVal(_)}
    res.fields = pub ++ priv
    prt match {
      case Some(resp) =>
        resp.children = resp.children :+ res
      case None => ()
    }
    res
  }

  def evalDiscreteAction(a: DiscreteAction, env: Env, p: Prog, magic: Object): Changeset =
    a match {
      case Assign(Dot(e, x), t) =>
        val VObjId(Some(id)) = evalExpr(e, p, env)
        val vt = evalExpr(t, p, env)
        setField(id, x, vt)
      case Assign(_, _) =>
        throw BadLhs()
      case Create(lhs, e, es) =>
        val c = evalExpr(e, p, env) match {
          case VClassName(cn) => cn
          case v => throw NotAClassName(v)
        }
        val ves = es map (evalExpr(_, p, env))
        val self = selfObjId(env)
        val sd = getNewSeed(self)
        val fa = mkObj(c, p, Some(self), sd, ves, magic)
        lhs match {
          case None => logModified
          case Some(Dot(e, x)) =>
            val VObjId(Some(id)) = evalExpr(e, p, env)
            logModified || setField(id, x, VObjId(Some(fa)))
          case Some(_) => throw BadLhs()
        }
      case Elim(e) =>
        val VObjId(Some(id)) = evalExpr(e, p, env)
        logDead(id)
      case Move(Dot(o1, x), o2) =>
        val VObjId(Some(o1Id)) = evalExpr(o1, p, env)
        val VObjId(Some(xId)) = getField(o1Id, x)
        val VObjId(Some(o2Id)) = evalExpr(o2, p, env)
        logReparent(xId, o2Id)
      case Move(_, _) =>
        throw BadMove()
    }

  def evalContinuousAction(a: ContinuousAction, env: Env, p: Prog, magic: Object): Changeset =
    a match {
      case EquationT(Dot(e, x), t) => if (magic.phaseParms.doEquationT) {
        val VObjId(Some(a)) = evalExpr(e, p, env)
        val vt = evalExpr(t, p, env)
        setField(a, x, vt)
      } else noChange
      case EquationI(Dot(e, x), t) => if (magic.phaseParms.doEquationI) {
        val dt = getTimeStep(magic)
        val VObjId(Some(id)) = evalExpr(e, p, env)
        val vt = evalExpr(t, p, env)
        val lhs = getField(id, x)
        setField(id, x, lhs match {
          case VLit(d) =>
            VLit(GDouble(extractDouble(d) + extractDouble(vt) * dt))
          case VVector(u) =>
            val us = extractDoubles(u)
            val ts = extractDoubles(vt)
            VVector((us, ts).zipped map ((a, b) => VLit(GDouble(a + b * dt))))
          case _ =>
            throw BadLhs()
        })
      } else noChange
      case _ =>
        throw ShouldNeverHappen() // FIXME: fix that with refinement types
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
    a match {
      case IfThenElse(c, a1, a2) =>
        val VLit(GBool(b)) = evalExpr(c, p, env)
        if (b) evalActions(a1, env, p, magic)
        else evalActions(a2, env, p, magic)
      case ForEach(i, l, b) =>
        val seq = evalExpr(l, p, env)
        val vs = seq match {
          case VList(vs)   => vs
          case VVector(vs) => vs
          case _           => throw NotACollection(seq)
        }
        combine(vs, ((v: Val) => evalActions(b, env + ((i, v)), p, magic)))
      case Switch(s, cls) =>
        val VLit(gv) = evalExpr(s, p, env)
        (cls find (_.lhs == gv)) match {
          case Some(c) => evalActions(c.rhs, env, p, magic)
          case None    => throw NoMatch(gv)
        }
      case Discretely(da) =>
        if (magic.phaseParms.doDiscrete)
          evalDiscreteAction(da, env, p, magic)
        else noChange
      case Continuously(ca) =>
        evalContinuousAction(ca, env, p, magic)
    }
  }

  def evalStep(p: Prog, magic: Object)(o: Object): Changeset = {
    val as = classDef(getClassOf(o), p).body
    val env = HashMap((self, VObjId(Some(o))))
    assert(o.phaseParms eq magic.phaseParms)
    evalActions(as, env, p, magic)
  }

  def traverseSimple(f: ObjId => Changeset, root: ObjId): Changeset = {
    val r = f(root)
    val cs = root.children
    if (cs.isEmpty) r else r || combine(cs, traverseSimple(f, _: ObjId))
  }
  
}
