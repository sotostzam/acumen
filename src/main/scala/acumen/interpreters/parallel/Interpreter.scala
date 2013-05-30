package acumen
package interpreters
package parallel

import scala.collection.Map
import scala.collection.immutable.HashMap
import scala.collection.immutable.HashSet
import scala.collection.immutable.IntMap
import scala.collection.immutable.Queue
import scala.collection.immutable.BitSet
import scala.collection.immutable.Set
import scala.collection.immutable.Vector
import scala.math._

import Errors._
import Pretty._
import util.Names._
import util.Conversions._
import util.Random

import Common._
import util.Canonical._

trait Common {

  type Store = Object
  type ObjId = Object
  type Val = Value[ObjId]
  type Env = Map[Name, Val]

  type MMap[A,B] = scala.collection.mutable.Map[A,B]
  val  MMap = scala.collection.mutable.Map

  case class Object(
    val id       : CId,
    var fields   : MMap[Name, Val], 
    var parent   : Option[Object],
    var ccounter : Int,
		var seed     : (Int,Int),
    var children : Vector[Object]) {
      override def hashCode = System.identityHashCode(this)
      override def toString = {
        val cn = 
          if (fields contains classf) pprint(fields(classf))
          else "?"
        cn+"@"+hashCode
      }
      override def equals(o: Any) = { eq(o.asInstanceOf[AnyRef])  }
    }

  val emptyStore : CStore = Map.empty

  def repr (st:Store) : CStore = {
    def convertValue(v:Val): CValue = 
      v match {
        case VObjId(None) => VObjId(None)
        case VObjId(Some(t)) => VObjId(Some(t.id))
        case VList(vs) => VList(vs map (convertValue(_)))
        case VVector(vs) => VVector(vs map (convertValue(_)))
        case VLit(l) => VLit(l)
        case VClassName(cn) => VClassName(cn)
        case VStepType(st) => VStepType(st)
      }
    def convertObject(o:Object) : CObject = {
      val p = VObjId(o.parent match {
        case None    => None
        case Some(p) => Some(p.id)
      })
      val fields = Map.empty ++ o.fields
      (fields mapValues (convertValue(_))) + 
         ((parent   , p)
         ,(nextChild, VLit(GInt(o.ccounter)))
				 ,(seed1    , VLit(GInt(o.seed._1)))
         ,(seed2    , VLit(GInt(o.seed._2))))
    }
    def convertStore(st:Store) : CStore = {
      val set : scala.collection.mutable.Set[Store] = 
        scala.collection.mutable.Set.empty
      def helper(s:Store) : CStore = {
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

  def fromCStore(st:CStore, root:CId) : Store = {
    val addresses : scala.collection.mutable.HashMap[CId,Object] = 
      scala.collection.mutable.HashMap.empty
    def treated(id:CId) = addresses contains id
    def concat(bs:Iterable[Set[CId]]) = 
      bs.foldLeft(Set.empty[CId])(_ ++ _)
    def referenced(o:CObject) : Set[CId] = {
      def helper(v:CValue) : Set[CId] =
        v match {
          case VList(l) => concat(l map helper)
          case VVector(l) => concat(l map helper)
          case VObjId(Some(a)) => Set(a)
          case _ => Set.empty[CId]
        }
      concat(o.values map helper)
    }
    def convertVal(v:Value[CId]) : Val = 
      v match {
        case VLit(gv) => VLit(gv)
        case VList(l) => VList(l map convertVal)
        case VVector(l) => VVector(l map convertVal)
        case VObjId(Some(a)) => VObjId(Some(addresses(a)))
        case VObjId(None) => VObjId(None)
        case VClassName(cn) => VClassName(cn)
        case VStepType(s) => VStepType(s)
      }
    def convertId(id:CId) : Unit = {
      if (!treated(id)) {
        val res = Object(id, null, null, 0, null, null)
        addresses += ((id, res))
        val o = st(id)
        for (r <- referenced(o)) convertId(r)
        val cs = childrenOf(id, st)
        for (c <- cs) convertId(c)
        res.fields = MMap.empty ++ ((o-parent) mapValues convertVal)
        res.parent = parentOf(o) map (addresses(_))
        res.children = Vector.empty ++ (cs map (addresses(_)))
				res.seed = seedOf(o)
        val VLit(GInt(cc)) = o(nextChild)
        res.ccounter = cc
      }
    }
    for (id <- st.keys) convertId(id)
    addresses(root)
  }

  /* return type of computation */
  sealed abstract class Changeset {
    def || (that : Changeset) : Changeset
  }
  case class NoChange() extends Changeset {
    def || (that : Changeset) = that
  }
  case class SomeChange(dead:Set[ObjId], rps:Set[(ObjId,ObjId)]) extends Changeset {
    def || (that : Changeset) = 
      that match {
        case NoChange() => this
        case SomeChange(dead1, rps1) => 
          SomeChange(dead ++ dead1, rps ++ rps1)
      }
  }

  def noChange = NoChange()
  def logDead(o:ObjId) = SomeChange(Set(o), Set.empty)
  def logReparent(o:ObjId, p:ObjId) = SomeChange(Set.empty, Set((o,p)))
  def logModified = SomeChange(Set.empty, Set.empty)

  /* get self reference in an env */
  def selfObjId(e:Env) : ObjId =
    e(self) match {
      case VObjId(Some(a)) => a
      case _         => throw ShouldNeverHappen()
    }

  /* objects fields setters and getters */
  def getField(o:Object, f:Name) = o.fields(f)

  /* SIDE EFFECT */
  def setField(o:Object, f:Name, v:Val) : Changeset = 
    if (o.fields contains f) {
      if (o.fields(f) == v) noChange
      else { o.fields(f) = v; logModified }
    }
    else throw VariableNotDeclared(f)

  /* get the class associated to an object */
  def getClassOf(o:Object) : ClassName = {
    val VClassName(cn) = getField(o, classf)
    cn
  }

  /* retrieve Simulator when passed Main */
  def getSimulator(main:Object) =
    (main.children find (getField(_,classf) == VClassName(cmagic))) match {
      case Some(o) => o
      case None    => throw NoInstanceFound(cmagic)
    }

  /* fetch values in magic */
  def getTime(magic:Object) = extractDouble(getField(magic, time))
  def getTimeStep(magic:Object) = extractDouble(getField(magic, timeStep))
  def getEndTime(magic:Object) = extractDouble(getField(magic, endTime))
  def getStepType(magic:Object) = { val VStepType(t) = getField(magic, stepType); t }

  /* write in magic */
  /* SIDE EFFECT */
  def setTime(magic:Object, d:Double) = setField(magic, time, VLit(GDouble(d)))
  /* SIDE EFFECT */
  def setStepType(magic:Object, t:StepType) = setField(magic, stepType, VStepType(t))

  /* SIDE EFFECT 
     NOT THREAD SAFE */
  def changeParent(o:Object, p:Object) : Unit = {
    o.parent match {
      case Some(op) => op.children = op.children diff Seq(o)
      case None     => ()
    }
    o.parent = Some(p)
    p.children = p.children :+ o
  }

	/* SIDE EFFECT */
	def getNewSeed(o:Object) : (Int,Int) = {
		val (sd1, sd2) = Random.split(o.seed)
		o.seed = sd1
		sd2
	}

  /* evaluate e in the scope of env 
   * for definitions p with current store st */
  def evalExpr(e:Expr, p:Prog, env:Env) : Val = {
    def eval(env:Env, e:Expr) : Val = {
	    e match {
  	    case Lit(i)         => VLit(i)
        case ExprVector(l)  => VVector (l map (eval(env,_)))
        case Var(n)         => env(n)
        case Dot(v,Name("children",0)) =>
          val VObjId(Some(id)) = eval(env,v)
          //id synchronized { VList((id.children map VObjId[ObjId]).toList) }
          VList((id.children map (c => VObjId(Some(c)))).toList)
        /* e.f */
        case Dot(e,f) =>
          val VObjId(Some(id)) = eval(env, e)
          getField(id, f)
        /* x && y */
        case Op(Name("&&",0), x::y::Nil) =>
          val VLit(GBool(vx)) = eval(env,x)
            if (!vx) VLit(GBool(false))
            else eval(env,y)
        /* x || y */
        case Op(Name("||",0),x::y::Nil) =>
          val VLit(GBool(vx)) = eval(env,x)
            if (vx) VLit(GBool(true))
            else eval(env,y)
        /* op(args) */
        case Op(Name(op,0),args) =>
          evalOp(op, args map (eval(env,_)))
        /* sum e for i in c st t */
        case Sum(e,i,c,t) =>
          def helper(acc:Val, v:Val) = {
            val VLit(GBool(b)) = eval(env+((i,v)), t)
            if (b) {
              val ev = eval(env+((i,v)), e)
              evalOp("+", List(acc, ev))
            } else acc
          }
          val vc = eval(env,c)
          val vs = vc match { 
            case VList(vs) => vs 
            case VVector(vs) => vs 
            case _ => throw NotACollection(vc)
          }
          vs.foldLeft(VLit(GDouble(0)):Val)(helper)
        case TypeOf(cn) =>
          VClassName(cn)
      }
    }
    eval(env,e)
  }

  /* create an env from a class spec and init values */
  def mkObj (c:ClassName, p:Prog, prt:Option[ObjId], sd:(Int,Int),
             v:List[Val], magic:Object, childrenCounter:Int=0) : Object = {
    val cd = classDef(c,p)
    val base = MMap((classf, VClassName(c)))
    val pub = base ++ (cd.fields zip v)

    /* change [Init(x1,rhs1), ..., Init(xn,rhsn)]
       into   ([x1, ..., xn], [rhs1, ..., rhsn] */
    def helper(p:(List[Name],List[InitRhs]),i:Init) = 
      (p,i) match { case ((xs,rhss), Init(x,rhs)) => (x::xs, rhs::rhss) }
    val (privVars, ctrs) = {
      val (xs,ys) = cd.priv.foldLeft[(List[Name],List[InitRhs])]((Nil,Nil))(helper)
      (xs.reverse, ys.reverse)
    }

    val res = prt match {
      case None => 
        Object(CId.nil, pub, prt, childrenCounter, sd, Vector.empty)
      case Some(p) =>
        val counter = p.ccounter
        p.ccounter += 1
        Object(counter :: p.id, pub, prt, childrenCounter, sd, Vector.empty)
    }

    val vs = ctrs map {
      case NewRhs(cn, es) =>
        val ves = es map (evalExpr(_, p, Map(self -> VObjId(Some(res)))))
				val nsd = getNewSeed(res)
        VObjId(Some(mkObj(cn, p, Some(res), nsd, ves, magic)))
      case ExprRhs(e) =>
        evalExpr(e, p, Map(self -> VObjId(Some(res))))
    }
    val priv = privVars zip vs
    res.fields = pub ++ priv
    prt match {
      case Some(resp) => 
        resp.children = resp.children :+ res
      case None => ()
    }
    res
  }

  def evalDiscreteAction(a:DiscreteAction, env:Env, p:Prog, magic:Object) : Changeset =
    a match {
      case Assign(Dot(e,x),t) =>
        val VObjId(Some(id)) = evalExpr(e, p, env)
        val vt = evalExpr(t, p, env)
        setField(id, x, vt)
      case Assign(_,_) => 
        throw BadLhs()
      case Create(lhs, c, es) => 
        val ves = es map (evalExpr(_, p, env))
        val self = selfObjId(env)
			  val sd = getNewSeed(self)
        val fa  = mkObj(c, p, Some(self), sd, ves, magic)
        lhs match { 
          case None => logModified
          case Some(Dot(e,x)) => 
            val VObjId(Some(id)) = evalExpr(e, p, env)
            logModified || setField(id, x, VObjId(Some(fa))) 
          case Some(_) => throw BadLhs()
        }
      case Elim(e) =>
        val VObjId(Some(id)) = evalExpr(e, p, env)
        logDead(id)
      case Move(Dot(o1,x), o2) => 
        val VObjId(Some(o1Id)) = evalExpr(o1, p, env)
        val VObjId(Some(xId))  = getField(o1Id, x)
        val VObjId(Some(o2Id)) = evalExpr(o2, p, env)
        logReparent(xId, o2Id)
      case Move(_,_) =>
        throw BadMove()
    }

  def evalContinuousAction(a:ContinuousAction, env:Env, p:Prog, magic:Object) = 
    a match {
      case EquationT(Dot(e,x),t) =>
        val VObjId(Some(a)) = evalExpr(e, p, env)
        val vt = evalExpr(t, p, env)
        setField(a, x, vt)
      case EquationI(Dot(e,x),t) =>
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
            VVector((us,ts).zipped map ((a,b) => VLit(GDouble(a + b * dt))))
          case _ =>
            throw BadLhs()
        })
      case _ =>
        throw ShouldNeverHappen() // FIXME: fix that with refinement types
    }

  def combine[A](xs:Traversable[A], f:A => Changeset) : Changeset = {
    var res : Changeset = noChange
    for (x <- xs) res = res || f(x)
    res
  }

  def combine[A](xs:Traversable[Changeset]) : Changeset = {
    var res : Changeset = noChange
    for (x <- xs) res = res || x
    res
  }

  def evalActions(as:List[Action], env:Env, p:Prog, magic:Object) : Changeset = 
    combine (as, evalAction(_:Action, env, p, magic))
  
  def evalAction(a:Action, env:Env, p:Prog, magic:Object) : Changeset = {
    a match {
      case IfThenElse(c, a1, a2) =>
        val VLit(GBool(b)) = evalExpr(c, p, env)
        if (b) evalActions(a1, env, p, magic)
        else evalActions(a2, env, p, magic)
      case ForEach(i, l, b) => 
        val seq = evalExpr(l, p, env)
        val vs = seq match { 
          case VList(vs) => vs 
          case VVector(vs) => vs 
          case _ => throw NotACollection(seq)
        }
        combine (vs, ((v:Val) => evalActions(b, env+((i,v)), p, magic)))
      case Switch(s,cls) =>
        val VLit(gv) = evalExpr(s, p, env)
        (cls find (_.lhs == gv)) match {
          case Some(c) => evalActions(c.rhs, env, p, magic)
          case None    => throw NoMatch(gv)
        }
      case Discretely(da) =>
        val ty = getStepType(magic)
        if (ty == Discrete()) 
          evalDiscreteAction(da, env, p, magic)
        else noChange
      case Continuously(ca) =>
        val ty = getStepType(magic)
        if (ty == Continuous()) 
          evalContinuousAction(ca, env, p, magic)
        noChange
    }
  }

  def evalStep(p:Prog, magic:Object)(o:Object) : Changeset ={
    val as = classDef(getClassOf(o), p).body
    val env = HashMap((self, VObjId(Some(o))))
    evalActions(as, env, p, magic)
  }

  def splitInto(size:Int, chunks:Int) : Option[List[(Int,Int)]] = {
    if (chunks > size) None
    else {
      val len = size / chunks
      val rem = size % chunks
      def helper1(i:Int, r:Int, acc:List[(Int,Int)]): List[(Int,Int)] = {
        if (r > 0) helper2(i, len, r-1, i, acc)
        else helper2(i, len-1, r, i, acc)
      }
      def helper2(i:Int, k:Int, r:Int, start:Int, acc:List[(Int,Int)]): List[(Int,Int)]= {
        if (i==size-1) {
          ((start,i)::acc).reverse
        } else if (k == 0) {
          helper1(i+1, r, ((start,i)::acc))
        } else {
          helper2(i+1, k-1, r, start, acc)
        }
      }
      Some(helper1(0, rem, Nil))
    }
  }

  def magicClassTxt = 
    """class Simulator(time, timeStep, endTime, stepType, lastCreatedId) end"""
  def initStoreTxt  = 
    """#0.0 { className = Simulator, parent = none, time = 0.0, timeStep = 0.01, 
              endTime = 10.0, stepType = @Discrete, nextChild = 0,
						  seed1 = 0, seed2 = 0 }"""

  lazy val magicClass = Parser.run(Parser.classDef, magicClassTxt)
  lazy val magicCObj  = Parser.run(Parser.store, initStoreTxt)

  def init(prog:Prog) : (Prog, Store) = {
    val magic = fromCStore(magicCObj, CId(0))
    /* WARNING: the following line works because there is no children access check
       if one of the instructions of the provate section tries to access magic,
       and there was a check, this would crash (which we don't want) */
		val (sd1,sd2) = Random.split(Random.mkGen(0))
    val mainObj = mkObj(cmain, prog, None, sd1, List(VObjId(Some(magic))), magic, 1)
		magic.seed = sd2
    changeParent(magic, mainObj)
    val cprog = CleanParameters.run(prog, CStoreInterpreterType)
    val sprog = Simplifier.run(cprog)
    val mprog = Prog(magicClass :: sprog.defs)
    (mprog , mainObj)
  }
}

object Interpreter extends Common {

  def withInterpreter[A](nbThreads:Int)(f: Interpreter => A) : A = {
     val pi = new interpreters.parallel.Interpreter(nbThreads)
     try { f(pi) } finally { pi.dispose }
  }
  
}

class Interpreter(nbThreads: Int) extends Common with acumen.CStoreInterpreter {

  lazy val threadPool = new SimpleThreadPool[Changeset](nbThreads)
  def dispose = threadPool.dispose

  def step(p:Prog, st:Store) : Option[Store] = {
    threadPool.reset
    val magic = getSimulator(st)
    if (getTime(magic) > getEndTime(magic)) None
    else Some(
      { val chtset = 
          if (nbThreads > 1) iterateMain(evalStep(p, magic), st, nbThreads)
          else iterateSimple(evalStep(p, magic), st)
        getStepType(magic) match {
          case Discrete() =>
           chtset match {
              case SomeChange(dead, rps) =>
                for ((o,p) <- rps) 
                  changeParent(o,p)
                for (o <- dead) {
                  o.parent match {
                    case None => ()
                    case Some(op) => 
                      for (oc <- o.children) changeParent(oc, op)
                      op.children = op.children diff Seq(o)
                  }
                }
              case NoChange() => 
                setStepType(magic, Continuous())
            }
          case Continuous() =>
            setStepType(magic, Discrete())
            setTime(magic, getTime(magic) + getTimeStep(magic))
        }
        st
      }
    )
  }

  def parCombine[A](xs:Traversable[A], f: A => Changeset) : Changeset = {
    val boxes = xs map { x => threadPool.run(() => f(x)) }
    var res : Changeset = noChange
    for (b <- boxes) res = res || b.get 
    res
  }

  def recurse(f:ObjId => Changeset, cs:Vector[ObjId], n:Int) : Changeset = {
    val len = cs.length
    splitInto(len, n) match {
      case Some(slices) =>
        parCombine[(Int,Int)](
          slices, 
          { case (b,e) =>
              var res : Changeset = noChange
              for (i <- b to e) res = res || iterateSimple(f, cs(i))
              res
          })
      case None =>
        if (len>1) {
          val tasks = n-len+1 
          val quot = tasks / len
          val rem = tasks % len
          val (load::loads) = List.fill(rem)(quot+1) ::: List.fill(len-rem)(quot)
          val threads = (cs zip loads) map {
            case (c,i) => threadPool.run( () => iterateThreaded(f,c,i) )
          }
          var res = iterateThreaded(f,cs.last,load)
          for (t <- threads) res = res || t.get
          res
        } else if (len == 1) {
          iterateThreaded(f, cs.head, n)
        } else noChange
    }
  }

  /* precondition: n != 0 */
  def iterateMain(f:ObjId => Changeset, root:ObjId, n:Int) : Changeset = {
    val r = f(root)
    val cs = root.children filter (getField(_,classf) != VClassName(cmagic))
    r || recurse(f, cs, n)
  }

  def iterateThreaded(f:ObjId => Changeset, root:ObjId, n:Int) : Changeset = {
    if (n==0) iterateSimple(f, root)
    else {
      val r = f(root)
      val cs = root.children
      r || recurse(f, cs, n)
    }
  }

  def iterateSimple(f:ObjId => Changeset, root:ObjId) : Changeset = {
    val r = f(root)
    val cs = root.children
    r || combine(cs, iterateSimple(f,_:ObjId))
  }
}


