/* QUESTION: What if we want to have a termination condition rather than
             a termination time?  */

/* We need to document very clearly and very carefully why ifthenelse is a
   statement, and if there are any benefits that are lost by doing that. */

/* FIXME:  We'd like to keep the names of operators like 
   "sum" around, but we'd like their semantics be defined simply by using the
   environment to look up a semantic constant that does everything interest at
   the level of values.  */

package acumen
package interpreters
package reference
package experimental

import Eval._

import Common._
import util.Names._
import util.Canonical
import util.Canonical._
import util.Conversions._
import util.Random

import scala.collection.immutable.HashMap
import scala.collection.immutable.HashSet
import scala.collection.immutable.Queue
import scala.math._
import Stream._
import Errors._

object Interpreter extends acumen.CStoreInterpreter {

  override def id = Array("experimental")

  type Store = CStore
  type Env = Map[Name, CValue]

  def repr(st:Store) = st
  def fromCStore(st:CStore, root:CId) = st

  /* initial values */
  val emptyStore : Store = HashMap.empty
  val emptyEnv   : Env   = HashMap.empty

  /* get a fresh object id for a child of parent */
  def freshCId(parent:Option[CId]) : Eval[CId] = parent match {
    case None => pure(CId.nil)
    case Some(p) =>
      for { VLit(GInt(n)) <- asks(getObjectField(p, nextChild, _))
            _ <- setObjectFieldM(p, nextChild, VLit(GInt(n+1)))
      } yield (n :: p)
  }
  
  /* get self reference in an env */
  def selfCId(e:Env) : CId =
    e(self) match {
      case VObjId(Some(a)) => a
      case _ => throw ShouldNeverHappen()
    }

  /* promoting canonical setters, 'M' is for Monad */

  def changeParentM(id:CId, p:CId) : Eval[Unit] = 
    promote(changeParent(id,p,_))
	def changeSeedM(id:CId, s:(Int,Int)) : Eval[Unit] = 
		promote(changeSeed(id,s,_))
  def setObjectM(id:CId, o:CObject) : Eval[Unit] =
    promote(setObject(id,o,_))
  def setObjectFieldM(id:CId, f:Name, v:CValue) : Eval[Unit] = {
    promote(setObjectField(id,f,v,_))
	}

  /* splits self's seed into (s1,s2), assigns s1 to self and returns s2 */
  def getNewSeed(self:CId) : Eval[(Int,Int)] = {
    for { (s1,s2) <- asks(getSeed(self,_)) map Random.split
	  _ <- changeSeedM(self, s1)
    } yield s2
  }
  
  /* transfer parenthood of a list of objects,
   * whose ids are "cs", to address p */
  def reparent(cs:List[CId], p:CId) : Eval[Unit] =
    mapM_ (logReparent(_:CId,p), cs)
    
  /* discretely assign the value v to a field n in object o */
  def assign(o: CId, n: Name, v:CValue) : Eval[Unit] = logAssign(o, n, v)

  /* continuously assign the value v to a field n in object o */
  def equation(o: CId, n: Name, v:CValue) : Eval[Unit] = logEquation(o, n, v)
  
  /* log an id as being dead */
  def kill(a:CId) : Eval[Unit] = logCId(a)
  
  /* transfer the parenthood of (object at a)'s 
   * children to (object at a)'s parent */
  def vanish(a:CId) : Eval[Unit] = { 
    for { Some(p) <- asks(getParent(a,_))
          cs <- asks(childrenOf(a,_))
    } reparent(cs,p) >> kill(a)
  }
    
  /* create an env from a class spec and init values */
  def mkObj(c:ClassName, p:Prog, prt:Option[CId], sd:(Int,Int),
            v:List[CValue], childrenCounter:Int =0) : Eval[CId] = {
    val cd = classDef(c,p)
    val base = HashMap(
      (classf, VClassName(c)),
      (parent, VObjId(prt)),
      (seed1, VLit(GInt(sd._1))),
      (seed2, VLit(GInt(sd._2))),
      (nextChild, VLit(GInt(childrenCounter))))
    val pub = base ++ (cd.fields zip v)

    // the following is just for debugging purposes:
    // the type system should ensure that property
    if (cd.fields.length != v.length) 
      throw ConstructorArity(cd, v.length)
  
    /* change [Init(x1,rhs1), ..., Init(xn,rhsn)]
       into   ([x1, ..., xn], [rhs1, ..., rhsn] */
    def helper(p:(List[Name],List[InitRhs]),i:Init) = 
      (p,i) match { case ((xs,rhss), Init(x,rhs)) => (x::xs, rhs::rhss) }
    val (privVars, ctrs) = {
      val (xs,ys) = cd.priv.foldLeft[(List[Name],List[InitRhs])]((Nil,Nil))(helper)
      (xs.reverse, ys.reverse)
    }
       
    for { fid <- freshCId(prt)
          _ <- setObjectM(fid, pub)
          vs <- mapM[InitRhs, CValue]( 
                  { case NewRhs(e,es) =>
                      for { ve <- asks(evalExpr(e, p, Map(self -> VObjId(Some(fid))), _)) 
                            val cn = ve match {case VClassName(cn) => cn; case _ => throw NotAClassName(ve)}
                            ves <- asks(st => es map (
                            evalExpr(_, p, Map(self -> VObjId(Some(fid))), st)))
			    nsd <- getNewSeed(fid)
			    oid <- mkObj(cn, p, Some(fid), nsd, ves)
                      } yield VObjId(Some(oid))
                    case ExprRhs(e) =>
                      asks(evalExpr(e, p, Map(self -> VObjId(Some(fid))), _))
                  },
                  ctrs)
          val priv = privVars zip vs 
          // new object creation may have changed the nextChild counter
          newpub <- asks(deref(fid,_))
          _ <- setObjectM(fid, newpub ++ priv)
    } yield fid
  }


  /* runtime checks, should be disabled once we have type safety */

  def checkAccessOk(id:CId, env:Env, st:Store) : Unit = {
    val sel = selfCId(env)
    lazy val cs = childrenOf(sel, st)
    if (sel != id && ! (cs contains id))
      throw AccessDenied(id,sel,cs)
  }

  def checkIsChildOf(child:CId, parent:CId, st:Store) : Unit = {
    val cs = childrenOf(parent, st)
    if (! (cs contains child)) throw NotAChildOf(child,parent)
  }

  /* evaluate e in the scope of env 
   * for definitions p with current store st */
  def evalExpr(e:Expr, p:Prog, env:Env, st:Store) : CValue = {
    def eval(env:Env, e:Expr) : CValue = {
	    e match {
  	    case Lit(i)         => VLit(i)
        case ExprVector(l)  => VVector (l map (eval(env,_)))
        case Var(n)         => env.get(n).getOrElse(VClassName(ClassName(n.x)))
        case Dot(o,Name("children",0)) =>
          /* In order to avoid redundancy en potential inconsistencies, 
             each object has a pointer to its parent instead of having 
             each object maintain a list of its children. This is why the childrens'
             list has to be computed on the fly when requested. 
             An efficient implementation wouldn't do that. */
          val id = extractId(eval(env,o))
          checkAccessOk(id, env, st)
          VList(childrenOf(id,st) map (c => VObjId(Some(c))))
        /* e.f */
        case Dot(e,f) =>
          val id = extractId(eval(env, e))
          checkAccessOk(id, env, st)
          getObjectField(id, f, st)
        /* FIXME:
           Could && and || be expressed in term of ifthenelse ? 
           => we would need ifthenelse to be an expression  */
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
          def helper(acc:CValue, v:CValue) = {
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
          vs.foldLeft(VLit(GDouble(0)):CValue)(helper)
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
    eval(env,e)
  }

  def evalActions(as:List[Action], env:Env, p:Prog) : Eval[Unit] =
    mapM_((a:Action) => evalAction(a, env, p), as)
  
  def evalAction(a:Action, env:Env, p:Prog) : Eval[Unit] = {
    a match {
      case IfThenElse(c,a1,a2) =>
        for (VLit(GBool(b)) <- asks(evalExpr(c, p, env, _)))
          if (b) evalActions(a1, env, p)
          else   evalActions(a2, env, p)
      case ForEach(i,l,b) => 
        for (seq <- asks(evalExpr(l, p, env, _))) {
          val vs = seq match { 
            case VList(vs) => vs 
            case VVector(vs) => vs 
            case _ => throw NotACollection(seq)
          }
          mapM_((v:CValue) => evalActions(b, env+((i,v)), p), vs)
        }
      case Switch(s,cls) =>
        for (VLit(gv) <- asks(evalExpr(s, p, env, _))) {
          (cls find (_.lhs == gv)) match {
            case Some(c) => evalActions(c.rhs, env, p)
            case None    => throw NoMatch(gv)
          }
        }
      case Discretely(da) =>
        for (ty <- asks(getResultType))
          if (ty == FixedPoint) pass
          else evalDiscreteAction(da, env, p)
      case Continuously(ca) =>
        for (ty <- asks(getResultType))
          if (ty != FixedPoint) pass
          else evalContinuousAction(ca, env, p) 
    }
  }
 
  def evalDiscreteAction(a:DiscreteAction, env:Env, p:Prog) : Eval[Unit] =
    a match {
      case Assign(d@Dot(e,x),t) => 
        /* Schedule the discrete assignment if it changes x, otherwise do nothing */
        for { id <- asks(evalExpr(e, p, env, _)) map extractId
        	  vt <- asks(evalExpr(t, p, env, _))
        	  _  <- asks(checkAccessOk(id, env, _))
        	  vx <- asks(evalExpr(d, p, env, _)) 
        } if (vt != vx) assign(id, x, vt) else pass
      /* Basically, following says that variable names must be 
         fully qualified at this language level */
      case Assign(_,_) => 
        throw BadLhs()
      case Create(lhs, e, es) =>
        for { ve <- asks(evalExpr(e, p, env, _)) 
              val c = ve match {case VClassName(c) => c; case _ => throw NotAClassName(ve)}
              ves <- asks(st => es map (evalExpr(_, p, env, st)))
						  val self = selfCId(env)
						  sd <- getNewSeed(self)
              fa  <- mkObj(c, p, Some(self), sd, ves)
        } lhs match { 
          case None => pass
          case Some(Dot(e,x)) => 
            for { id <- asks(evalExpr(e, p, env, _)) map extractId
                  _ <- asks(checkAccessOk(id, env, _))
            } setObjectFieldM(id, x, VObjId(Some(fa))) 
          case Some(_) => throw BadLhs()
        }
      case Elim(e) =>
        for { id <- asks(evalExpr(e, p, env, _)) map extractId
              _ <- asks(checkAccessOk(id, env, _))
        } vanish(id)
      case Move(Dot(o1,x), o2) => 
        for { o1Id <- asks(evalExpr(o1, p, env, _)) map extractId
              _ <- asks(checkAccessOk(o1Id, env, _))
              xId  <- asks(getObjectField(o1Id, x, _)) map extractId
              _ <- asks(checkIsChildOf(xId, o1Id, _))
              o2Id <- asks(evalExpr(o2, p, env, _)) map extractId
              _ <- asks(checkAccessOk(o2Id, env, _))
        } reparent(List(xId), o2Id)
      case Move(_,_) =>
        throw BadMove()
    }

  def evalContinuousAction(a:ContinuousAction, env:Env, p:Prog) : Eval[Unit] = 
    a match {
      case EquationT(Dot(e,x),t) =>
        /* Schedule a continuous assignment of vt to x */
        for { a <- asks(evalExpr(e, p, env, _)) map extractId
              vt <- asks(evalExpr(t, p, env, _))
        } equation(a, x, vt)
      case EquationI(Dot(e,x),t) =>
        /* Schedule a continuous assignment of the Euler approximation of x (at time+dt) to x */
        for { dt <- asks(getTimeStep)
              a <- asks(evalExpr(e, p, env, _)) map extractId
              vt <- asks(evalExpr(t, p, env, _))
              lhs <- asks(getObjectField(a, x, _))
        } equation(a, x, lhs match {
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
        throw ShouldNeverHappen() // FIXME: enforce that with refinment types
    }
  
  def evalStep(p:Prog)(id:CId) : Eval[Unit] =
    for (cl <- asks(getCls(id,_))) {
      val as = classDef(cl, p).body
      val env = HashMap((self, VObjId(Some(id))))
      evalActions(as, env, p)
    }

  /* Outer loop: iterates f from the root to the leaves of the
     tree formed by the parent-children relation. The relation
     may be updated live */
  def iterate(f:CId => Eval[Unit], root:CId) : Eval[Unit] = {
    for { _   <- f(root)
          ids <- asks(childrenOf(root,_))
    } mapM_(iterate(f,_:CId), ids)
  }

  /* Main simulation loop */  

  def init(prog:Prog) : (Prog, Store) = {
    val cprog = CleanParameters.run(prog, CStoreInterpreterType)
    val sprog = Simplifier.run(cprog)
    val mprog = Prog(magicClass :: sprog.defs)
    val (sd1,sd2) = Random.split(Random.mkGen(0))
    val (id,_,_,_,_,st1) = 
      mkObj(cmain, mprog, None, sd1, List(VObjId(Some(CId(0)))), 1)(initStoreRef)
    val st2 = changeParent(CId(0), id, st1)
    val st3 = changeSeed(CId(0), sd2, st2)
    (mprog, st3)
  }

  override def expose_externally(store: Store) : Store = {
    if (Main.serverMode) {
      val json1 = JSon.toJSON(store).toString
      val store2 = JSon.fromJSON(Main.send_recv(json1))
      store2
    } else {
      store
    }
  }

  def step(p:Prog, st:Store) : Option[Store] =
    if (getTime(st) > getEndTime(st)) None
    else Some(
      { val (_,ids,rps,ass,eqs,st1) = iterate(evalStep(p), mainId(st))(st)
        getResultType(st) match {
          case Discrete | Continuous =>
            if (st == st1 && ids.isEmpty && rps.isEmpty && ass.isEmpty) 
              setResultType(FixedPoint, st1) // Reached discrete fixpoint
            else {
              checkDuplicateAssingments(ass, "discrete")
              val stA = mapM_(applyAssingment, ass.toList) ~> st1
              def applyReparenting(pair:(CId, CId)) = changeParentM(pair._1, pair._2) 
              val stR = mapM_(applyReparenting, rps.toList) ~> stA
              val st3 = stR -- ids
              setResultType(Discrete, st3)
            }
          case FixedPoint => // Now perform continuous assignments
            checkDuplicateAssingments(eqs, "continuous")
            val stA = mapM_(applyAssingment, eqs.toList) ~> st1
            val st2 = setResultType(Continuous, stA)
            setTime(getTime(st1) + getTimeStep(st1), st2)
        }
      }
    )
  
  /** Applies an assignment to the monad. */
  def applyAssingment(a: (CId,Name,CValue)) = setObjectFieldM(a._1, a._2, a._3)

  /** Checks for a duplicate assignment (of a specific kind) scheduled in assignments. */
  def checkDuplicateAssingments(assignments: Set[(CId, Name, CValue)], kind: String): Unit = {
    val duplicates = assignments.groupBy(a => (a._1,a._2)).filter{ case (_, l) => l.size > 1 }.keys.toList
    if (duplicates.size != 0) {
      val n = duplicates(0)._2
      sys.error("Repeated " + kind + " assignment to variable (" + n.x + "'" * n.primes + ") is not allowed.")
    }
  }
    
}
