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
package reference2015

import Eval._
import Common._
import ui.tl.Console
import util.ASTUtil.{ checkContinuousAssignmentToSimulator, checkNestedHypotheses, dots }
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

  type Store = CStore

  def repr(st:Store) = st
  def fromCStore(st:CStore, root:CId) = st
  val initStepType = Initial
  val timeStep = 0.015625
  val outputRows = "All"
  val initStore = initStoreInterpreter(initStep = initStepType, initTimeStep = timeStep, initOutputRows = outputRows, isImperative = false)
  override def visibleParameters = visibleParametersMap(initStore) + ("method" -> VLit(GStr(RungeKutta))) + ("orderOfIntegration" -> VLit(GInt(4)))

  /* Bindings, expressed in models as continuous assignments 
   * to unprimed variables, are used to look up sub-expressions
   * during evaluation. */
  type Bindings = Map[(CId,Name,List[Int]), Binding] 
  val NoBindings = Map.empty[(CId,Name,List[Int]), Binding]
  sealed trait Binding
  case object UsedBinding extends Binding
  case class UnusedBinding(e: Expr, env: Env) extends Binding
  case class CachedUnusedBinding(v: CValue) extends Binding
  def cacheBindings(b: Bindings, st: Store): Bindings = 
    b.foldLeft(b){ 
      case (res, (k, UnusedBinding(e, env))) => 
        res updated (k, CachedUnusedBinding(evalExpr(e, env, st)(res)))
      case (res, (_, _: CachedUnusedBinding)) => res
    }
  
  def evaluateIndexes(as:List[CollectedAction],st:Store)(implicit bindings:Bindings):List[CollectedAction] = {
    as.map(a => a match{
      case CollectedAction(o,Index(d,idx),rhs,env) => 
        val evaledIdx = idx.map(x => evalExpr(x,env,st)(bindings)).map(y => Lit(GInt(extractInt(y))))       
        CollectedAction(o,Index(d,evaledIdx),rhs,env)
    })
  }
  
  /* initial values */
  val emptyStore : Store = HashMap.empty
  val emptyEnv   : Env   = Env.empty

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
    
  /* discretely assign the value of r evaluated in e to a field n in object o */
  def assign(o: CId, d: Index, r: Expr, e: Env) : Eval[Unit] = 
    logAssign(o, d, r, e)

  /* continuously assign the value of r evaluated in e to a field n in object o */
  def equation(o: CId, d: Index, r: Expr, e: Env) : Eval[Unit] = 
    logEquation(o, d, r, e)

  /* continuously assign the value of r evaluated in e to a field n in object o */
  def ode(o: CId, d: Index, r: Expr, e: Env) : Eval[Unit] = 
    logODE(o, d, r, e)
  
  /* log an id as being new */
  def birth(da: Option[(CId, Name)], c: ClassName, parent: CId, sd: (Int, Int), ves: List[CValue]) : Eval[Unit] = 
    logBorn(da, c, parent, sd, ves)
  
  /* log an id as being dead */
  def kill(a:CId) : Eval[Unit] = 
    logDead(a)
        
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
    val (privVars, crInits) = {
      val (xs,ys) = cd.priv.foldLeft[(List[Name],List[InitRhs])]((Nil,Nil))(helper)
      (xs.reverse, ys.reverse)
    }
    implicit val bindings = NoBindings
    for { fid <- freshCId(prt)
          _ <- setObjectM(fid, pub) // add new object to resulting store 
          vs <- mapM[InitRhs, CValue]( // process create initializers
                  { case NewRhs(e,es) =>
                      for { ve <- asks(evalExpr(e, Env(Map(self -> VObjId(Some(fid)))), _)) 
                            val cn = ve match { case VClassName(cn) => cn; case _ => throw NotAClassName(ve) }
                            ves <- asks(st => es map (evalExpr(_, Env(Map(self -> VObjId(Some(fid)))), st)))
                  			    nsd <- getNewSeed(fid)
                  			    oid <- mkObj(cn, p, Some(fid), nsd, ves)
                      } yield VObjId(Some(oid))
                    case ExprRhs(e) =>
                      asks(evalExpr(e, Env(Map(self -> VObjId(Some(fid)))), _))
                  },
                  crInits)
          val priv = privVars zip vs 
          // new object creation may have changed the nextChild counter
          newpub <- asks(deref(fid,_))
          _ <- setObjectM(fid, newpub ++ priv)
    } yield fid
  }

  /* utility function */

  def evalToObjId(e: Expr, env: Env, st:Store)(implicit bindings: Bindings) = evalExpr(e, env, st) match {
    case VObjId(Some(id)) => checkAccessOk(id, env, st, e); id
    case v => throw NotAnObject(v).setPos(e.pos)
  }

  /* evaluate e in the scope of env 
   * for definitions p with current store st */
  def evalExpr(e:Expr, env:Env, st:Store)(implicit bindings: Bindings) : CValue = {
    def eval(env:Env, e:Expr)(implicit bindings: Bindings) : CValue = try {
	    e match {
  	    case Lit(i)         => VLit(i)
        case ExprVector(l)  => VVector (l map (eval(env,_)))
        case Var(n)         => env.get(n).getOrElse(VClassName(ClassName(n.x)))
        case Input(s,i)     => Devices.getDeviceInput(extractInt(eval(env, s)), i)
        case Index(v,i)     => evalIndexOp(eval(env, v), i.map(x => eval(env, x)))
        /* e.f */
        case Dot(o, f) =>
          val id = evalToObjId(o, env, st)
          if (f == children)
            /* In order to avoid redundancy and potential inconsistencies, 
             each object has a pointer to its parent instead of having 
             each object maintain a list of its children. This is why the 
             children list has to be computed on the fly when requested. 
             An efficient implementation wouldn't do that. */
            VList(childrenOf(id,st) map (c => VObjId(Some(c))))
          else {
            val fid = (id, f, Nil)
            bindings.get(fid) match {
              case None =>
                if (id == selfCId(env))
                  env.get(f).getOrElse(getObjectField(id, f, st))
                else
                  getObjectField(id, f, st)
              case Some(CachedUnusedBinding(v)) => v
              case Some(UnusedBinding(e1, env1)) =>
                eval(env1,e1)(bindings updated (fid, UsedBinding))
              case Some(UsedBinding) =>
                throw new AlgebraicLoop(ObjField(id, getCls(id,st).x, f))
            }  
          }
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
    } catch {
      case err: AlgebraicLoop => throw e match {
        case d @ Dot(o, f) =>
          val id = resolveDot(d, env, st).id
          err.addToChain(ObjField(id, getCls(id, st).x, f), e.pos)
        case _ => err
      }
      case err: PositionalAcumenError => err.setPos(e.pos); throw err
    }
    eval(env,e)
  }.setPos(e.pos)

  def evalActions(as:List[Action], env:Env, p:Prog)(implicit bindings: Bindings) : Eval[Unit] =
    mapM_((a:Action) => evalAction(a, env, p), as)
  
  def evalAction(a:Action, env:Env, p:Prog)(implicit bindings: Bindings) : Eval[Unit] = {
    def VListToPattern(ls:List[Value[_]]):GPattern = 
            GPattern(ls.map(x => x match{
              case VLit(n) => n
              case VVector(nls) => VListToPattern(nls)            
            }))
    a match {
      case IfThenElse(c,a1,a2) =>
        for (VLit(GBool(b)) <- asks(evalExpr(c, env, _)))
          if (b) evalActions(a1, env, p)
          else   evalActions(a2, env, p)
      case ForEach(i,l,b) => 
        for (seq <- asks(evalExpr(l, env, _))) {
          val vs = seq match { 
            case VList(vs) => vs 
            case VVector(vs) => vs 
            case _ => throw NotACollection(seq)
          }
          mapM_((v:CValue) => evalActions(b, env+((i,v)), p), vs)
        }
      case Switch(s,cls) => s match{     
        case ExprVector(_) =>           
          for (VVector(ls) <- asks(evalExpr(s, env, _))) {
            val gp = VListToPattern(ls)
            (cls find (_.lhs == gp)) match {
              case Some(c) => evalActions(c.rhs, env, p)
              case None    => throw NoMatch(gp)
            }
          }
        case _ =>
          for (VLit(gv) <- asks(evalExpr(s, env, _))) {
            (cls find (_.lhs == gv)) match {
              case Some(c) => evalActions(c.rhs, env, p)
              case None    => throw NoMatch(gv)
            }
          }
      }
      /* Decides when a discrete assignment is in scope */  
      case Discretely(da) =>
        for (ty <- asks(getResultType))
          /* We do not consider discrete and structural actions after 
           * a fixpoint is reached, i.e. during continuous steps. */
          if (ty == Initial || ty == Discrete || ty == Continuous) 
            evalDiscreteAction(da, env, p)
          else pass
      /* Decides when a continuous assignment is in scope */
      case Continuously(ca) =>
        evalContinuousAction(ca, env, p)
      case Claim(_) =>
        pass
      case Hypothesis(s, e) =>
        logHypothesis(selfCId(env), s, e, env)
    }
  }
 
  def evalDiscreteAction(a:DiscreteAction, env:Env, p:Prog)(implicit bindings: Bindings) : Eval[Unit] =
    a match {
      case Assign(d @ Dot(e, n), rhs) =>
        for {
          id <- asks(evalToObjId(e, env, _))
          _ <- asks(checkVariableDeclared(id, n, d.pos, _))
        } assign(id, Index(d,Nil), rhs, env)
      case Assign(d @ Index(Dot(e, n),idx), rhs) =>
        for {
          id <- asks(evalToObjId(e, env, _))
          _ <- asks(checkVariableDeclared(id, n, d.pos, _))
        } {
          assign(id, Index(Dot(e,n),idx), rhs, env)}
      /* Basically, following says that variable names must be 
         fully qualified at this language level */
      case Assign(_,_) => 
        throw BadLhs()
      case Create(lhs, e, es) =>
        for { ve <- asks(evalExpr(e, env, _)) 
              val c = ve match {case VClassName(c) => c; case _ => throw NotAClassName(ve)}
              ves <- asks(st => es map (evalExpr(_, env, st)))
						  val self = selfCId(env)
						  sd <- getNewSeed(self)
        } lhs match { 
          case None =>
            birth(None, c, self, sd, ves)
          case Some(Dot(e,x)) => 
            for (id <- asks(evalToObjId(e, env, _)))
              birth(Some(id,x), c, self, sd, ves) 
          case Some(_) => throw BadLhs()
        }
      case Elim(e) =>
        for (id <- asks(evalToObjId(e, env, _)))
          kill(id)
      case Move(Dot(o1,x), o2) => 
        for { o1Id <- asks(evalToObjId(o1, env, _))
              xId  <- asks(getObjectField(o1Id, x, _)) map extractId
              _ <- asks(checkIsChildOf(xId, o1Id, _, o1))
              o2Id <- asks(evalToObjId(o2, env, _))
        } reparent(List(xId), o2Id)
      case Move(_,_) =>
        throw BadMove()
    }

  def evalContinuousAction(a:ContinuousAction, env:Env, p:Prog)(implicit bindings: Bindings) : Eval[Unit] = {
    a match {
      case EquationT(d @ Dot(e, n), rhs) =>
        for {
          id <- asks(evalToObjId(e, env, _))
          _ <- asks(checkVariableDeclared(id, n, d.pos, _))
        } equation(id, Index(d,Nil), rhs, env)
      case EquationT(d @ Index(Dot(e, n),idx), rhs) =>
        for {
          id <- asks(evalToObjId(e, env, _))
          _ <- asks(checkVariableDeclared(id, n, d.pos, _))
        }{
          equation(id, Index(Dot(e,n),idx), rhs, env)}
      case EquationI(d @ Dot(e, _), rhs) => // No need to check that lhs is declared, as EquationI:s are generated
        for {
          id <- asks(evalToObjId(e, env, _))
          resultType <- asks(getResultType)
        } if (resultType == FixedPoint) ode(id, Index(d, Nil), rhs, env)
        else pass
      case EquationI(d @ Index(Dot(e, n), idx), rhs) => // No need to check that lhs is declared, as EquationI:s are generated
        for {
          id <- asks(evalToObjId(e, env, _))
          resultType <- asks(getResultType)
        } {
          if (resultType == FixedPoint) ode(id, Index(Dot(e, n), idx), rhs, env)
          else pass
        }
      case _ =>
        throw ShouldNeverHappen() // FIXME: enforce that with refinement types
    }
  }
  
  def evalStep(p:Prog)(id:CId)(implicit bindings: Bindings) : Eval[Unit] =
    for (cl <- asks(getCls(id,_))) {
      val as = classDef(cl, p).body
      val env = Env(HashMap((self, VObjId(Some(id)))))
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

  def init(prog:Prog) : (Prog, Store, Metadata) = {
    checkNestedHypotheses(prog)
    checkContinuousAssignmentToSimulator(prog)
    val cprog = CleanParameters.run(prog, CStoreInterpreterType)
    val sprog = Simplifier.run(cprog)
    val mprog = Prog(magicClass :: sprog.defs)
    val (sd1,sd2) = Random.split(Random.mkGen(0))
    val (id,_,st1) = mkObj(cmain, mprog, None, sd1, List(VObjId(Some(CId(0)))), 1)(initStore)
    val st2 = changeParent(CId(0), id, st1)
    val st3 = changeSeed(CId(0), sd2, st2)
    val st4 = countVariables(st3)
    val hyps = st4.toList.flatMap { case (cid, co) =>
      mprog.defs.find(_.name == getCls(cid, st4)).get.body.flatMap {
        case Hypothesis(s, e) =>
          CollectedHypothesis(cid, s, e, Env(Map(self -> VObjId(Some(cid))))) :: Nil
        case _ => Nil
    }}
    val md = testHypotheses(hyps, NoMetadata, st4, 0)(NoBindings)
    (mprog, st4, md)
  }
  
  /** Updates the values of variables in xs (identified by CId and Dot.field) to the corresponding CValue. */
  def applyAssignments(xs: List[(CId, Name, CValue)]): Eval[Unit] = 
    mapM_((a: (CId, Name, CValue)) => setObjectFieldM(a._1, a._2, a._3), xs)

  /** Computes the values of variables in xs (identified by CId and Dot.field). */
  def evaluateAssignments(xs: List[CollectedAction], st: Store)(implicit bindings: Bindings): List[(CId, Name, CValue)] = {
    val cache = cacheBindings(bindings, st)
    val groupUpdates = xs.groupBy {x => (x.o,x.d.lhs)}
    
    groupUpdates.map{case ((id,dot),as) => as match {
      /* Update (id,dot) with new value v */
      case CollectedAction(o, Index(d: Dot, Nil), rhs, env)::Nil =>
        val ResolvedDot(rId, _, rN) = resolveDot(d, env, st)
        (rId, rN, evalExpr(rhs, env, st)(cache))
      /* Congregate multiple index assignments to (id,dot) into one assignment and update */
      case multipleIndexUpdates => 
        val indexes = multipleIndexUpdates.map(x => x.d.idx.map(y => y match{case Lit(GInt(i)) => i}))
        val vts = multipleIndexUpdates.map(x => evalExpr(x.rhs, x.env, st)(cache))
        val lhs = getObjectField(id, dot.field, st)
        val v = lhs match {
          case VVector(u) =>
            updateMultipleEntries(VVector(u), indexes, vts)
        }
        val ResolvedDot(rId, _, rN) = resolveDot(dot, multipleIndexUpdates.head.env, st)
        (rId, rN, v)
    }}.toList
  }
    
  /** Updates the values of variables in xs (identified by CId and Dot.field) to the corresponding CValue. */
  def applyCollectedAssignments(xs: List[CollectedAction], st: Store)(implicit bindings: Bindings): Store = 
    applyAssignments(evaluateAssignments(xs, st)) ~> st

  /** For each r in rps, makes r._1 into a child of r._2.  */
  def applyReparentings(rps: List[(CId,CId)]): Eval[Unit] =
    mapM_((pair:(CId, CId)) => changeParentM(pair._1, pair._2), rps)
    
  /** Adds new objects to the store and applies any corresponding discrete assignments. */
  def applyCollectedCreates(dcs: List[CollectedCreate], p: Prog): Eval[Unit] =
    mapM_({ case CollectedCreate(da, c, parent, sd, ves) =>
      for (fa <- mkObj(c, p, Some(parent), sd, ves))
        da match {
          case None          => pass
          case Some((id, x)) => setObjectFieldM(id, x, VObjId(Some(fa)))
        }}: CollectedCreate => Eval[Unit], dcs)
  
  /** Remove dead objects and compute list of orphans, i.e. reparentings required 
   *  to transfer the parenthood of dead objects' children to their parents */
  def applyTerminations(dead: List[CId], st: Store): (Store, List[(CId,CId)]) = {
    val (_, cs, st1) = mapM_((a: CId) => 
      for { Some(p) <- asks(getParent(a,_))
            cs <- asks(childrenOf(a,_))
          } reparent(cs,p), dead)(st)
    (st1 -- dead, cs.reps)
  }

  /**
   * Reference interpreter state machine:
   * 
   * 0. Initial step, where the contents of the initially section
   *    are reported.
   * 1. Discrete steps, where discrete assignments and structural
   *    actions are evaluated, are taken until a FixedPoint.
   * 2. A Continuous step is taken, where the ODEs defined by 
   *    continuous assignments to primed variables are solved. 
   * 
   * After each sub-step of step 1 and step 2, all continuous 
   * assignments (EquationT) are applied to the store with the
   * exception that, during Discrete steps, discrete assignments 
   * are given precedence over clashing continuous assignments.
   * Simulation starts with step 0 and then alternates steps 1 
   * and 2 until a) the end time is reached and b) step 1 has 
   * reached a FixedPoint.
   */
  def step(p:Prog, st:Store, md: Metadata) : StepRes = {
    val resultType = getResultType(st)
    /* We are done when the simulation time is over the given end 
     * time and we have reached a discrete fixed point */
    if (resultType == FixedPoint && getTime(st) >= getEndTime(st))
      Done(md, getEndTime(st))
    else 
      { val (_, Changeset(born, dead, rps, das1, eqs1, odes1, hyps), _) = iterate(evalStep(p)(_)(NoBindings), mainId(st))(st)
        /* Create objects and apply any corresponding discrete assignments */
        val st1 = applyCollectedCreates(born, p) ~> st
        // All indexes are evaluated to List[Lit(GInt)]        
        val das = evaluateIndexes(das1, st1)(NoBindings)
        val eqs = evaluateIndexes(eqs1, st1)(NoBindings)
        val odes = evaluateIndexes(odes1, st1)(NoBindings)
        implicit val bindings = eqs.map{ e => val rd = resolveDot(e.d.lhs, e.env, st1)
          (rd.id, rd.field,e.d.idx.map{x => x match{case Lit(GInt(i)) => i}}) -> UnusedBinding(e.rhs, e.env)}.toMap       
      
        def resolveDots(s: List[CollectedAction]): List[(ResolvedDot,List[Int])] =
          s.map(da => (resolveDot(da.d.lhs, da.env, st1),da.d.idx match{
            case Nil => Nil
            case ls => ls.map(x => x match{
              case Lit(GInt(i)) => i
            })
          }))
        val res = resultType match {
          case Initial | Discrete | Continuous => // Do discrete step or conclude discrete fixpoint
            checkDuplicateAssingments(resolveDots(das), DuplicateDiscreteAssingment)
            checkDuplicateAssingments(resolveDots(eqs), DuplicateContinuousAssingment)
            /* Evaluate discrete assignments */
            val dasValues = evaluateAssignments(das, st1)
            /* Evaluate continuous assignments that do not clash with discrete assignments */
            val nonClashingEqs = eqs.filterNot (e => dasValues.exists { case (id, n, _) =>  
              id == resolveDot(e.d.lhs, e.env, st1).id && n == e.d.lhs.field })
            val nonClashingEqsValues = evaluateAssignments(nonClashingEqs, st1)(bindings ++
              /* Give discrete assignments precedence by replacing clashing bindings */
              dasValues.map { case (id, n, v) => (id, n, Nil) -> CachedUnusedBinding(v) })
            /* Find (non-ODE) assignments that modify the store */
            val nonIdentityAs = (dasValues ++ nonClashingEqsValues).filterNot{ case (id, n, v) => 
              threeDField(n.x) || v == getObjectField(id, n, st1) }
            /* If the discrete, structural and non-ODE continuous actions do not modify the store, conclude discrete fixpoint */
            if (nonIdentityAs.isEmpty && born.isEmpty && dead.isEmpty && rps.isEmpty)
              setResultType(FixedPoint, st1)
            else {
              /* Apply discrete and non-clashing continuous assignment values */
              val st2 = applyAssignments(nonClashingEqsValues ++ dasValues) ~> st1
              /* Apply terminations to get store without dead objects and list of orphans */
              val (st3, orphans) = applyTerminations(dead, st2)
              /* Apply reparentings */
              val st4 = applyReparentings(rps ++ orphans) ~> st3
              setResultType(Discrete, st4)
            }
          case FixedPoint => // Do continuous step
            val eqsIds = resolveDots(eqs)
            val odesIds = resolveDots(odes)
            checkDuplicateAssingments(eqsIds ++ odesIds, DuplicateContinuousAssingment)
            checkContinuousDynamicsAlwaysDefined(p, eqsIds, st1)
            /* After reaching a discrete fixpoint, integrate */
            val stODES = solveIVP(odes, p, st1)
            val stT = setTime(getTime(stODES) + getTimeStep(stODES), stODES)
            /* Ensure that the resulting store is consistent w.r.t. continuous assignments */
            val stEQS = applyCollectedAssignments(eqs, stT)
            setResultType(Continuous, stEQS)
        }
        // Hypotheses check only when the result is not a FixedPoint
        lazy val md1 = testHypotheses(hyps, md, res, getTime(st))(NoBindings) // No bindings needed, res is consistent 
        if (getResultType(res) != FixedPoint)
          Data(countVariables(res), md1)
        else
          Data(countVariables(res), md)
      }
    }
  
  /** Summarize result of evaluating the hypotheses of all objects. */
  def testHypotheses(hyps: List[CollectedHypothesis], old: Metadata, st: Store, timeBefore: Double)(implicit bindings: Bindings): Metadata =
    old combine (if (hyps isEmpty) NoMetadata else SomeMetadata(hyps.map {
      case CollectedHypothesis(o, hn, h, env) =>
        (o, getCls(o, st), hn) -> computeHypothesisOutcomes( 
          evalExpr(h, env, st), getTime(st), getResultType(st), 
          dots(h).toSet[Dot].map(d => d -> (evalExpr(d, env, st))))
    }.toMap, timeBefore, getTime(st), false, None))

  /**
   * Solve ODE-IVP defined by odes parameter tuple, which consists of:
   *  - CId:  The object in which the ODE was encountered.
   *  - Dot:  The LHS of the ODE.
   *  - Expr: The RHS of the ODE.
   *  - Env:  Initial conditions of the IVP.
   * The time segment is derived from time step in store st. 
   */
  def solveIVP(odes: List[CollectedAction], p: Prog, st: Store)(implicit bindings: Bindings): Store = {
    implicit val field = FieldImpl(odes, p)
    implicit val doubleIsReal = AD.DoubleIsReal
    new Solver(getInSimulator(Name("method", 0),st), xs = st, h = getTimeStep(st)) {
      override def knownSolvers = super.knownSolvers :+ EulerCromer
      override def solveIfKnown(name: String) = super.solveIfKnown(name) orElse (name match {
        case EulerCromer => Some(solveIVPEulerCromer(xs, h))
        case _           => None
      })
    }.solve
  }
  
  /** Representation of a set of ODEs. */
  case class FieldImpl(odes: List[CollectedAction], p: Prog)(implicit bindings: Bindings) extends Field[Store,CId] {
    /** Evaluate the field (the RHS of each equation in ODEs) in s. */
    override def apply(s: Store): Store = applyCollectedAssignments(odes, s)
    /** NOTE: Assumes that the de-sugarer has reduced all higher-order ODEs.  */
    override def variables(s: Store): List[(CId, Name)] =
      odes.map { da =>
        val ResolvedDot(dId, _, dN) = resolveDot(da.d.lhs, da.env, s)
        (dId, dN) 
      }
    override def map(em: Expr => Expr) = 
      FieldImpl(odes.map(ode => ode.copy(rhs = em(ode.rhs))))
  }

  /**
   * Embedded DSL for expressing integrators.
   * NOTE: Operators affect only field.variables and field.derivatives.
   */
  case class RichStoreImpl(s: Store)(implicit field: FieldImpl) extends RichStore[Store,CId] {
    override def +++(that: Store): Store = op("+", (cid, n) => getObjectField(cid, n, that))
    override def ***(that: Double): Store = op("*", (_, _) => VLit(GDouble(that)))
    /** Combine this (s) and that Store using operator. */
    def op(operator: String, that: (CId, Name) => Value[CId]): Store =
      applyAssignments(field.variables(s).map {
        case (o, n) => (o, n, evalOp(operator, List(s(o, n), that(o, n))))
      }) ~> s
    override def map(m: CValue => CValue) = 
      s.mapValues(_.map{ 
        case nv@(Name(n,_),_) if interpreters.Common.specialFields.contains(n) => nv   
        case (n,v) => (n, m(v)) 
      })
    override def apply(id: CId, n: Name): CValue = getObjectField(id, n, s)
    override def updated(id: CId, n: Name, v: CValue): Store = setObjectField(id, n, v, s)
    override def getInSimulator(variable: String) = Canonical.getInSimulator(Name(variable, 0), s)
  }
  implicit def liftStore(s: Store)(implicit field: FieldImpl): RichStoreImpl = RichStoreImpl(s)
  
  /**
   * Euler-Cromer integration. 
   * 
   * A solver that produces stable solutions for some systems where 
   * Forward Euler solutions become unstable. This is accomplished by 
   * using the next value of the higher derivative when computing the 
   * next Euler estimate (instead of using the previous value, as is 
   * the case with Forward Euler).  
   * 
   * NOTE: Some equational properties of Acumen programs may not hold 
   *       when using this integration method.
   */
  def solveIVPEulerCromer(st: Store, h: Double)(implicit f: FieldImpl, bindings: Bindings): Store = {
    // Ensure that derivatives are being integrated in the correct order
    val sortedODEs = f.odes
      .groupBy{ case CollectedAction(o, Index(Dot(_, n),idx), r, e) => (o, n.x) }
      .mapValues(_.sortBy { case CollectedAction(_, Index(Dot(_, n),idx), _, _) => n.primes }).values.flatten
    val solutions = sortedODEs.foldRight(Map.empty[(CId, Name), CValue]) {
      case (CollectedAction(o, d@Index(Dot(_, n),idx), r, e), updatedEnvs) =>
        val updatedEnv = e ++ (for (((obj, name), v) <- updatedEnvs if obj == o) yield (name -> v))
        val vt = evalExpr(r, updatedEnv, st)
        val lhs = getObjectField(o, n, st)
        val v = lhs match {
          case VLit(d) =>
            VLit(GDouble(extractDouble(d) + extractDouble(vt) * h))
          case VVector(u) => idx match{
            case Nil => 
              val us = extractDoubles(u)
              val ts = extractDoubles(vt)
              VVector((us, ts).zipped map ((a, b) => VLit(GDouble(a + b * h))))              
          }         
          case _ =>
            throw BadLhs()
        }
        val ResolvedDot(rId,_,rN) = resolveDot(d.lhs, e, st)
        updatedEnvs + ((rId, rN) -> v)
    }.map { case ((o, d), v) => (o, d, v) }.toList
    applyAssignments(solutions) ~> st
  }
  
}
