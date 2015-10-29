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
package reference2014

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
  val initStepType = Discrete
  val timeStep = 0.01
  val outputRows = "WhenChanged"
  val initStore = initStoreInterpreter(initStep = initStepType, initTimeStep = timeStep, initOutputRows = outputRows, isImperative = false)
  override def visibleParameters = visibleParametersMap(initStore) + ("method" -> VLit(GStr(RungeKutta))) + ("orderOfIntegration" -> VLit(GInt(4)))

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
  def assign(o: CId, d: Dot, r: Expr, e: Env) : Eval[Unit] = logAssign(o, d, r, e)

  /* continuously assign the value of r evaluated in e to a field n in object o */
  def equation(o: CId, d: Dot, r: Expr, e: Env) : Eval[Unit] = logEquation(o, d, r, e)

  /* continuously assign the value of r evaluated in e to a field n in object o */
  def ode(o: CId, d: Dot, r: Expr, e: Env) : Eval[Unit] = logODE(o, d, r, e)
  
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
                      for { ve <- asks(evalExpr(e, Env(self -> VObjId(Some(fid))), _)) 
                            val cn = ve match {case VClassName(cn) => cn; case _ => throw NotAClassName(ve)}
                            ves <- asks(st => es map (
                            evalExpr(_, Env(self -> VObjId(Some(fid))), st)))
			    nsd <- getNewSeed(fid)
			    oid <- mkObj(cn, p, Some(fid), nsd, ves)
                      } yield VObjId(Some(oid))
                    case ExprRhs(e) =>
                      asks(evalExpr(e, Env(self -> VObjId(Some(fid))), _))
                  },
                  ctrs)
          val priv = privVars zip vs 
          // new object creation may have changed the nextChild counter
          newpub <- asks(deref(fid,_))
          _ <- setObjectM(fid, newpub ++ priv)
    } yield fid
  }

  /* utility function */

  def evalToObjId(e: Expr, env: Env, st:Store) = evalExpr(e, env, st) match {
    case VObjId(Some(id)) => checkAccessOk(id, env, st, e); id
    case v => throw NotAnObject(v).setPos(e.pos)
  }

  /* evaluate e in the scope of env 
   * for definitions p with current store st */
  def evalExpr(e:Expr, env:Env, st:Store) : CValue = {
    def eval(env:Env, e:Expr) : CValue = try {
	    e match {
  	    case Lit(i)         => VLit(i)
        case ExprVector(l)  => VVector (l map (eval(env,_)))
        case Var(n)         => env.get(n).getOrElse(VClassName(ClassName(n.x)))
        case Input(s,i)     => Devices.getDeviceInput(extractInt(eval(env, s)), i)
        case Index(v,i)     => evalIndexOp(eval(env, v), i.map(x => eval(env, x)))
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
      case err: PositionalAcumenError => err.setPos(e.pos); throw err
    }
    eval(env,e)
  }.setPos(e.pos)

  def evalActions(as:List[Action], env:Env, p:Prog) : Eval[Unit] =
    mapM_((a:Action) => evalAction(a, env, p), as)
  
  def evalAction(a:Action, env:Env, p:Prog) : Eval[Unit] = {
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
        
        
      case Discretely(da) =>
        for (ty <- asks(getResultType))
          if (ty == FixedPoint) pass
          else evalDiscreteAction(da, env, p)
      case Continuously(ca) =>
        for (ty <- asks(getResultType))
          if (ty != FixedPoint) pass
          else evalContinuousAction(ca, env, p) 
      case Claim(_) =>
        pass
      case Hypothesis(s, e) =>
        logHypothesis(selfCId(env), s, e, env)
    }
  }
 
  def evalDiscreteAction(a:DiscreteAction, env:Env, p:Prog) : Eval[Unit] =
    a match {
      case Assign(d@Dot(e,n),rhs) => 
        for { id <- asks(evalToObjId(e, env, _)) 
              _  <- asks(checkVariableDeclared(id, n, d.pos, _))
            } assign(id, d, rhs, env)
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
              fa  <- mkObj(c, p, Some(self), sd, ves)
        } lhs match { 
          case None => pass
          case Some(Dot(e,x)) => 
            for (id <- asks(evalToObjId(e, env, _)))
              setObjectFieldM(id, x, VObjId(Some(fa))) 
          case Some(_) => throw BadLhs()
        }
      case Elim(e) =>
        for (id <- asks(evalToObjId(e, env, _)))
          vanish(id)
      case Move(Dot(o1,x), o2) => 
        for { o1Id <- asks(evalToObjId(o1, env, _))
              xId  <- asks(getObjectField(o1Id, x, _)) map extractId
              _ <- asks(checkIsChildOf(xId, o1Id, _, o1))
              o2Id <- asks(evalToObjId(o2, env, _))
        } reparent(List(xId), o2Id)
      case Move(_,_) =>
        throw BadMove()
    }

  def evalContinuousAction(a:ContinuousAction, env:Env, p:Prog) : Eval[Unit] = 
    a match {
      case EquationT(d@Dot(e,n),rhs) =>
        for { id <- asks(evalToObjId(e, env, _)) 
              _  <- asks(checkVariableDeclared(id, n, d.pos, _))
            } equation(id, d, rhs, env)
      case EquationI(d@Dot(e,_),rhs) => // No need to check that lhs is declared, as EquationI:s are generated  
        for { id <- asks(evalToObjId(e, env, _)) } ode(id, d, rhs, env)
      case _ =>
        throw ShouldNeverHappen() // FIXME: enforce that with refinement types
    }
  
  def evalStep(p:Prog)(id:CId) : Eval[Unit] =
    for (cl <- asks(getCls(id,_))) {
      val as = classDef(cl, p).body
      val env = Env((self, VObjId(Some(id))))
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
    val (id,_,st1) = 
      mkObj(cmain, mprog, None, sd1, List(VObjId(Some(CId(0)))), 1)(initStore)
    val st2 = changeParent(CId(0), id, st1)
    val st3 = changeSeed(CId(0), sd2, st2)
    val st4 = countVariables(st3)
    (mprog, st4, NoMetadata)
  }

  /** Updates the values of variables in xs (identified by CId and Dot.field) to the corresponding CValue. */
  def applyAssignments(xs: List[(CId, Name, CValue)]): Eval[Unit] = 
    mapM_((a: (CId, Name, CValue)) => setObjectFieldM(a._1, a._2, a._3), xs)

  /** Computes the values of variables in xs (identified by CId and Dot.field). */
  def evaluateAssignments(xs: List[(CId, Dot, Expr, Env)], st: Store): List[(CId, Name, CValue)] = 
    xs.map{ case (id, d, rhs, env) => 
      val ResolvedDot(rId, _, rN) = resolveDot(d, env, st)
      (rId, rN, evalExpr(rhs, env, st))
    }
    
  /** Updates the values of variables in xs (identified by CId and Dot.field) to the corresponding CValue. */
  def applyDelayedAssignments(xs: List[(CId, Dot, Expr, Env)], st: Store): Eval[Unit] = 
    applyAssignments(evaluateAssignments(xs,st))
  
  def step(p:Prog, st:Store, md: Metadata) : StepRes =
    if (getTime(st) >= getEndTime(st)){
      Done(md, getEndTime(st))
    } 
    else 
      { val (_, Changeset(ids, rps, das, eqs, odes, hyps), st1) = iterate(evalStep(p), mainId(st))(st)
        val md1 = testHypotheses(hyps, md, st)
        def resolveDots(s: List[(CId,Dot,Expr,Env)]): List[ResolvedDot] =
          s.map{ case (o, d, _, env) => resolveDot(d, env, st1) }
        val res = getResultType(st) match {
          case Initial | Discrete | Continuous => // Either conclude fixpoint is reached or do discrete step
            checkDuplicateAssingments2014(resolveDots(das), DuplicateDiscreteAssingment)
            val dasValues = evaluateAssignments(das, st1)
            val nonIdentityDas = dasValues.filterNot{ case (id,n,v) => v == getObjectField(id, n, st1) }
            if (st == st1 && ids.isEmpty && rps.isEmpty && nonIdentityDas.isEmpty) 
              setResultType(FixedPoint, st1)
            else {
              val stA = applyAssignments(dasValues) ~> st1
              def repHelper(pair:(CId, CId)) = changeParentM(pair._1, pair._2) 
              val stR = mapM_(repHelper, rps) ~> stA
              val st3 = stR -- ids
              setResultType(Discrete, st3)
            }
          case FixedPoint => // Do continuous step
            val eqsIds = resolveDots(eqs)
            val odesIds = resolveDots(odes)
            checkDuplicateAssingments2014(eqsIds ++ odesIds, DuplicateContinuousAssingment)
            checkContinuousDynamicsAlwaysDefined2014(p, eqsIds, st1)
            val stODE = solveIVP(odes, p, st1)
            val stE = applyDelayedAssignments(eqs, st1) ~> stODE
            val st2 = setResultType(Continuous, stE)
            setTime(getTime(st2) + getTimeStep(st2), st2)
        }
        Data(countVariables(res), md1)
      }

  /** Summarize result of evaluating the hypotheses of all objects. */
  def testHypotheses(hyps: List[(CId, Option[String], Expr, Env)], old: Metadata, st: Store): Metadata =
    old combine (if (hyps isEmpty) NoMetadata else SomeMetadata(hyps.map {
      case (o, hn, h, env) =>
        (o, getCls(o, st), hn) -> computeHypothesisOutcomes( 
          evalExpr(h, env, st), getTime(st), getResultType(st), 
          dots(h).toSet[Dot].map(d => d -> (evalExpr(d, env, st))))
    }.toMap, getTime(st), getTime(st) + getTimeStep(st), false, None))

  /**
   * Solve ODE-IVP defined by odes parameter tuple, which consists of:
   *  - CId:  The object in which the ODE was encountered.
   *  - Dot:  The LHS of the ODE.
   *  - Expr: The RHS of the ODE.
   *  - Env:  Initial conditions of the IVP.
   * The time segment is derived from time step in store st. 
   */
  def solveIVP(odes: List[(CId, Dot, Expr, Env)], p: Prog, st: Store): Store = {
    implicit val field = FieldImpl(odes, p)
    new Solver[CId,CStore,Double](getInSimulator(Name("method", 0),st), xs = st, h = getTimeStep(st)){
      // add the EulerCromer solver
      override def knownSolvers = super.knownSolvers :+ EulerCromer
      override def solveIfKnown(name: String) = super.solveIfKnown(name) orElse (name match {
        case EulerCromer => Some(solveIVPEulerCromer(xs, h))
        case _           => None
      })
    }.solve
  }
  
  /** Representation of a set of ODEs. */
  case class FieldImpl(odes: List[(CId, Dot, Expr, Env)], p: Prog) extends Field[Store,CId] {
    /** Evaluate the field (the RHS of each equation in ODEs) in s. */
    override def apply(s: Store): Store = applyDelayedAssignments(odes, s) ~> s
    /** 
     * Returns the set of variables affected by the field.
     * These are the LHSs of each ODE and the corresponding unprimed variables.
     * NOTE: Assumes that the de-sugarer has reduced all higher-order ODEs.  
     */
    override def variables(s: Store): List[(CId, Name)] =
      odes.flatMap { case (o, d, _, env) => 
        val ResolvedDot(dId, _, dN) = resolveDot(d, env, s)  
        List((dId, dN), (dId, Name(dN.x, 0)))
      }
    override def map(m: Expr => Expr) =
      copy(odes = odes.map { case (id, d, e, env) => (id, d, m(e), env) })
  }

  /**
   * Embedded DSL for expressing integrators.
   * NOTE: Operators affect only field.variables.
   */
  case class RichStoreImpl(s: Store)(implicit field: FieldImpl) extends RichStore[Store,CId] {
    override def +++(that: Store): Store = op("+", (cid, n) => getObjectField(cid, n, that))
    override def haramardProduct(that: Store): Store = op("*", (cid, n) => getObjectField(cid, n, that))
    override def ***(that: Double): Store = op("*", (_, _) => VLit(GDouble(that)))
    override def foldVariables(names: List[(CId, Name)], f: (Value[CId],Value[CId]) => Value[CId]): Value[CId] = ???
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
  def solveIVPEulerCromer(st: Store, h: Double)(implicit f: FieldImpl): Store = {
    // Ensure that derivatives are being integrated in the correct order
    val sortedODEs = f.odes
      .groupBy{ case (o, Dot(_, n), r, e) => (o, n.x) }
      .mapValues(_.sortBy { case (_, Dot(_, n), _, _) => n.primes }).values.flatten
    val solutions = sortedODEs.foldRight(Map.empty[(CId, Name), CValue]) {
      case ((o, d@Dot(_, n), r, e), updatedEnvs) =>
        val updatedEnv = e ++ (for (((obj, name), v) <- updatedEnvs if obj == o) yield (name -> v))
        val vt = evalExpr(r, updatedEnv, st)
        val lhs = getObjectField(o, n, st)
        val v = lhs match {
          case VLit(d) =>
            VLit(GDouble(extractDouble(d) + extractDouble(vt) * h))
          case VVector(u) =>
            val us = extractDoubles(u)
            val ts = extractDoubles(vt)
            VVector((us, ts).zipped map ((a, b) => VLit(GDouble(a + b * h))))
          case _ =>
            throw BadLhs()
        }
        val ResolvedDot(rId,_,rN) = resolveDot(d, e, st)
        updatedEnvs + ((rId, rN) -> v)
    }.map { case ((o, d), v) => (o, d, v) }.toList
    applyAssignments(solutions) ~> st
  }
  
}
