package acumen
package interpreters
package compiler2015

import scala.collection.immutable.{
  HashMap, TreeMap 
}

import interpreters.Common._
import Desugarer._
import Errors._
import Pretty._
import reference2015.Interpreter.{
  mkObj, NoBindings
}
import util.ASTUtil.{
  checkContinuousAssignmentToSimulator, checkNestedHypotheses, 
  dots, reduce, substitute
}
import util.Canonical._
import util.Conversions._
import util.Random
import Changeset._

import IR._

/** Used to represent the statements that are active at a given point during the simulation. */
case class Changeset
  ( das:   List[CollectedAction]     = Nil       /* discrete assignments */
  , eqs:   List[CollectedAction]     = Nil       /* continuous assignments / equations */
  , odes:  List[CollectedAction]     = Nil       /* ode assignments / differential equations */
  , hyps:  List[CollectedHypothesis] = Nil       /* hypotheses */
  , guard: Path                      = Set.empty /* Path condition */ 
  ) {
  def || (that: Changeset) =
    Changeset(das ++ that.das, eqs ++ that.eqs, odes ++ that.odes, hyps ++ that.hyps, guard ++ that.guard)
  def isEmpty: Boolean =
    das.isEmpty && eqs.isEmpty && odes.isEmpty && hyps.isEmpty && guard.isEmpty
}
object Changeset {
  def logAssign(path: Path, o: CId, a: Action, env: Env): List[Changeset] =
    List(Changeset(das = List(CollectedAction(path,o,a,env)), guard = path))
  def logEquation(path: Path, o: CId, a: Action, env: Env): List[Changeset] =
    List(Changeset(eqs = List(CollectedAction(path,o,a,env)), guard = path))
  def logODE(path: Path, o: CId, a: Action, env: Env): List[Changeset] =
    List(Changeset(odes = List(CollectedAction(path,o,a,env)), guard = path))
  def logHypothesis(o: CId, s: Option[String], h: Expr, env: Env): List[Changeset] =
    List(Changeset(hyps = List(CollectedHypothesis(o,s,h,env))))
  lazy val empty = Changeset()
}

case class CollectedCreate(da: Option[(CId, Name)], c: ClassName, parent: CId, sd: (Int, Int), ves: List[CValue])
case class CollectedAction(path: Path, selfCId: CId, a: Action, env: Env) {
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

/** Intermediate representation of an Acumen model (Prog), intended 
 *  to simplify specific compiler implementations, by performing
 *  common tasks in the transformation from Prog to IR.
 */
case class IR
  ( modes: Map[Path, Changeset]
  , continuousState: TreeMap[(CId, Name), Double]
  , discreteState: TreeMap[(CId, Name), Int]
  , stringIndices: Map[Int, String] 
  , stringValues: Map[String, Int] // indices of string-valued discrete variables
  , initialCStore: CStore // This was used to find the root object
  )

object IR {
  
  /** Representation of a set of guards. When used to generate 
   *  code, the elements will be combined with "&&". */
  type Path = Set[Expr]
  
  type Store = CStore
  
  // FIXME Import these from reference2015 interpreter
  val initStepType = Initial
  val timeStep = 0.015625
  val outputRows = "All"
  val initStore = initStoreInterpreter(initStep = initStepType, initTimeStep = timeStep, initOutputRows = outputRows, isImperative = false)

  /** Convert a Prog p into an IR. This is done by first computing 
   *  an initial store (using largely the same code as the reference
   *  interpreter) and then flattening nested conditionals in p into 
   *  a Map[Path, Changeset], where Path is a path condition, and
   *  the Changeset is a basic block of constraints, guarded by this
   *  path condition. 
   */
  def apply(p: Prog, fileName: String): IR = {
    val mprog = preProcess(p)
    val (mainCId, initSt) = makeInitialStore(mprog)
    val (cState, dState, stringIdxs) = partitionStore(initSt)
    val modes = flatten(mprog, initSt, mainCId)
    val stringValues = reduce(p, _ match {
      case Lit(GStr(s)) => s :: Nil
      case _ => Nil
    }).distinct.zipWithIndex.toMap
    IR(modes, cState, dState, stringIdxs, stringValues, initSt)
  }
  
  /** Apply source-to-source transformations to AST,
   *  apply static checks and supplement with model
   *  definition for Simulator. */
  def preProcess(p: Prog): Prog = {
    val des = new Desugarer(odeTransformMode = LocalInline).desugar(p)
    checkNestedHypotheses(des)
    checkContinuousAssignmentToSimulator(des)
    val cprog = CleanParameters.run(des, CStoreInterpreterType)
    val sprog = Simplifier.run(cprog)
    Prog(magicClass :: sprog.defs)
  }
  
  /** Return initial store, derived from p, as well
   *  as the CId of the main object. */
  def makeInitialStore(p: Prog): (CId, CStore) = {
    val (sd1,sd2) = Random.split(Random.mkGen(0))
    val (mainCId,_,is) = mkObj(
     cmain, p, None, sd1, List(VObjId(Some(CId(0)))), 1, evalExpr)(initStore)
    (mainCId, is)
  }
  
  /** Partition st into the Maps that represent it in the IR */
  def partitionStore(st: CStore): 
    ( TreeMap[(CId, Name), Double] // continuous state
    , TreeMap[(CId, Name), Int]    // discrete state
    , TreeMap[Int, String]         // indices of string-valued discrete variables           
    ) = {
    val simulatorId = magicId(st)
    val nonStateParameters = Common.simulatorFields ++ Common.specialFields
    // Handle Scala 2.11 type system flakiness
    implicit val CIdNameOrdering: Ordering[(CId, Name)] = new Ordering[(CId, Name)] {
      import Name.nameOrdering
      import Ordered.orderingToOrdered
      def compare(l: (CId, Name), r: (CId, Name)) = (l, r) match {
        case ((lid, ln), (rid, rn)) =>
          val idOrder = lid compare rid
          if (idOrder == 0) (ln compare rn) else idOrder
      }
    }
    // Partition st
    st.flatMap{ case (cid, co) => co.flatMap{ case (n, v) => Set((cid, n) -> v) } }
       .foldLeft(( TreeMap.empty[(CId, Name), Double]
                 , TreeMap.empty[(CId, Name), Int]
                 , TreeMap.empty[Int, String]           
                )){
      case (tmp, ((cid,Name(n,_)), _)) if nonStateParameters contains n =>
        tmp
      case ((ctmp, dtmp, stmp), (vid, VLit(GDouble(d)))) =>
        (ctmp + (vid -> d), dtmp, stmp)
      case ((ctmp, dtmp, stmp), (vid, VLit(GStr(s)))) =>
        val i = dtmp.size
        (ctmp, dtmp + (vid -> i), stmp + (i -> s))
      case ((ctmp, dtmp, stmp), (vid@(cid,n), VLit(GInt(i)))) =>
        if (cid == simulatorId)
          (ctmp, dtmp + (vid -> i), stmp)
        else          
          (ctmp + (vid -> i), dtmp, stmp)
      case (tmp, (vid, v)) =>
        println(s"DISCARDING: $vid = ${Pretty pprint v}")
        tmp
    }
  }
  
  /** Traverses p to collects all constraints, along with their
   *  path condition. Then associates all constraints with equal
   *  path condition in the resulting Map. */
  def flatten(p: Prog, st: CStore, mainCId: CId): Map[Path, Changeset] = {
    val css = evalStep(p, st, mainCId)
    val preModes = css.groupBy(_.guard)
    preModes.foldLeft(preModes) {
      case (msTmp, (g, scs)) =>
        val msTmpMinusG = msTmp - g
        // Modes whose guards imply g
        val mwhig = msTmpMinusG.filter { case (pg, _) => g.forall(pg contains _) }
        if (mwhig isEmpty) msTmp
        else msTmpMinusG ++ mwhig.map{ case (pg, pm) => pg -> (pm union scs) }
    }.mapValues(cs => cs.reduce(_ || _)) // FIXME Inline continuous assignments
  }

  /* Utilities */

  /** Replace spaces with */
  def toValidFunctionNamePart(s: String) =
    s.hashCode.toHexString
  
  def op(o: String, ps: Expr*): Expr = Op(Name(o,0), ps.toList)
           
  /* AST Traversal, used by flatten() to convert Prog to the modes Map */
  
  def evalStep(p:Prog, st: Store, rootId:CId) : List[Changeset] = {
    val cl = getCls(rootId, st)
    val as = classDef(cl, p).body
    val env = HashMap((self, VObjId(Some(rootId))))
    evalActions(Set.empty, as, env, p, st)
  }
  
  def evalActions(path: Path, as:List[Action], env:Env, p:Prog, st: Store) : List[Changeset] =
    if (as isEmpty) List(Changeset.empty.copy(guard = path)) 
    else as.flatMap(evalAction(path, _: Action, env, p, st))
    
  def evalAction(path: Path, a:Action, env:Env, p:Prog, st: Store) : List[Changeset] =
    a match {
      case IfThenElse(c,a1,a2) =>
        val cAndPath = path + c
        val notCAndPath = path + op("not", c)
        evalActions(cAndPath, a1, env, p, st) union evalActions(notCAndPath, a2, env, p, st)
      case Switch(mode, cs) =>
        cs.foldLeft(List.empty[Changeset]){ 
          case (tmp, Clause(lhs,claim,rhs)) =>
            val clausePath = path + op("==", mode, Lit(lhs))
            tmp union evalActions(clausePath, rhs, env, p, st) }
      case Discretely(da) =>
        evalDiscreteAction(path, da, env, p, st)
      case Continuously(ca) =>
        evalContinuousAction(path, ca, env, p, st) 
      case Hypothesis(s, e) =>
        if (path isEmpty) // top level action
          logHypothesis(selfCId(env), s, e, env)
        else 
          throw internalPosError("Hypothesis statements are only allowed on the top level.", e.pos)
      case ForEach(n, col, body) => //TODO Add support for for-each statements
        throw internalPosError("For-each statements are not supported in the 2015 Compiled semantics.", col.pos)
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

  def evalDiscreteAction(path: Path, a:DiscreteAction, env:Env, p:Prog, st: Store) : List[Changeset] =
    a match {
      case Assign(Dot(o @ Dot(Var(self),Name(simulator,0)), n), e) =>
        Nil // TODO Ensure that this does not cause trouble down the road 
      case Assign(e @ Dot(o, _), rhs) =>
        /* Schedule the discrete assignment */
        val (rd, rRhs) = resolve(e, rhs, env, st)
        logAssign(path, rd.id, Discretely(Assign(rd,rRhs)), env)
      /* Basically, following says that variable names must be 
         fully qualified at this language level */
      case c: Create =>
        throw internalPosError("The 2015 Compiled semantics does not support create statements in the always section.", c.pos)
      case Assign(_,_) => 
        throw BadLhs()
    }

  def evalContinuousAction(path: Path, a:ContinuousAction, env:Env, p:Prog, st: Store) : List[Changeset] = 
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
  
  /** Evaluate e in the scope of env for definitions p with current store st */
  def evalExpr(e:Expr, env:Env, st:Store) : CValue = {
    def eval(env:Env, e:Expr) : CValue = try {
      e match {
        case Lit(i)         => VLit(i)
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

}