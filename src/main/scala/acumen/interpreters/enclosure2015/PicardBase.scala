package acumen
package interpreters
package enclosure2015

import enclosure2015.Common._
import Errors._
import interpreters.Common._
import interpreters.enclosure._
import interpreters.enclosure.Types.{
  VarName
}
import interpreters.enclosure.ivp.{
  LohnerSolver, PicardSolver
}
import Pretty._
import util.ASTUtil.dots
import util.Canonical
import util.Canonical._
import util.DebugUtil.asProgram

/** Solver based on Affine Interval Function evaluation of a Picard iterator. */
object picardEnclosureSolver extends EnclosureSolver[Enclosure] {
  
  import picardBase._
  
  def convertEnclosure(e: Enclosure): Enclosure = e match {
    case dse: DynSetEnclosure => initializeEnclosure(dse.cStore)
    case _ => e 
  }

  
  def continuousEncloser
    ( odes: Set[CollectedAction] // Set of delayed ContinuousAction
    , eqs: Set[CollectedAction]
    , claims: Set[CollectedConstraint]
    , T: Interval
    , p: Prog
    , enc: Enclosure
    , evalExpr: (Expr,Env,EStore) => CValue
    ): (Enclosure, Enclosure) = {
    Logger.trace(s"continuousEncloserPicard (over $T)")
    val ic = contract(enc, claims, p, evalExpr) match {
      case Left(_) => sys.error("Initial condition violates claims {" + claims.map(c => pprint(c.c)).mkString(", ") + "}.")
      case Right(r) => r
    }
    val varNameToFieldId = varNameToFieldIdMap(ic)
    val F = getFieldFromActions(inline(eqs, odes, enc.cStore), ic, p, evalExpr) // in-line eqs to obtain explicit ODEs
    val stateVariables = varNameToFieldId.keys.toList
    val A = new Box(stateVariables.flatMap{ v => 
      val (o,n) = varNameToFieldId(v)
      (ic(o)(n): @unchecked) match {
        case VLit(ce: GConstantRealEnclosure) => (v, ce.range) :: Nil
        case VObjId(_) => Nil
        case e => sys.error((o,n) + " ~ " + e.toString)
      }
    }.toMap)
    val solutions = legacySolver.solveIVP(F, T, A, delta = 0, m = 0, n = 200, degree = 1) // FIXME Expose as simulator parameters
    def updateICToSolution(pickValue: (VarName, Interval) => Interval) = {
      val solutionMap = solutions._1.apply(T).map{
        case (k, v) => (varNameToFieldId(k) -> VLit(GConstantRealEnclosure(pickValue(k,v))))
      }
      val odeSolutions = ic update solutionMap
      val equationsMap = inline(eqs, eqs, enc.cStore).map { // LHSs of EquationsTs, including highest derivatives of ODEs
        case CollectedAction(_, cid, Continuously(EquationT(ResolvedDot(_, _, n), rhs)), env) =>
          (cid, n) -> evalExpr(rhs, env, odeSolutions)
      }.toMap
      odeSolutions update equationsMap
    }
    ( updateICToSolution((k,v) => v               /* range enclosure */)
    , updateICToSolution((k,v) => solutions._2(k) /* end-time enclosure */) )
  }
  
}

/** Base for PicardEnclosureSolver */
object picardBase extends SolverBase {
  
  type E = Enclosure
  
  def initializeEnclosure(st: CStore): E = CValueEnclosure(st)
  
  def solver = picardEnclosureSolver
  
  val legacySolver = new PicardSolver {} // if (contraction) new LohnerSolver {} else new PicardSolver {}
  val extract = new acumen.interpreters.enclosure.Extract{}
  private val contractInstance = new Contract{}
  
  /** Representation of a set of ODEs. */
  case class FieldImpl(odes: List[CollectedAction], evalExpr: (Expr, Env, EStore) => CValue) extends interpreters.Common.Field[Enclosure,CId] {
    /** Evaluate the field (the RHS of each equation in ODEs) in s. */
    override def apply(s: Enclosure): Enclosure =
      odes.foldLeft(s){ case (tmpSt, ode) => 
        s.setObjectField(ode.lhs.id, ode.lhs.field, evalExpr(ode.rhs, ode.env, s)) 
      }
    /** NOTE: Assumes that the de-sugarer has reduced all higher-order ODEs.  */
    override def variables(s: Enclosure): List[(CId, Name)] = odes.map(ode => (ode.lhs.id, ode.lhs.field))
    override def map(em: Expr => Expr) =
      FieldImpl(odes.map(ode => ode.copy(a = (ode.a: @unchecked) match {
        case Discretely(Assign(lhs: Expr, rhs: Expr)) =>
          Discretely(Assign(em(lhs), em(rhs)))
        case Continuously(EquationT(lhs: Expr, rhs: Expr)) =>
          Continuously(EquationT(em(lhs), em(rhs)))
        case Continuously(EquationI(lhs: Expr, rhs: Expr)) =>
          Continuously(EquationI(em(lhs), em(rhs)))
      })), evalExpr)
  }
  
  /**
   * Embedded DSL for expressing integrators.
   * NOTE: Operators affect only field.variables and field.derivatives.
   */
  case class RichStoreImpl(s: Enclosure)(implicit field: FieldImpl) extends RichStore[Enclosure,CId] {
    override def +++(that: Enclosure): Enclosure = ???
    override def ***(that: Double): Enclosure = ???
    override def map(m: CValue => CValue): Enclosure = s map m 
    override def mapName(m: (GId, Name, CValue) => CValue): Enclosure = s mapName m 
    override def apply(id: CId, n: Name): CValue = s.getObjectField(id, n)
    override def updated(id: CId, n: Name, v: CValue): Enclosure = s.setObjectField(id, n, v)
    override def getInSimulator(variable: String) = s.getInSimulator(variable)
  }
  implicit def liftStore(s: Enclosure)(implicit field: FieldImpl): RichStoreImpl = RichStoreImpl(s)
  
  case class CValueEnclosure(st: CStore) extends Enclosure with EStore {
  
    def initialize(s: CStore): Enclosure = initializeEnclosure(s)
    
    /* Store Operations */
      
    def cStore: CStore = st
  
    /* Enclosure Operations */

    /** Apply m to all CValues in the CStore and Lohner set components */
    def map(m: CValue => CValue): Enclosure =
      CValueEnclosure(st.mapValues(_ mapValues m))
    /** Apply m to all CValues in the CStore and Lohner set components with the 
     *  CId and Name of the value in context */
    def mapName(m: (CId, Name, CValue) => CValue): Enclosure =
      CValueEnclosure(st.map{ case (cid,co) => 
        (cid, co.map{ case (n,v) => (n, m(cid,n,v)) }) })
  }
  
  /** Convert odes into a Field compatible with acumen.interpreters.enclosure.ivp.IVPSolver. */
  def getFieldFromActions(odes: Set[CollectedAction], st: Enclosure, p: Prog, evalExpr: (Expr,Env,EStore) => CValue): enclosure.Field =
    Field(odes.map { case CollectedAction(_, cid, Continuously(EquationI(ResolvedDot(_, _, n), rhs)), env) =>
      (fieldIdToName(cid, n), acumenExprToExpression(rhs, cid, env, st, p, evalExpr))
    }.toMap)
  
  def varNameToFieldIdMap(e: Enclosure): Map[VarName,(CId,Name)] =
    e.toList.flatMap{ case (cid, co) =>
      if (classOf(co) == cmagic) Map[String,(CId,Name)]()
      else co.filter{
        case (n,_) if bannedFieldNames contains n => false
        case (_, VLit(_: GStr) | VLit(_: GStrEnclosure) | VLit(_: GBoolEnclosure) | _:VObjId[_]) => false
        case (_, VLit(_: GConstantRealEnclosure)) => true
        case (n,v) =>
          val typ = "type " + v.getClass.getSimpleName
          throw new UnsupportedTypeError(typ, s"(${cid.cid.toString}:${e.getCls(cid).x}).${pprint(n)}", v)
      }.map{ case (n,v) => (fieldIdToName(cid,n), (cid, n)) } 
    }.toMap
    
  def acumenExprToExpression(e: Expr, selfCId: CId, env: Env, st: Enclosure, p: Prog, evalExpr: (Expr,Env,EStore) => CValue): Expression = {
    def convert(x: Expr) = acumenExprToExpression(x, selfCId, env, st, p, evalExpr)
    import util.Names.op
    e match {
      case Lit(v) if v.eq(Constants.PI) => Constant(Interval.pi) // Test for reference equality not structural equality
      case Lit(GInt(d))                 => Constant(d)
      case Lit(GDouble(d))              => Constant(d)
      case Lit(e:GConstantRealEnclosure)=> Constant(e.range) // FIXME Over-approximation of end-time interval!
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
      case Op(Name("asin"   , 0), List(x))    => ASin(convert(x))  
      case Op(Name("atan"   , 0), List(x))    => ATan(convert(x))  
      case Op(Name("exp"    , 0), List(x))    => Exp(convert(x))   
      case Op(Name("log"    , 0), List(x))    => Log(convert(x))   
      case Op(Name("log10"  , 0), List(x))    => Log10(convert(x)) 
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
  
  /**
   * Contract st based on all claims.
   * NOTE: Returns Left if the support of any of the claims has an empty intersection with st,
   *       or if some other exception is thrown by contract.
   */
  def contract(enc: Enclosure, claims: Iterable[CollectedConstraint], prog: Prog, evalExpr: (Expr,Env,EStore) => CValue): Either[String, Enclosure] = {
    /**
     * Given a predicate p and store st, removes that part of st for which p does not hold.
     * NOTE: The range of st is first computed, as contraction currently only works on intervals.  
     */
    def contract(st: Enclosure, p: Expr, prog: Prog, env: Env, selfCId: CId): Either[String,Enclosure] = {
      lazy val box = envBox(p, env, st, evalExpr)
      val varNameToFieldId = varNameToFieldIdMap(st)
      val noUpdate = Map[(CId,Name), CValue]()
      def toAssoc(b: Box) = b.map{ case (k, v) => (varNameToFieldId(k), VLit(GConstantRealEnclosure(v))) }
      p match {
        case Lit(CertainTrue | Uncertain) => Right(st)
        case Lit(CertainFalse) => Left("Contracted with CertainFalse")
        case Op(Name("||",0), _) => Right(st)
        case Op(Name("&&",0), List(l,r)) => 
          (contract(st,l,prog,env,selfCId), contract(st,r,prog,env,selfCId)) match {
            case (Right(pil), Right(pir)) => 
              (pil intersect pir) map (Right(_)) getOrElse Left("Empty intersection.")
            case (Right(_), Left(pir)) => Left(pir)
            case (Left(pil), Right(_)) => Left(pil)
            case (Left(pil), Left(pir)) => Left(pil + ", " + pir)
          }
        case Op(Name(op,0), List(l,r)) =>
          val lv = evalExpr(l, env, st)
          val rv = evalExpr(r, env, st)
          lazy val le = acumenExprToExpression(l,selfCId,env,st,prog,evalExpr)
          lazy val re = acumenExprToExpression(r,selfCId,env,st,prog,evalExpr)
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
          Right(st update smallerBox)
        case _ => 
          Right(st) // Do not contract
      }
    }
    val VLit(GBool(disableContraction)) = enc getInSimulator "disableContraction"
    if (disableContraction) Right(enc)
    else claims.foldLeft(Right(enc): Either[String, Enclosure]) {
      case (res, claim) => res match {
        case Right(r) => 
          try {
            contract(r, claim.c, prog, claim.env, claim.selfCId).
              fold(s => Left("Empty enclosure after applying claim " + pprint(claim.c) + ": " + s), Right(_)) 
          } catch {
            case e: Throwable =>
              if (e.isInstanceOf[AcumenError]) throw e
              else if (e.isInstanceOf[NotImplementedError]) 
                throw new NotImplementedError(s"Cannot contract. Missing implementation for: ${e.getMessage}.") 
              else Left("Error while applying claim " + pprint(claim.c) + ": " + e.getMessage) 
          }
        case _ => res
      }
    }
  }
  
  /** Box containing the values of all variables (Dots) that occur in it. */
  def envBox(e: Expr, env: Env, st: Enclosure, evalExpr: (Expr,Env,EStore) => CValue): Box = {
    new Box(dots(e).flatMap{ case d@Dot(obj,n) =>
      val VObjId(Some(objId)) = evalExpr(obj, env, st) 
      evalExpr(d, env, st) match {
        case VLit(r: GConstantRealEnclosure) => (fieldIdToName(objId.cid,n), r.range) :: Nil
        case VLit(r: GStrEnclosure) => Nil
      }
    }.toMap)
  }
  
}