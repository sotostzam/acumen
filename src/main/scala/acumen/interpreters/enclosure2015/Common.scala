package acumen
package interpreters
package enclosure2015

import enclosure.Interval
import enclosure2015.Common._
import interpreters.Common._
import Pretty.pprint
import util._
import util.ASTUtil.{
  dots, substitute
}
import util.Canonical._
import util.Conversions.{
  extractDouble, extractDoubles, extractId, extractInt, extractInterval, extractIntervals, extractString
}
import Errors._
import scala.util.parsing.input.Position

object Common {

  val bannedFieldNames = List(self, parent, classf, nextChild, magicf, _plot)
  
  /* DynSet names */
  
  val DynSetCuboid      = "Cuboid"
  val DynSetIntervalBox = "IntervalBox"
  val allDynSets = (DynSetCuboid, DynSetIntervalBox)
  
  /* DynSet Reorganization strategies */
    
  val ReorganizationOff               = "Off"
  val ReorganizationErrorExceedsWidth = "ErrorExceedsWidth"
  val allReorganizations = (ReorganizationOff, ReorganizationErrorExceedsWidth)

  /* Branch Merging strategies */
    
  val BranchMergingOff       = "Off"
  val BranchMergingEvolution = "Evolution"
  val allBranchMergingStrategies = (BranchMergingOff, BranchMergingEvolution)
  
  /* Interval splitting strategy */

  val splitOff = "Off"
  val splitInitial = "Initial"
  val allSplittingStrategies = (splitOff, splitInitial)

  /** Parameter carrier
   *  Note: Defaults are set here.  
   */
  // Note: When adding a parameter, remember to add it in all five places below.
  case class Parameters( time                          : Double                 =  0.0 
                       , endTime                       : Double                 = 10.0 
                       , timeStep                      : Double                 =  0.015625
                       , minTimeStep                   : Double                 =  0.015625
                       , maxTimeStep                   : Double                 =  0.015625
                       , dynSetType                    : String                 = DynSetCuboid 
                       , reorganization                : (String, List[Double]) = ("Off", Nil)
                       , method                        : String                 = Taylor
                       , orderOfIntegration            : Int                    = 4 
                       , maxPicardIterations           : Int                    = 1000 
                       , maxBranches                   : Int                    = 500 
                       , maxIterationsPerBranch        : Int                    = 1000 
                       , mergeBranches                 : List[String]           = List("Evolution")
                       , userSplit                     : List[String]           = List("Initial")
                       , intersectWithGuardBeforeReset : Boolean                = true 
                       , disableContraction            : Boolean                = false 
                       , hypothesisReport              : String                 = "Comprehensive"
                       , deadStore                     : Boolean                = false
                       )
  object Parameters {

    object Names {
      val time                          = "time"                         
      val endTime                       = "endTime"                      
      val timeStep                      = "timeStep"                     
      val minTimeStep                   = "minTimeStep"                     
      val maxTimeStep                   = "maxTimeStep"                     
      val dynSetType                    = "dynSetType"                   
      val reorganization                = "reorganization"                   
      val method                        = "method"                       
      val orderOfIntegration            = "orderOfIntegration"           
      val maxPicardIterations           = "maxPicardIterations"          
      val maxBranches                   = "maxBranches"                  
      val userSplit                     = "userSplit"
      val maxIterationsPerBranch        = "maxIterationsPerBranch"       
      val mergeBranches                 = "mergeBranches"                
      val intersectWithGuardBeforeReset = "intersectWithGuardBeforeReset"
      val disableContraction            = "disableContraction"           
      val hypothesisReport              = "hypothesisReport"             
      val deadStore                     = "deadStore"
    }
    
    /** Name -> (User-visible?, Default Value) */
    val defaults: Map[String, (Boolean, CValue)] = {
      import Names._
      val p = new Common.Parameters() // default values will be copied from here
      Map( time                          -> (true, VLit(GDouble(p.time)))
         , endTime                       -> (true, VLit(GDouble(p.endTime)))
         , timeStep                      -> (true, VLit(GDouble(p.timeStep)))
         , minTimeStep                   -> (true, VLit(GDouble(p.minTimeStep)))
         , maxTimeStep                   -> (true, VLit(GDouble(p.maxTimeStep)))
         , dynSetType                    -> (true, VLit(GStr(p.dynSetType)))
         , reorganization                -> (true, VVector[CId](VLit(GStr(p.reorganization._1)) :: p.reorganization._2.map(d => VLit(GDouble(d)))))
         , method                        -> (true, VLit(GStr(p.method)))
         , orderOfIntegration            -> (true, VLit(GInt(p.orderOfIntegration)))
         , maxPicardIterations           -> (true, VLit(GInt(p.maxPicardIterations)))
         , maxBranches                   -> (true, VLit(GInt(p.maxBranches)))
         , maxIterationsPerBranch        -> (true, VLit(GInt(p.maxIterationsPerBranch)))
         , mergeBranches                 -> (true, VVector(p.mergeBranches.map(s => VLit(GStr(s)))))
         , userSplit                     -> (true, VVector(p.userSplit.map(s => VLit(GStr(s)))))
         , intersectWithGuardBeforeReset -> (true, VLit(GBool(p.intersectWithGuardBeforeReset)))
         , disableContraction            -> (true, VLit(GBool(p.disableContraction)))
         , hypothesisReport              -> (true, VLit(GStr(p.hypothesisReport)))
         , deadStore                     -> (true, VLit(GBool(p.deadStore)))
         )
    }
    
    /** Load parameter values from st. 
     *  The Prog p is used the check if a simulator parameter in st was set by the user. */
    def apply(st: CStore, p: Option[Prog]): Parameters = {
      
      val paramsSetByUser: List[String] = if (p.isEmpty) Nil else p.get.defs.flatMap {
        case ClassDef(`cmain`, _, _, body) => body.flatMap {
          case Discretely(Assign(Dot(Dot(Var(`self`), `magicf`), Name(paramName, 0)), _)) =>
            List(paramName)
          case _ => Nil /* FIXME Also look for parameters inside nested actions */ }
        case _ => Nil }

      /* Force user to either only set timeStep (to use fixed stepping) 
       * or only set both maxTimeStep and minTimeStep (to use adaptive stepping) */
      if ((paramsSetByUser contains Names.timeStep) &&
          ((paramsSetByUser contains Names.minTimeStep) || (paramsSetByUser contains Names.maxTimeStep)))
        throw new InvalidTimeStepConfiguration(paramsSetByUser.filter(_.toLowerCase.contains("timestep")))
      
      val           time                             = extractDouble(getInSimulator(Names.time, st))
      val           endTime                          = extractDouble(getInSimulator(Names.endTime, st))
      val           timeStep                         = extractDouble(getInSimulator(Names.timeStep, st))
      val           minTimeStep                      = extractDouble(getInSimulator(Names.minTimeStep, st))
      val           maxTimeStep                      = extractDouble(getInSimulator(Names.maxTimeStep, st))
      val VLit(GStr (dynSetType))                    = getInSimulator(Names.dynSetType, st)
      val           reorganization                   = getInSimulator(Names.reorganization, st) match {
        case VLit(GStr(reorgName)) => 
          (reorgName, Nil)
        case VLit(GPattern(GStr(reorgName) :: reorgParams)) =>
          (reorgName, reorgParams map extractDouble)
        case VVector(VLit(GStr(reorgName)) :: reorgParams) =>
          (reorgName, reorgParams map extractDouble)
        case VVector(VLit(reorgName:GStrEnclosure) :: reorgParams) if reorgName.isThin =>
          (reorgName.range.head, reorgParams map extractDouble)
        case ro =>
          throw new InvalidReorganization(Pretty pprint ro)
      }
      val VLit(GStr (method))                        = getInSimulator(Names.method, st)
      val            orderOfIntegration              = extractInt(getInSimulator(Names.orderOfIntegration, st))
      val            maxPicardIterations             = extractInt(getInSimulator(Names.maxPicardIterations, st))
      val            maxBranches                     = extractInt(getInSimulator(Names.maxBranches, st))
      val            maxIterationsPerBranch          = extractInt(getInSimulator(Names.maxIterationsPerBranch, st))
      val            mergeBranches                   = getInSimulator(Names.mergeBranches, st) match {
        case VLit(GStr(strategy))                 => List(strategy)
        case VLit(GPattern(List(GStr(strategy)))) => List(strategy)
        case VVector(strategies)                  => strategies map extractString
      }
      val            splitIntervals                  = getInSimulator(Names.userSplit, st) match {
        case VLit(GStr(strategy))                 => List(strategy)
        case VLit(GPattern(List(GStr(strategy)))) => List(strategy)
        case VVector(strategies)                  => strategies map extractString
      }
      val VLit(GBool(intersectWithGuardBeforeReset)) = getInSimulator(Names.intersectWithGuardBeforeReset, st)
      val VLit(GBool(disableContraction))            = getInSimulator(Names.disableContraction, st)
      val VLit(GStr (hypothesisReport))              = getInSimulator(Names.hypothesisReport, st)
      val VLit(GBool(deadStore))                     = getInSimulator(Names.deadStore, st)
      Parameters( time                          = time                         
                , endTime                       = endTime                      
                , timeStep                      = timeStep                     
                , minTimeStep                   = minTimeStep
                , maxTimeStep                   = maxTimeStep
                , dynSetType                    = dynSetType                   
                , reorganization                = reorganization                       
                , method                        = method                       
                , orderOfIntegration            = orderOfIntegration           
                , maxPicardIterations           = maxPicardIterations          
                , maxBranches                   = maxBranches                  
                , maxIterationsPerBranch        = maxIterationsPerBranch       
                , userSplit                     = splitIntervals
                , mergeBranches                 = mergeBranches                
                , intersectWithGuardBeforeReset = intersectWithGuardBeforeReset
                , disableContraction            = disableContraction           
                , hypothesisReport              = hypothesisReport             
                , deadStore                     = deadStore
                )                                           
    }
  }
  
  trait EnclosureSolver[E <: Enclosure] {
    /** Computes the enclosure over T of the IVP defined by odes and enc.
     *  Uses convertEnclosure to ensure that continuousEncloser is called
     *  with the appropriate enclosure type E. */
    def solve
      ( odes: Set[CollectedAction] // Set of delayed ContinuousAction
      , eqs: Set[CollectedAction]
      , claims: Set[CollectedConstraint]
      , T: Interval
      , p: Prog
      , enc: Enclosure
      , evalExpr: (Expr,Env,EStore) => CValue
      )( implicit parameters: Parameters
      ): (Enclosure, Enclosure) =
      continuousEncloser(odes, eqs, claims, T, p, convertEnclosure(enc), evalExpr)
    /** Obtain an E <: Enclosure as required by continuousEncloser. */
    protected[EnclosureSolver] def convertEnclosure(e: Enclosure): E 
    /** Computes the enclosure over T of the IVP defined by odes and enc */
    protected[EnclosureSolver] def continuousEncloser
      ( odes: Set[CollectedAction] // Set of delayed ContinuousAction
      , eqs: Set[CollectedAction]
      , claims: Set[CollectedConstraint]
      , T: Interval
      , p: Prog
      , enc: E
      , evalExpr: (Expr,Env,EStore) => CValue
      )( implicit parameters: Parameters
      ): (Enclosure, Enclosure)
  }
  trait SolverBase {
    type E <: Enclosure
    def initializeEnclosure(st: CStore): E
    def solver: EnclosureSolver[E]
  }
  
  def solverBase(st: CStore): SolverBase = getInSimulator("method", st) match {
    case VLit(GStr(`Taylor`)) => intervalBase
    case VLit(GStr(`Picard`)) => picardBase
  }
  
  /** Abstract store known to evalExpr */
  trait EStore {
    def getObjectField(id:CId, f:Name): CValue
    def cStore: CStore
    def childrenOf(id: CId): List[CId]
  }

  val CertainTrue = GBoolEnclosure(Set(true))
  val CertainFalse = GBoolEnclosure(Set(false))
  val Uncertain = GBoolEnclosure(Set(true, false))
  
  /* Interface for Breeze Linalg */
  
  // FIXME: Replace breeze.linalg.Vector[CValue] with breeze.linalg.Vector[R] with R: Real
  type RealVector = breeze.linalg.Vector[CValue]
  type RealMatrix = breeze.linalg.Matrix[CValue]

  // Adding functionality to RealVector
  class FunctionalRealVector(v: RealVector) {
    def updated(i: Int, c: CValue) = {
      val tmpVector = v.copy
      tmpVector.update(i, c)
      tmpVector
    }
    def mapIndexwise(m: (Int, CValue) => CValue) = breeze.linalg.Vector.tabulate[CValue](v.length) { i => m(i, v(i)) }
    /** Maximum of the widths of the elements of v.
     *  Note: Only applicable when v is a vector of VLit(GConstantRealEnclosure) */
    def maxWidth: Double =
      v.iterator.foldLeft(Double.MinValue){ case (res, (_, VLit(GConstantRealEnclosure(v)))) =>
        val w = v.width.hiDouble 
        if (w > res) w else res
      }
  }
  implicit def RealVectorIsFunctional(v: RealVector) = new FunctionalRealVector(v)

  object midpointVector extends breeze.generic.UFunc with breeze.generic.MappingUFunc {
   implicit object implInterval extends Impl[CValue, CValue] {
     def apply(i: CValue) = i match { case VLit(GConstantRealEnclosure(i)) => VLit(GConstantRealEnclosure(Interval(i.midpoint))) }
   }
  }
  object centeredVector extends breeze.generic.UFunc with breeze.generic.MappingUFunc {
   implicit object implInterval extends Impl[CValue, CValue] {
     def apply(i: CValue) = i match { case VLit(GConstantRealEnclosure(i)) => VLit(GConstantRealEnclosure(Interval(-0.5, 0.5) * i.width)) }
   }
  }

  
  implicit class CValueOps(val v: CValue) extends AnyVal {
    def **(that: RealVector)(implicit ev: Real[CValue]) = that.copy.map(v * _)
  }
  
  trait Mapping {
    def apply(x: RealVector)(implicit parameters: Parameters): RealVector 
  }
  
  trait C1Mapping extends Mapping {
    def phi      (x: RealVector)(implicit parameters: Parameters) : RealVector
    def jacPhi   (x: RealVector)(implicit parameters: Parameters) : RealMatrix
    def remainder(x: RealVector)(implicit parameters: Parameters) : RealVector
  }
  
  trait Flow extends Mapping {
    def range: Mapping
  }
  
  trait C1Flow extends C1Mapping with Flow {
    def range: C1Mapping
  }
  
  trait Enclosure extends EStore {
    
    def initialize(st: CStore): Enclosure
    
    /* Enclosure as Map */
    
    def apply(id: CId): CObject =
      cStore(id)
    def foreach(p: ((CId,CObject)) => Unit): Unit =
      cStore foreach p
    def forall(p: ((CId,CObject)) => Boolean): Boolean =
      cStore forall p
    def keySet: Set[CId] =
      cStore.keySet
    def toList: List[(CId,CObject)] =
      cStore.toList
    
    /* Store Operations */
    
    def cStore: CStore
    def getObjectField(id: CId, f: Name) = Canonical.getObjectField(id, f, cStore)
    def setObject(id:CId, o:CObject): Enclosure = initialize(cStore updated (id,o))
    def setObjectField(id:CId, f:Name, v:CValue) : Enclosure = {
      val obj = apply(id)
      if (f != _3D && f != _3DView && f != devicef && f != _plot && id != simulatorId)
        obj.get(f) map { oldVal =>
          if (oldVal.yieldsPlots != v.yieldsPlots)
            throw new UnsupportedTypeChangeError(f, id, classOf(obj), oldVal, v, 
              "These values require a different number of plots")
        }
      setObject(id, setField(obj,f,v))
    }
    
    /* Simulator Object Operations 
     * NOTE: These operations only affect cStore */  
  
    def getInSimulator(f: Name): CValue = 
      getObjectField(simulatorId, f)
    def getInSimulator(s: String): CValue = 
      getInSimulator(Name(s, 0))
    def getTime: Double =
      Canonical getTime cStore
    def getEndTime: Double =
      Canonical getEndTime cStore
    def getTimeStep: Double =
      Canonical getTimeStep cStore
    def getResultType: ResultType =
      Canonical getResultType cStore
    def getCls(id: CId): ClassName = 
      Canonical classOf deref(id,cStore)
    def mainId: CId = 
      Canonical mainId cStore
    def simulatorId: CId = 
      Canonical magicId cStore
    def childrenOf(id: CId): List[CId] = 
      Canonical childrenOf (id, cStore)
    def checkAccessOk(id:CId, env:Env, context: Expr): Unit =
      interpreters.Common checkAccessOk (id, env, cStore, context)
    /**
     * Updates the variableCount simulator field. This corresponds to 
     * the number of plots of model variables that will be shown, but not the 
     * number of columns in the table, as the latter also contains information 
     * such as class names etc. 
     */
    def countVariables: Enclosure = 
      setObjectField(simulatorId, stateVars, VLit(GInt(countStateVars(cStore))))
    
    /* Enclosure Operations */
    
    /** Take the meet, or g.l.b., of e and that Enclosure. */
    def /\(that: Enclosure): Enclosure = this meet that
    def /\(that: Option[Enclosure]): Enclosure = if (that isDefined) this meet that.get else this
    /** Take the meet, or g.l.b., of e and that Enclosure. */
    def meet(that: Enclosure): Enclosure =
      merge(that: Enclosure, (l: GEnclosure[_], r: GEnclosure[_]) => (l,r) match {
        case (le: GConstantRealEnclosure, re: GConstantRealEnclosure) => Some(le /\ re)
        case (ls: GStrEnclosure, rs: GStrEnclosure) => Some(GStrEnclosure(ls.range union rs.range))
        case (ls: GIntEnclosure, rs: GIntEnclosure) => Some(GIntEnclosure(ls.range union rs.range))
        case (ls: GBoolEnclosure, rs: GBoolEnclosure) => Some(GBoolEnclosure(ls.range union rs.range))
      }).get
    /** Merge e and that Enclosure using ce to combine scalar enclosure values. */
    def merge(that: Enclosure, ce: (GEnclosure[_], GEnclosure[_]) => Option[GEnclosure[_]]): Option[Enclosure] = {
      require(this.keySet == that.keySet, "Can not merge enclosures with differing object sets.") // TODO Update for dynamic objects
      try {
        val st1 = for ((cid,coThis) <- this.cStore) yield { 
          val coThat = that(cid)
          require(coThis.keySet == coThat.keySet, "Can not merge objects with differing name sets.") // TODO Update for dynamic objects
          require(classOf(coThis) == classOf(coThat), s"Can not merge objects of differing models (${classOf(coThis).x}, ${classOf(coThat).x}).")
          (cid, 
            if (classOf(coThis) == cmagic)
              coThis // FIXME Sanity check this (that it is OK to merge Enclosures with different simulator objects)
            else
              for ((n, _) <- coThis) yield (n,
                (this.getObjectField(cid,n), that.getObjectField(cid,n)) match {
                  case (VLit(l: GEnclosure[_]), VLit(r: GEnclosure[_])) => 
                    VLit(ce(l, r).getOrElse(sys.error("Error when merging $cid.$n.")))
                  case (v @ VVector(List(VLit(l: GEnclosure[_]))),VVector(Nil)) =>
                    v // For choosing the first non-empty value of "_plot" when "_plot = ()" in the initially section 
                  case (l, r) if l == r => 
                    l
                })
        )}
        Some(initialize(st1))
      } catch { case e: Throwable => 
        e.printStackTrace()
        None } // FIXME Use Either to propagate error information 
    }
    /** Returns a copy of e with das applied to it. */
    def apply(das: Set[CollectedAction], evalExpr: (Expr, Env, EStore) => CValue): Enclosure = 
      update(das.map(d => (d.selfCId, d.lhs.field) -> evalExpr(d.rhs, d.env, this)).toMap)
    /** Update e with respect to u. */
    def update(u: Map[(CId, Name), CValue]): Enclosure =
      u.foldLeft(this: Enclosure) { case (res, ((id, n), v)) => res.setObjectField(id, n, v) }
    def contains(that: Enclosure, ignoredFields: Set[(CId,Name)]): Boolean =
      contains(that, ignoredFields, false)
    /** Field-wise containment that omits fields identified by "ignoredFields".
     *  Fields in the Simulator object other than time and timeStep are ignored. */
    def contains(that: Enclosure, ignoredFields: Set[(CId,Name)], ignoreTimeDomain: Boolean): Boolean = { // TODO Update for dynamic objects
      def containsCObject(lo: CObject, ro: CObject, id: CId): Boolean =
        if (classOf(lo) == cmagic && classOf(ro) == cmagic){
          if (ignoreTimeDomain || (lo(time) == ro(time) && lo(timeStep) == ro(timeStep)))
            true
          else 
            throw internalError("Attempt to check containment of enclosures with incompatoble time domains: " +
              s" [${lo(time)},${extractDouble(lo(time)) + extractDouble(lo(timeStep))}]" +
              s", [${ro(time)},${extractDouble(ro(time)) + extractDouble(ro(timeStep))}]") 
        }
        else lo.forall {
          case (n, v) =>
            if ((bannedFieldNames contains n) || (ignoredFields contains (id,n))) true
            else {
              ((v, ro get n) match {
                case (VLit(l: GConstantRealEnclosure), Some(VLit(r: GConstantRealEnclosure))) => 
                  l contains r
                case (VLit(l: GStrEnclosure), Some(VLit(r: GStrEnclosure))) => 
                  l contains r
                case (VLit(l: GIntEnclosure), Some(VLit(r: GIntEnclosure))) => 
                  l contains r
                case (VLit(l: GBoolEnclosure), Some(VLit(r: GBoolEnclosure))) => 
                  l contains r
                case (VLit(_:GStr) | _:VResultType | _:VClassName, Some(tv @ (VLit(_:GStr) | _:VResultType | _:VClassName))) => 
                  v == tv
                case (VObjId(Some(id1)), Some(VObjId(Some(id2)))) =>
                  require(id1 == id2, s"Contains can not compare objects with different CId: $id1 is not equal to $id2.")
                  containsCObject(this(id1), that(id2), id1)
                case (_, Some(tv)) => 
                  throw internalError(s"Contains not applicable to ${pprint(n)}: ${pprint(v)}, ${pprint(tv)}")
                
              })}}
        this.forall { case (cid, co) => containsCObject(co, that(cid), cid) }
      }
    /** Field-wise containment that checks all fields (in objects other than the Simulator). */
    def contains(that: Enclosure): Boolean = contains(that, Set.empty)
    /** Take the intersection of e and that Object. */
    def intersect(that: Enclosure): Option[Enclosure] = {
      Logger.trace("Intersecting enclosures")
      merge(that, { 
        case (l: GConstantRealEnclosure, r: GConstantRealEnclosure) => 
          l intersect r
        case (l: GStrEnclosure, r: GStrEnclosure) => 
          l intersect r
        case (l: GIntEnclosure, r: GIntEnclosure) => 
          l intersect r
        case (l: GBoolEnclosure, r: GBoolEnclosure) => 
          l intersect r
        case (l,r) => 
          if (l == r) Some(l) else None
      })
    }
    /** Apply m to all CValues in the CStore and Lohner set components */
    def map(m: CValue => CValue): Enclosure
    /** Use f to reduce this enclosure to a value of type A. */
    def foldLeft[A](z: A)(f: (A, GConstantRealEnclosure) => A): A =
      this.flatten.foldLeft(z) { case (r, (id, n, e)) => f(r, e) }
    /** Returns iterable of all GConstantRealEnclosures contained in this object and its descendants. */
    def flatten(): Iterable[(CId, Name, GConstantRealEnclosure)] =
      cStore.flatMap{ case (cid, co) => co.flatMap{ 
        case (n, VLit(ce:GConstantRealEnclosure)) => List((cid, n, ce))
        case _ => Nil
      }}
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
    def mapRhs(m: Expr => Expr) = copy(a = (a: @unchecked) match {
      case Discretely(x: Assign)      => Discretely(x.copy(rhs = m(x.rhs)))
      case Continuously(x: EquationT) => Continuously(x.copy(rhs = m(x.rhs)))
      case Continuously(x: EquationI) => Continuously(x.copy(rhs = m(x.rhs)))
    })
    override def toString() =
      s"$selfCId.${Pretty pprint (lhs: Expr)} = ${Pretty pprint rhs}"
  }
  case class CollectedConstraint(selfCId: CId, c: Expr, env: Env)
  case class CollectedHypothesis(selfCId: CId, s: Option[String], h: Expr, env: Env)
  
/**
   * In-line the variables defined by the equations in as into the equations in bs. 
   * This means that, in the RHS of each element of b, all references to variables that are 
   * defined by an equation in a.
   * 
   * NOTE: This implies a restriction on the programs that are accepted: 
   *       If the program contains an algebraic loop, an exception will be thrown.
   */
  def inline(as: Set[CollectedAction], bs: Set[CollectedAction], st: CStore): Set[CollectedAction] = {
    val uniqueIdToDA = as.map{ da => val rd = da.lhs; (rd.id, rd.field) -> da }.toMap
    def inline(hosts: Map[CollectedAction,List[CollectedAction]]): Set[CollectedAction] = {
      val next = hosts.map {
        case (host, inlined) =>
          dots(host.rhs).foldLeft((host, inlined)) {
          case (prev@(hostPrev, ildPrev), d) =>
              val rd = resolveDot(d, host.env, st)
              uniqueIdToDA.get((rd.id, rd.field)) match {
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
                    env = hostPrev.env ++ inlineMe.env.env)
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
  
  /* Utilities */
  
  def fieldIdToName(o: CId, n: Name): String = s"$n@$o"
  
  def internalError(s: String): AcumenError = new AcumenError {
    def mesg = s 
  }

  def internalPosError(s: String, p: Position): PositionalAcumenError = new PositionalAcumenError {
    def mesg = s
  }.setPos(p)
  
}

