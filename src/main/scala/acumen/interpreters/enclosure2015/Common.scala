package acumen
package interpreters
package enclosure2015

import enclosure.Interval
import enclosure2015.Common._
import interpreters.Common._
import Pretty.pprint
import util._
import util.Canonical._
import util.Conversions.{
  extractDouble, extractDoubles, extractId, extractInterval, extractIntervals
}
import Errors._
import scala.util.parsing.input.Position

object Common {

  val bannedFieldNames = List(self, parent, classf, nextChild, magicf)
  
  trait SolverBase {
    type E <: Enclosure
    def initializeEnclosure(st: CStore): E
  }
  
  def solverBase(st: CStore): SolverBase = getInSimulator("method", st) match {
    case VLit(GStr(`Taylor`)) => intervalBase
    case VLit(GStr(`Picard`)) => picardBase
  }
  def orderOfIntegration(st: CStore) = getInSimulator("orderOfIntegration", st) match {
    case VLit(GInt(taylorOrder)) => taylorOrder
  }
  def maxPicardIterations(st: CStore) = getInSimulator("maxPicardIterations", st) match {
    case VLit(GInt(maxPicardIterations)) => maxPicardIterations
  }
  
  /** Abstract store known to evalExpr */
  trait EStore {
    def getObjectField(id:CId, f:Name): CValue
    def childrenOf(id: CId): List[CId]
  }
  
  // FIXME: Replace breeze.linalg.Vector[CValue] with breeze.linalg.Vector[R] with R: Real
  type RealVector = breeze.linalg.Vector[CValue]
  type RealMatrix = breeze.linalg.Matrix[CValue]
  
  implicit class CValueOps(val v: CValue) extends AnyVal {
    def **(that: RealVector)(implicit ev: Real[CValue]) = that.copy.map(v * _)
  }
  
  trait LohnerEnclosure extends Enclosure {
    def midpoint: RealVector
    def linearTransformation: RealMatrix
    def width: RealVector
    def error: RealVector
    def nameToIndex: Map[(CId,Name), Int]
    def indexToName: Map[Int, (CId,Name)]
    /** LohnerBase.{ODEEnv,Enclosure}.getObjectField will use this to get values for 
      * non-ODE variables from the (up-to-date) cStore instead of the (out-of-date) lohnerSet. */
    def nonOdeIndices: Set[Int]

    def outerEnclosure: RealVector
    lazy val dim = midpoint.size
    
    /** Move the enclosure by the mapping m, returning range and image enclosures. */
    def move
      ( eqsInlined: Set[CollectedAction]
      , timeStep: Double
      , timeStepInterval: Interval
      , coarseEnclosure: (LohnerEnclosure, RealVector, Interval) => RealVector
      , encloseMap: (LohnerEnclosure, RealVector, RealVector, RealVector, Interval) => (RealVector, RealMatrix, RealVector)
      , evalExpr: (Expr, Env, EStore) => CValue
      ): (LohnerEnclosure, LohnerEnclosure)
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
      if (f != _3D && f != _3DView && f != devicef)
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
                case (VObjId(Some(o1)), Some(VObjId(Some(o2)))) => 
                  containsCObject(this(o1), that(o2))
                case (_, Some(tv)) => 
                  throw internalError(s"Contains not applicable to ${pprint(n)}: ${pprint(v)}, ${pprint(tv)}")
                
              })}}
        this.forall { case (cid, co) => containsCObject(co, that(cid)) }
      }
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
  
  /* Utilities */
  
  def fieldIdToName(o: CId, n: Name): String = s"$n@$o"
  
  def internalError(s: String): AcumenError = new AcumenError {
    def mesg = s 
  }

  def internalPosError(s: String, p: Position): PositionalAcumenError = new PositionalAcumenError {
    def mesg = s
  }.setPos(p)
  
}

