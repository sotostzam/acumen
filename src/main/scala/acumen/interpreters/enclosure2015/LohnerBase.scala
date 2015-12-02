package acumen
package interpreters
package enclosure2015

import enclosure.Interval
import enclosure2015.Common._
import interpreters.Common._
import FAD.FDif
import TAD.TDifAsReal
import Pretty.pprint
import util._
import util.Canonical._
import util.Conversions.{
  extractDouble, extractDoubles, extractId, extractInterval, extractIntervals
}
import Errors._

/** Interval evaluation of solveIVPTaylor */
object intervalBase extends LohnerBase()(intervalCValueIsReal, intervalCValueTDifIsReal)
  
/** FDif[Interval] evaluation of solveIVPTaylor */
object fDifBase extends LohnerBase()(fDifCValueIsReal, fDifCValueTDifIsReal)

/** Base for evaluation of solveIVPTaylor */
case class LohnerBase
  ( implicit cValueIsReal:     Real[CValue]
  ,          cValueTDifIsReal: TDifAsReal[CValue] 
  ) extends SolverBase {
  
  type E = DynSetEnclosure
  
  def initializeEnclosure(st: CStore): CValueEnclosure = {
    val nameToIndex = st.toList.sortBy(_._1).flatMap {
      case (id, co) => co.toList.sortBy(_._1).flatMap {
        case (n, VLit(v: GConstantRealEnclosure)) => List((id, n))
        case (n, v)                               => Nil
      }
    }.zipWithIndex.toMap
    val indexToName = nameToIndex.map(_.swap)
    val dim = indexToName.size
    def initializeVector(f: Interval => Interval) =
      breeze.linalg.Vector.tabulate[CValue](dim) { i =>
        val (id, n) = indexToName(i)
        getObjectField(id, n, st) match {
          case VLit(e: GConstantRealEnclosure) => VLit(GConstantRealEnclosure(f(e.range)))
        }}
    val midpoint = initializeVector(i => Interval(i.midpoint))
    val width = initializeVector{ i => val w = i.width / 2; Interval((-w).lo, w.hi) }
    val zero = VLit(GConstantRealEnclosure(Interval.zero))
    val one = VLit(GConstantRealEnclosure(Interval.one))
    val linearTransformation = breeze.linalg.Matrix.tabulate[CValue](dim, dim) { case (r, c) if r == c => one; case _ => zero }
    val error = breeze.linalg.Vector.fill[CValue](dim)(zero)
    CValueEnclosure(st, Cuboid(midpoint, linearTransformation, width, error), nameToIndex, indexToName, Set.empty) 
  }
  
  case class ODEEnv
    ( s: RealVector 
    , private val e: DynSetEnclosure
    ) extends EStore {
    val dim = s.size
    def nameToIndex = e.nameToIndex
    def indexToName = e.indexToName
    /* EStore */
    def childrenOf(id: CId): List[CId] = e.childrenOf(id)
    def getObjectField(id: CId, n: Name): CValue = e.nameToIndex.get(id,n) match {
      case Some(i) if !(e.nonOdeIndices contains i) => s(i)
      case _ => e.getObjectField(id, n)
    }
  }

  implicit class RichStoreImpl(odeEnv: ODEEnv) extends RichStore[ODEEnv,CId] {
    /* RichStore */
    override def +++(that: ODEEnv): ODEEnv =
      odeEnv.copy(s = breeze.linalg.Vector.tabulate(odeEnv.dim)(i => odeEnv.s(i) + that.s(i)))
    override def ***(that: Double): ODEEnv =
      odeEnv.copy(s = breeze.linalg.Vector.tabulate(odeEnv.dim)(i => odeEnv.s(i) * cValueIsReal.fromDouble(that)))
    override def map(m: CValue => CValue): ODEEnv = odeEnv.copy(s = odeEnv.s map m)
    override def mapName(m: (GId, Name, CValue) => CValue): ODEEnv =
      odeEnv.copy(s = breeze.linalg.Vector.tabulate[CValue](odeEnv.dim){ i => 
        val (cid,n) = odeEnv indexToName i
        m(cid, n, odeEnv s i)
      })
    override def apply(id: CId, n: Name): CValue = odeEnv.s(odeEnv.nameToIndex(id, n))
    override def updated(id: CId, n: Name, v: CValue): ODEEnv =
      // TODO: Group updates or do this with mutation instead
      odeEnv.copy(s = { val s1 = odeEnv.s.copy; s1.update(odeEnv.nameToIndex(id, n), v); s1 })
    override def getInSimulator(variable: String) = odeEnv.getInSimulator(variable)
  }
  def liftODEEnv(s: ODEEnv)(implicit field: FieldImpl): RichStoreImpl = RichStoreImpl(s)
  
  case class FieldImpl(odes: List[CollectedAction], evalExpr: (Expr, Env, EStore) => CValue) extends interpreters.Common.Field[ODEEnv,CId] {
    override def apply(odeEnv: ODEEnv): ODEEnv = {
      val s1 = odeEnv.s.copy
      odes.foreach{ ode => 
        s1.update(odeEnv.nameToIndex(ode.lhs.id, ode.lhs.field), evalExpr(ode.rhs, ode.env, odeEnv)) 
      }
      odeEnv.copy(s = s1)
    }
    override def variables(s: ODEEnv): List[(CId, Name)] = odes.map(ode => (ode.lhs.id, ode.lhs.field))
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
  
  case class CValueEnclosure
    ( private val st: CStore
    , dynSet: Cuboid
    , nameToIndex: Map[(CId,Name), Int]
    , indexToName: Map[Int, (CId,Name)]
    , nonOdeIndices: Set[Int]
    , cachedOuterEnclosure: Option[RealVector] = None
    ) extends DynSetEnclosure with EStore {
    
    def initialize(s: CStore): Enclosure = initializeEnclosure(s)
    
    // FIXME should it not take into account the actual variables in the dynset
    lazy val outerEnclosure = cachedOuterEnclosure.getOrElse(dynSet.outerEnclosure)
    
    /* Store Operations */
      
    def cStore: CStore = st
  
    override def getObjectField(id: CId, f: Name) =
      nameToIndex.get((id, f)) match {
        case Some(i) if !(nonOdeIndices contains i) =>
          outerEnclosure(i)
        case _ =>
          super.getObjectField(id, f)
      }
    override def setObjectField(id:CId, f:Name, v:CValue): Enclosure =
      nameToIndex.get((id,f)) match {
        case Some(i) =>
          Logger.trace(s"Setting Lohner set variable $id.${Pretty pprint f}.")
          super.setObjectField(id, f, v)
        case None =>
          this.copy(st = Canonical.setObjectField(id, f, v, st))
      }
       
    /* Enclosure Operations */
    
    /** Apply m to all CValues in the CStore and Lohner set components */
    def map(m: CValue => CValue): Enclosure =
      CValueEnclosure( st.mapValues(_ mapValues m)
                     , dynSet.map(m)
                     , nameToIndex
                     , indexToName
                     , nonOdeIndices )
    /** Apply m to all CValues in the CStore and Lohner set components with the 
     *  CId and Name of the value in context */
    def mapName(m: (CId, Name, CValue) => CValue): Enclosure = {
      CValueEnclosure( st.map{ case (cid,co) => 
                         (cid, co.map{ case (n,v) =>
                           (n, m(cid,n,v))  
                         }) 
                       }
                     , dynSet.map((i: Int, v: CValue) => m(indexToName(i)._1, indexToName(i)._2, v))
                     , nameToIndex
                     , indexToName
                     , nonOdeIndices )
    }
    
    
    /** Move the enclosure by the mapping m, returning range and image enclosures. */
    def move
      ( eqsInlined: Set[CollectedAction]
      , flow      : C1Flow
      , evalExpr  : (Expr, Env, EStore) => CValue
      ): (CValueEnclosure, CValueEnclosure) = {
      
      // Update variables in this.cStore defined by continuous assignments w.r.t. odeValues
      def updateCStore(odeValues: RealVector): CStore = {
        def updateODEVariables(unupdated: CStore, odeValues: RealVector): CStore =
          (0 until this.dim).foldLeft(unupdated) {
            case (tmpSt, i) =>
              val (id, n) = this.indexToName(i)
              Canonical.setObjectField(id, n, odeValues(i), tmpSt)
          }
        def updateEquationVariables(unupdated: CStore, odeValues: RealVector): CStore = {
          val odeStore = intervalBase.ODEEnv(odeValues, this)
          eqsInlined.foldLeft(unupdated){ case (stTmp, ca) =>
            val rd = ca.lhs
            val cv = evalExpr(ca.rhs, ca.env, odeStore)
            Canonical.setObjectField(rd.id, rd.field, cv, stTmp)
          }
        }
        updateEquationVariables(updateODEVariables(this.cStore, odeValues), odeValues)
      }
      
      // Indices in the Lohner set that are not defied by ODEs. These will become invalid after applying the mapping.
      // FIXME These can change over time! Will this cause issues? (see comment for LohnerEnclosure.nonOdeIndices)
      val eqIndices = 
        eqsInlined.map{ ca => val lhs = ca.lhs; this.nameToIndex(lhs.id, lhs.field) }
      
        val (rangeDynSet, endDynSet) = dynSet.move(flow)
        
        val rangeEnclosure = rangeDynSet.outerEnclosure
        val rangeStNext    = updateCStore(rangeEnclosure)
        
        val endEnclosure   = endDynSet.outerEnclosure
        val endStNext      = updateCStore(endEnclosure)
        
        ( CValueEnclosure( rangeStNext
                         , rangeDynSet                              
                         , this.nameToIndex
                         , this.indexToName
                         , eqIndices
                         , Some(rangeEnclosure) )
        , CValueEnclosure( endStNext
                         , endDynSet                              
                         , this.nameToIndex
                         , this.indexToName
                         , eqIndices
                         , Some(endEnclosure) ) )
      

    }
    
  }
  
}

