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
  def initializeEnclosure(st: CStore): ODEEnv = {
    // TODO when introducing indexing, this one needs to match on indices too
    val nameToIndex = st.toList.sortBy(_._1).flatMap {
      case (id, co) => co.toList.sortBy(_._1).flatMap {
        case (n, VLit(v: GConstantRealEnclosure)) => List((id, n))
        case (n, v)                               => Nil
      }
    }.zipWithIndex.toMap
    val indexToName = nameToIndex.map(_.swap)
    def initialVector = breeze.linalg.Vector.tabulate[CValue](indexToName.size) { 
      i => val (id, n) = indexToName(i)
           getObjectField(id, n, st) match {
             case VLit(e: GConstantRealEnclosure) => VLit(GConstantRealEnclosure(e.range))
           }
    }

    ODEEnv(st, Cuboid(initialVector), nameToIndex, indexToName, Set.empty) 
  }
  
  object ODEEnv {
    def apply(v: RealVector, enc: DynSetEnclosure): ODEEnv =
      ODEEnv(enc.cStore, IntervalBox(v), enc.nameToIndex, enc.indexToName, enc.nonOdeIndices, Some(v))
  }
  
  case class ODEEnv
    ( st: CStore
    , dynSet: IntervalDynSet
    , nameToIndex: Map[(CId,Name), Int]
    , indexToName: Map[Int, (CId,Name)]
    , nonOdeIndices: Set[Int]
    , cachedOuterEnclosure: Option[RealVector] = None
    ) extends DynSetEnclosure {



        def init( st: CStore
    , dynSet: IntervalDynSet
    , nameToIndex: Map[(CId,Name), Int]
    , indexToName: Map[Int, (CId,Name)]
    , nonOdeIndices: Set[Int]
    , cachedOuterEnclosure: Option[RealVector] = None
    ) =     
   ODEEnv( st
    , dynSet
    , nameToIndex
    , indexToName
    , nonOdeIndices
    , cachedOuterEnclosure
    )
   
       
  
  }

  implicit class RichStoreImpl(odeEnv: ODEEnv) extends RichStore[ODEEnv,CId] {
    /* RichStore */
    override def +++(that: ODEEnv): ODEEnv =
      odeEnv.copy(dynSet = IntervalBox(breeze.linalg.Vector.tabulate(odeEnv.dim)(i => odeEnv.dynSet(i) + that.dynSet(i))))
    override def ***(that: Double): ODEEnv =
      odeEnv.copy(dynSet = IntervalBox(breeze.linalg.Vector.tabulate(odeEnv.dim)(i => odeEnv.dynSet(i) * cValueIsReal.fromDouble(that))))
    override def map(m: CValue => CValue): ODEEnv = odeEnv.copy(dynSet = odeEnv.dynSet map m)
    override def mapName(m: (GId, Name, CValue) => CValue): ODEEnv =
      odeEnv.copy(dynSet = IntervalBox(breeze.linalg.Vector.tabulate[CValue](odeEnv.dim){ i => 
        val (cid,n) = odeEnv indexToName i
        m(cid, n, odeEnv dynSet i)
      }))
    override def apply(id: CId, n: Name): CValue = odeEnv.dynSet(odeEnv.nameToIndex(id, n))
    override def updated(id: CId, n: Name, v: CValue): ODEEnv =
      // TODO: Group updates or do this with mutation instead
      odeEnv.copy(dynSet = { val encl = odeEnv.dynSet.copy; encl.update(odeEnv.nameToIndex(id, n), v); IntervalBox(encl) })
    override def getInSimulator(variable: String) = odeEnv.getInSimulator(variable)
  }
  def liftODEEnv(s: ODEEnv)(implicit field: FieldImpl): RichStoreImpl = RichStoreImpl(s)
  
  case class FieldImpl(odes: List[CollectedAction], evalExpr: (Expr, Env, EStore) => CValue) extends interpreters.Common.Field[ODEEnv,CId] {
    override def apply(odeEnv: ODEEnv): ODEEnv = {
      val s1 = odeEnv.dynSet.copy
      odes.foreach{ ode => 
        s1.update(odeEnv.nameToIndex(ode.lhs.id, ode.lhs.field), evalExpr(ode.rhs, ode.env, odeEnv)) 
      }
      odeEnv.copy(dynSet = IntervalBox(s1))
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
    
}

