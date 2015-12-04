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
  
  /* SolverBase interface */
  type E = DynSetEnclosure
  def initializeEnclosure(st: CStore) = DynSetEnclosure(st)
  
  /* Implicit Conversions */
  implicit class RichStoreImpl(dynSetEnclosure: DynSetEnclosure) extends RichStore[DynSetEnclosure,CId] {
    /* RichStore */
    override def +++(that: DynSetEnclosure): DynSetEnclosure =
      dynSetEnclosure.copy(dynSet = IntervalBox(breeze.linalg.Vector.tabulate(dynSetEnclosure.dim)(i => dynSetEnclosure.dynSet(i) + that.dynSet(i))))
    override def ***(that: Double): DynSetEnclosure =
      dynSetEnclosure.copy(dynSet = IntervalBox(breeze.linalg.Vector.tabulate(dynSetEnclosure.dim)(i => dynSetEnclosure.dynSet(i) * cValueIsReal.fromDouble(that))))
    override def map(m: CValue => CValue): DynSetEnclosure = dynSetEnclosure.copy(dynSet = dynSetEnclosure.dynSet map m)
    override def mapName(m: (GId, Name, CValue) => CValue): DynSetEnclosure =
      dynSetEnclosure.copy(dynSet = IntervalBox(breeze.linalg.Vector.tabulate[CValue](dynSetEnclosure.dim){ i => 
        val (cid,n) = dynSetEnclosure indexToName i
        m(cid, n, dynSetEnclosure dynSet i)
      }))
    override def apply(id: CId, n: Name): CValue = dynSetEnclosure.dynSet(dynSetEnclosure.nameToIndex(id, n))
    override def updated(id: CId, n: Name, v: CValue): DynSetEnclosure =
      // TODO: Group updates or do this with mutation instead
      dynSetEnclosure.copy(dynSet = { val encl = dynSetEnclosure.dynSet.copy; encl.update(dynSetEnclosure.nameToIndex(id, n), v); IntervalBox(encl) })
    override def getInSimulator(variable: String) = dynSetEnclosure.getInSimulator(variable)
  }
  
  def liftDynSetEnclosure(s: DynSetEnclosure)(implicit field: FieldImpl): RichStoreImpl = RichStoreImpl(s)
  
  case class FieldImpl(odes: List[CollectedAction], evalExpr: (Expr, Env, EStore) => CValue) extends interpreters.Common.Field[DynSetEnclosure,CId] {
    override def apply(dynSetEnclosure: DynSetEnclosure): DynSetEnclosure = {
      val s1 = dynSetEnclosure.dynSet.copy
      odes.foreach{ ode => 
        s1.update(dynSetEnclosure.nameToIndex(ode.lhs.id, ode.lhs.field), evalExpr(ode.rhs, ode.env, dynSetEnclosure)) 
      }
      dynSetEnclosure.copy(dynSet = IntervalBox(s1))
    }
    override def variables(s: DynSetEnclosure): List[(CId, Name)] = odes.map(ode => (ode.lhs.id, ode.lhs.field))
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

