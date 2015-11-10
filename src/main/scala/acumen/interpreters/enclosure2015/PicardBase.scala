package acumen
package interpreters
package enclosure2015

import enclosure2015.Common._
import Errors._
import interpreters.Common._
import interpreters.enclosure.Contract
import interpreters.enclosure.ivp.{
  LohnerSolver, PicardSolver
}
import util.Canonical
import util.Canonical._

/** Base for evaluation of PicardSolver.solveIVP */
object picardBase extends SolverBase {
  
  type E = Enclosure
  
  def initializeEnclosure(st: CStore): E = CValueEnclosure(st)
  
  val solver = new PicardSolver {} // if (contraction) new LohnerSolver {} else new PicardSolver {}
  
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
  
}