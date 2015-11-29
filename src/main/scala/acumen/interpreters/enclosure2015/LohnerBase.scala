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

/** Base for evaluation of solveIVPTaylor */
case class LohnerBase
  ( implicit cValueIsReal:     Real[CValue]
  ,          cValueTDifIsReal: TDifAsReal[CValue] 
  ) extends SolverBase {
  
  type E = LohnerEnclosure
  
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
    CValueEnclosure(st, midpoint, linearTransformation, width, error, nameToIndex, indexToName, Set.empty) 
  }
  
  case class ODEEnv
    ( s: RealVector 
    , private val e: LohnerEnclosure
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
    , midpoint: RealVector
    , linearTransformation: RealMatrix  
    , width: RealVector  
    , error: RealVector
    , nameToIndex: Map[(CId,Name), Int]
    , indexToName: Map[Int, (CId,Name)]
    , nonOdeIndices: Set[Int]
    , cachedLohnerSet: Option[RealVector] = None
    ) extends LohnerEnclosure with EStore {
    
    def initialize(s: CStore): Enclosure = initializeEnclosure(s)
    
    lazy val lohnerSet =
      cachedLohnerSet.getOrElse(midpoint + (linearTransformation * width) + error)
  
    /* Store Operations */
      
    def cStore: CStore = st
  
    override def getObjectField(id: CId, f: Name) =
      nameToIndex.get((id, f)) match {
        case Some(i) if !(nonOdeIndices contains i) =>
          lohnerSet(i)
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
                     , midpoint.copy map m
                     , breeze.linalg.Matrix.tabulate[CValue](dim, dim) { 
                         (r, c) => m(linearTransformation(r, c)) 
                       }
                     , width.copy map m
                     , error.copy map m
                     , nameToIndex
                     , indexToName
                     , nonOdeIndices )
    /** Apply m to all CValues in the CStore and Lohner set components with the 
     *  CId and Name of the value in context */
    def mapName(m: (CId, Name, CValue) => CValue): Enclosure = {
      def mapVector(v: RealVector) = 
        breeze.linalg.Vector.tabulate[CValue](dim){ i => 
          val (cid,n) = indexToName(i)
          m(cid, n, v(i)) 
        }
      CValueEnclosure( st.map{ case (cid,co) => 
                         (cid, co.map{ case (n,v) =>
                           (n, m(cid,n,v))  
                         }) 
                       }
                     , mapVector(midpoint)
                     , breeze.linalg.Matrix.tabulate[CValue](dim, dim) {  (r, c) =>
                         val (cid,n) = indexToName(c) // Columns correspond to state variables 
                         m(cid, n, linearTransformation(r, c)) 
                       }
                     , mapVector(width)
                     , mapVector(error)
                     , nameToIndex
                     , indexToName
                     , nonOdeIndices )
    }
  }
  
}

/** Template for Real instances of Vs wrapped as CValues */
abstract class CValueIsReal[V] extends Real[CValue] {
  def ev: Real[V]
  implicit def wrap(i: V): CValue
  def unOp[A](x: CValue, op: V => A): A
  def binOp[A](a: CValue, b: CValue, op: (V, V) => A): A 
  def one: CValue = ev.one
  def zero: CValue = ev.zero
  def !=(a: CValue, b: CValue): Boolean = binOp(a, b , (l,r) => ev.!=(l,r))
  def ==(a: CValue, b: CValue): Boolean = binOp(a, b , (l,r) => ev.==(l,r))
  def acos(x: CValue): CValue = unOp(x, i => ev.acos(i))
  def add(a: CValue, b: CValue): CValue = binOp(a, b , (l,r) => ev.add(l,r))
  def asin(x: CValue): CValue = unOp(x, i => ev.asin(i))
  def atan(x: CValue): CValue = unOp(x, i => ev.atan(i))
  def cos(x: CValue): CValue = unOp(x, i => ev.cos(i))
  def div(a: CValue, b: CValue): CValue = binOp(a, b , (l,r) => ev.div(l,r))
  def exp(x: CValue): CValue = unOp(x, i => ev.exp(i))
  def groundValue(v: CValue): GroundValue = v match { case VLit(gv) => gv }
  def isValidDouble(x: CValue): Boolean = unOp(x, i => ev.isValidDouble(i))
  def isValidInt(x: CValue): Boolean = unOp(x, i => ev.isValidInt(i))
  def log(x: CValue): CValue = unOp(x, i => ev.log(i))
  def lteq(x: CValue, y: CValue): Boolean = binOp(x, y, (l,r) => ev.lteq(l,r))
  def mul(a: CValue, b: CValue): CValue = binOp(a, b , (l,r) => ev.mul(l,r))
  def neg(x: CValue): CValue = unOp(x, i => ev.neg(i))
  def pow(a: CValue, b: CValue): CValue = binOp(a, b , (l,r) => ev.pow(l,r))
  def sin(x: CValue): CValue = unOp(x, y => ev.sin(y))
  def sqrt(x: CValue): CValue = unOp(x, y => ev.sqrt(y))
  def square(x: CValue): CValue = unOp(x, y => ev.square(y))
  def sub(a: CValue, b: CValue): CValue = binOp(a, b, (l,r) => ev.sub(l,r))
  def tan(x: CValue): CValue = unOp(x, y => ev.tan(y))
  def toDouble(x: CValue): Double = unOp(x, y => ev.toDouble(y))
  def toInt(x: CValue): Int = unOp(x, y => ev.toInt(y))
  def tryCompare(a: CValue, b: CValue): Option[Int] = binOp(a, b, (l,r) => ev.tryCompare(l,r))
}

/** Interval evaluation of solveIVPTaylor */
object intervalBase extends LohnerBase()(intervalCValueIsReal, intervalCValueTDifIsReal)
  
object intervalCValueIsReal extends CValueIsReal[Interval] {
  val ev = implicitly[Real[Interval]]
  implicit def wrap(i: Interval): CValue = VLit(GConstantRealEnclosure(i))
  def unOp[A](x: CValue, op: Interval => A): A = 
    x match { case VLit(y: GConstantRealEnclosure) => op(y.range) }
  def binOp[A](a: CValue, b: CValue, op: (Interval, Interval) => A): A = 
    (a,b) match { case (VLit(l: GConstantRealEnclosure), VLit(r: GConstantRealEnclosure)) => op(l.range, r.range) }
  def fromDouble(i: Double): CValue = VLit(GConstantRealEnclosure(i))  
  def fromInt(i: Int): CValue = VLit(GConstantRealEnclosure(i))
}

object intervalCValueTDifIsReal extends TAD.TDifAsReal[CValue]()(intervalCValueIsReal) {
  def groundValue(v: TAD.TDif[CValue]) = GCValueTDif(v)
}

/** FDif[Interval] evaluation of solveIVPTaylor */
object fDifBase extends LohnerBase()(fDifCValueIsReal, fDifCValueTDifIsReal)

object fDifCValueIsReal extends CValueIsReal[FDif[Interval]] {
  val ev = implicitly[Real[FDif[Interval]]]
  implicit def wrap(i: FDif[Interval]): CValue = VLit(GIntervalFDif(i))
  def unOp[A](x: CValue, op: FDif[Interval] => A): A = 
    x match { case VLit(GIntervalFDif(y)) => op(y) }
  def binOp[A](a: CValue, b: CValue, op: (FDif[Interval], FDif[Interval]) => A): A = 
    (a,b) match { case (VLit(GIntervalFDif(l)), VLit(GIntervalFDif(r))) => op(l, r) }
  def fromDouble(i: Double): CValue = VLit(GIntervalFDif(FDif constant i))  
  def fromInt(i: Int): CValue = VLit(GIntervalFDif(FDif constant i))
}

object fDifCValueTDifIsReal extends TDifAsReal[CValue]()(fDifCValueIsReal) {
  def groundValue(v: TAD.TDif[CValue]) = GCValueTDif(v)
}
