package acumen

import interpreters.Common.{
  Field, RichStore
}
import interpreters.enclosure.Interval
import util.ASTUtil.mapValue

object FAD extends App {
  
  /** Representation of a number and its derivatives. */
  case class FDif[V: Integral](head: V, coeff: Map[QName,V]) extends Dif[V,QName] {
    def apply(i: QName) = coeff.getOrElse(i, implicitly[Integral[V]].zero)
    val length = coeff.size + 1
    def map[W: Integral](m: V => W): FDif[W] = FDif(m(head), coeff mapValues m)
  }
  object FDif {
    /** Lift a constant value of type A to an FDif. */
    def constant[V: Integral](a: V) = FDif(a, Map.empty)
  }
  
  /** Lift all numeric values in a store into FDifs */
  def lift[S, I: Integral](st: RichStore[S, GId])(implicit n: QName, ns: List[QName]): S = st map liftValue[I] 
  
  /** Lift all numeric values in an Expr into FDifs */
  def lift[Id, V: Integral](e: Expr)(implicit n: QName, ns: List[QName]): Expr = new acumen.util.ASTMap {
    override def mapExpr(e: Expr): Expr = (e match {
      case Lit(gv) => Lit(liftGroundValue(gv))
      case _ => super.mapExpr(e)
    }).setPos(e.pos)
  }.mapExpr(e)

  private def liftGroundValue(gv: GroundValue)(implicit n: QName, ns: List[QName]): GroundValue = {
    val der = ns.map(id => id -> (if (id == n) 1d else 0d)).toMap
    gv match {
      case GDouble(d) => GDoubleFDif(FDif(d,          der))
      case GInt(i)    => GDoubleFDif(FDif(i.toDouble, der))
      case _          => gv
    }
  }
  
  /** Lift all the values inside a Value[Id] into FDifs */
  private def liftValue[I: Integral](v: Value[GId])(implicit n: QName, ns: List[QName]): Value[GId] = v match {
    case VLit(gv) => VLit(liftGroundValue(gv))
    case VVector(lv: List[Value[GId]]) => VVector(lv map liftValue[I])
    case VList(lv: List[Value[GId]]) => VList(lv map liftValue[I])
    case _ => v
  }
  
  /** Integral instance for FDif[V], where V itself has an Integral instance */
  abstract class FDifAsIntegral[V: Integral] extends DifAsIntegral[V,QName,FDif[V]] with Integral[FDif[V]] {
    /* Integral instance */
    def add(l: FDif[V], r: FDif[V]): FDif[V] = {
      require(l.coeff.keySet == r.coeff.keySet, "Cannot add FDifs with different sets of names.")
      FDif(l.head + r.head, l.coeff.keySet.map(k => k -> (l.coeff(k) + r.coeff(k))).toMap)
    }
    def sub(l: FDif[V], r: FDif[V]): FDif[V] = {
      require(l.coeff.keySet == r.coeff.keySet, "Cannot subtract FDifs with different sets of names.")
      FDif(l.head + r.head, l.coeff.keySet.map(k => k -> (l.coeff(k) - r.coeff(k))).toMap)
    }
    def neg(x: FDif[V]): FDif[V] =
      FDif(- x.head, x.coeff.mapValues(- _))
    def mul(l: FDif[V], r: FDif[V]): FDif[V] = {
      require(l.coeff.keySet == r.coeff.keySet, "Cannot multiply FDifs with different sets of names.")      
      FDif(l.head * r.head, l.coeff.keySet.map(k => k -> (l(k)*r.head + l.head*r(k))).toMap)
    }
    def fromInt(x: Int): FDif[V] = FDif.constant(evVIsIntegral fromInt x)
    def zero: FDif[V] = FDif constant zeroOfV
    def one: FDif[V] = FDif constant oneOfV
    def isConstant(x: FDif[V]): Boolean = x.coeff.values.forall(_ == zeroOfV)
  }
  
  /** Real instance for FDif[V], where V itself has a Real instance */
  abstract class FDifAsReal[V: Real] extends FDifAsIntegral[V] with Real[FDif[V]] {
    /* Constants */
    val evVIsReal = implicitly[Real[V]]
    /* Real instance */
    def div(l: FDif[V], r: FDif[V]): FDif[V] = {
      require(!r.head.isZero, "Division by zero is not allowed.")
      require(l.coeff.keySet == r.coeff.keySet, "Cannot multiply FDifs with different sets of names.")
      FDif(l.head * r.head, l.coeff.keySet.map(k => k -> ((l(k)*r.head - l.head*r(k)) / r.head.square)).toMap)
    }
    def pow(l: FDif[V], r: FDif[V]): FDif[V] = ???
    def exp(x: FDif[V]): FDif[V] = ???
    def log(x: FDif[V]): FDif[V] = ???
    def square(x: FDif[V]): FDif[V] = ???
    def sqrt(x: FDif[V]): FDif[V] = ???
    def sin(x: FDif[V]): FDif[V] = ???
    def cos(x: FDif[V]): FDif[V] = ???
    def tan(x: FDif[V]): FDif[V] = ???
    def acos(x: FDif[V]): FDif[V] = ???
    def asin(x: FDif[V]): FDif[V] = ???
    def atan(x: FDif[V]): FDif[V] = ???
    def fromDouble(x: Double): FDif[V] = FDif.constant(evVIsReal fromDouble x)
  }
  implicit object IntFDifIsIntegral extends FDifAsIntegral[Int] {
    def groundValue(v: FDif[Int]) = GIntFDif(v)
  }
  implicit object DoubleFDifIsReal extends FDifAsReal[Double] {
    def groundValue(v: FDif[Double]) = GDoubleFDif(v)
  }
  implicit object IntervalFDifIsReal extends FDifAsReal[Interval] {
    def groundValue(v: FDif[Interval]) = GIntervalFDif(v)
  }
  
  def computeJacobian[Id <: GId, S <% RichStore[S,Id], V: Real](f: Field[S,Id], s: S): Vector[FDif[V]] = {
    val vars = f.variables(s)
    val dim = 1 + vars.size
    val zero = implicitly[Integral[V]].zero
    val one = implicitly[Integral[V]].one
    vars.toVector.map{ case qn@(id,n) =>
      // FIXME Extend to vectors
      s(id,n) match {
        case VLit(v: V) =>
          val mappp: Map[QName,V] = vars.map{ case qn1@(id1,n1) => QName(id1,n1) -> (if (qn1 == qn) one else zero) }.toMap
          FDif(v, mappp)
      }
    }
  }

}