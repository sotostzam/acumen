package acumen

import interpreters.Common.{
  Field, RichStore
}
import interpreters.enclosure.Interval
import util.ASTUtil.mapValue

object FAD extends App {
  
  /** Representation of a number and its derivatives. */
  case class FDif[V: Integral](coeff: Seq[V], known: Option[Int]) extends Dif[V]
  object FDif {
    def apply[V: Integral](coeff: Seq[V], known: Int): FDif[V] = FDif(coeff, Some(known))
  }
  
  /** Lift all numeric values in a store into FDifs */
  def lift[S, Id, I: Integral](st: RichStore[S, Id])(implicit n: (Id, Name), ns: List[(Id, Name)]): S = st map liftValue[Id,I] 
  
  /** Lift all numeric values in an Expr into FDifs */
  def lift[Id, V: Integral](e: Expr)(implicit n: (Id, Name), ns: List[(Id, Name)]): Expr = new acumen.util.ASTMap {
    override def mapExpr(e: Expr): Expr = (e match {
      case Lit(gv) => Lit(liftGroundValue(gv))
      case _ => super.mapExpr(e)
    }).setPos(e.pos)
  }.mapExpr(e)
  
  private def liftGroundValue[Id](gv: GroundValue)(implicit n: (Id, Name), ns: List[(Id, Name)]): GroundValue = gv match {
    case GDouble(d) => GDoubleFDif(FDif(d :: ns.map(_ => 0d), 1 + ns.size))
    case GInt(i) => GDoubleFDif(FDif(i.toDouble :: ns.map(_ => 0d), 1 + ns.size))
    case _ => gv
  }
  
  /** Lift all the values inside a Value[Id] into TDifs */
  private def liftValue[Id, I: Integral](v: Value[Id])(implicit n: (Id, Name), ns: List[(Id, Name)]): Value[Id] = v match {
    case VLit(gv) => VLit(liftGroundValue(gv))
    case VVector(lv: List[Value[Id]]) => VVector(lv map liftValue[Id,I])
    case VList(lv: List[Value[Id]]) => VList(lv map liftValue[Id,I])
    case _ => v
  }
  
  /** Integral instance for TDif[V], where V itself has an Integral instance */
  abstract class FDifAsIntegral[V: Integral] extends DifAsIntegral[V,FDif[V]] with Integral[FDif[V]] {
    def dif(v: Seq[V], known: Option[Int]): FDif[V] = FDif(v, known)
    /* Integral instance */
    def mul(l: FDif[V], r: FDif[V]): FDif[V] = 
      FDif(Stream.from(0).map(k => l(k)*r(0) + l(0)*r(k)), combinedKnown(l,r))
    def fromInt(x: Int): FDif[V] = ???
    def zero: FDif[V] = ???
    def one: FDif[V] = ???
    /** Return index of first non-zero coefficient. When none exists, returns -1. */
    def firstNonZero(x: FDif[V]): Int = ???
  }
  
  /** Real instance for TDif[V], where V itself has a Real instance */
  abstract class FDifAsReal[V: Real] extends FDifAsIntegral[V] with Real[FDif[V]] {
    /* Constants */
    val evVIsReal = implicitly[Real[V]]
    /* Real instance */
    def div(l: FDif[V], r: FDif[V]): FDif[V] = 
      FDif(Stream.from(0).map(k => (l(k)*r(0) - l(0)*r(k)) / r(0).square), combinedKnown(l,r))
    def pow(l: FDif[V], r: FDif[V]): FDif[V] = ???
    def sin(x: FDif[V]): FDif[V] = ???
    def cos(x: FDif[V]): FDif[V] = ???
    def tan(x: FDif[V]): FDif[V] = ???
    def acos(x: FDif[V]): FDif[V] = ???
    def asin(x: FDif[V]): FDif[V] = ???
    def atan(x: FDif[V]): FDif[V] = ???
    def exp(x: FDif[V]): FDif[V] = ???
    def log(x: FDif[V]): FDif[V] = ???
    def square(x: FDif[V]): FDif[V] = ???
    def sqrt(x: FDif[V]): FDif[V] = ???
    def fromDouble(i: Double): FDif[V] = ???
    
    override def isValidInt(x: FDif[V]): Boolean = ???
    override def toInt(x: FDif[V]): Int = ???
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
  
  def computeJacobian[Id, S <% RichStore[S,Id], V: Real](f: Field[S,Id], s: S): Vector[FDif[V]] = {
    val vars = f.variables(s).toVector
    val dim = 1 + vars.size
    val zero = implicitly[Integral[V]].zero
    val one = implicitly[Integral[V]].one
    vars.toVector.map{ case qn@(id,n) =>
      // FIXME Extend to vectors
      s(id,n) match { 
        case VLit(v: V) =>
          FDif(v +: vars.map{ (qn1: (Id,Name)) => if (qn1 == qn) one else zero }, dim)
      }
    }
  }

}