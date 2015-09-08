package acumen

import interpreters.Common.{
  Field, RichStore
}
import interpreters.enclosure.Interval
import util.ASTUtil.mapValue

object FAD extends App {
  
  /** Representation of a number and its derivatives. */
  case class FDif[V: Integral](coeff: Seq[V], length: Int) extends Dif[V] {
    def apply(i: Int) = if (i < length) coeff(i) else implicitly[Integral[V]].zero
    def map[W: Integral](m: V => W): FDif[W] = FDif(coeff map m, length)
    def indexWhere(m: V => Boolean): Int = coeff.take(length).indexWhere(c => m(c))
  }
  object FDif {
    /** Lift a constant value of type A to an FDif. */
    def constant[V: Integral](a: V) = FDif(a #:: Stream.continually(implicitly[Integral[V]].zero), 1)
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
  
  /** Lift all the values inside a Value[Id] into FDifs */
  private def liftValue[Id, I: Integral](v: Value[Id])(implicit n: (Id, Name), ns: List[(Id, Name)]): Value[Id] = v match {
    case VLit(gv) => VLit(liftGroundValue(gv))
    case VVector(lv: List[Value[Id]]) => VVector(lv map liftValue[Id,I])
    case VList(lv: List[Value[Id]]) => VList(lv map liftValue[Id,I])
    case _ => v
  }
  
  /** Integral instance for FDif[V], where V itself has an Integral instance */
  abstract class FDifAsIntegral[V: Integral] extends DifAsIntegral[V,FDif[V]] with Integral[FDif[V]] {
    def dif(v: Seq[V], length: Int): FDif[V] = FDif(v, length)
    /* Integral instance */
    def add(l: FDif[V], r: FDif[V]): FDif[V] =
      FDif(Stream.from(0).map(k => l(k) + r(k)), combinedLength(l,r))
    def sub(l: FDif[V], r: FDif[V]): FDif[V] =
      FDif(Stream.from(0).map(k => l(k) - r(k)), combinedLength(l,r))
    def neg(x: FDif[V]): FDif[V] =
      FDif(Stream.from(0).map(k => - x(k)), x.length)
    def mul(l: FDif[V], r: FDif[V]): FDif[V] = 
      FDif(Stream.from(0).map(k => l(k)*r(0) + l(0)*r(k)), combinedLength(l,r))
    def fromInt(x: Int): FDif[V] = FDif.constant(evVIsIntegral fromInt x)
    lazy val zero: FDif[V] = FDif.constant(zeroOfV)
    lazy val one: FDif[V] = FDif.constant(oneOfV)
    def isConstant(x: FDif[V]) = (1 until x.length).forall(x(_) == zeroOfV)
    /** Return index of first non-zero coefficient. When none exists, returns -1. */
    def firstNonZero(x: FDif[V]): Int = x.indexWhere(xk => !(evVIsIntegral isZero xk))
  }
  
  /** Real instance for FDif[V], where V itself has a Real instance */
  abstract class FDifAsReal[V: Real] extends FDifAsIntegral[V] with Real[FDif[V]] {
    def fromDouble(x: Double): FDif[V] = FDif.constant(evVIsReal fromDouble x)
    /* Constants */
    val evVIsReal = implicitly[Real[V]]
    /* Real instance */
    def div(l: FDif[V], r: FDif[V]): FDif[V] = 
      FDif(Stream.from(0).map(k => (l(k)*r(0) - l(0)*r(k)) / r(0).square), combinedLength(l,r))
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