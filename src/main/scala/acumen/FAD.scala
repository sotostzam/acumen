package acumen

import interpreters.Common.{
  Field, RichStore
}
import interpreters.enclosure.Interval
import util.ASTUtil.mapValue
import acumen.util.Conversions

object FAD extends App {
  
  /** Representation of a number and its derivatives. 
   *  NOTE: An FDif with an empty coeff represents a constant value.
   */
  case class FDif[V: Integral](head: V, coeff: Map[QName,V]) extends Dif[V,QName] {
    def apply(i: QName) = if (coeff isEmpty) implicitly[Integral[V]].zero else coeff(i)
    val length = coeff.size + 1
    def map[W: Integral](m: V => W): FDif[W] = FDif(m(head), coeff mapValues m)
  }
  object FDif {
    /** Lift a constant value of type A to an FDif. */
    def constant[V: Integral](a: V) = FDif(a, Map.empty)
  }

  /** Lift all numeric values in a store into FDifs */
  def lift[Id, S <% RichStore[S, Id], I: Integral](st: S, odeVariables: List[QName]): S =
    st mapName { case (id,n,v) => liftValue[Id, I](v, QName(id,n), odeVariables) }
  
  /** Lift all numeric values in an Expr into FDifs */
  def lift[Id, V: Integral](e: Expr, ns: List[QName]): Expr = new acumen.util.ASTMap {
    override def mapExpr(e: Expr): Expr = (e match {
      case Lit(gv) => Lit(liftGroundValue(gv, None, ns))
      case _ => super.mapExpr(e)
    }).setPos(e.pos)
  }.mapExpr(e)

  private def liftGroundValue(gv: GroundValue, n: Option[QName], ns: List[QName]): StaticGroundValue = {
    def zeroOrOne[A](qn: QName, zero: A, one: A): A = if (n.isDefined && qn == n.get) one else zero
    gv match {
      case GDouble(d)   => 
        GDoubleFDif(FDif(d,          ns.map(id => id -> zeroOrOne(id, 0d, 1d)).toMap))
      case GInt(i)      => 
        GDoubleFDif(FDif(i.toDouble, ns.map(id => id -> zeroOrOne(id, 0d, 1d)).toMap))
      case GInterval(i) => 
        GIntervalFDif(FDif(i,        ns.map(id => id -> zeroOrOne(id, Interval.zero, Interval.one)).toMap))
      case GConstantRealEnclosure(i) => 
        GIntervalFDif(FDif(i,        ns.map(id => id -> zeroOrOne(id, Interval.zero, Interval.one)).toMap))
      case gv: StaticGroundValue     => gv
    }
  }
  
  /** Lift all the values inside a Value[Id] into FDifs */
  private def liftValue[Id, I: Integral](v: Value[Id], n: QName, ns: List[QName]): Value[Id] = v match {
    case VLit(gv) => VLit(liftGroundValue(gv, Some(n), ns))
    case VVector(lv: List[Value[Id]]) => VVector(lv map (liftValue[Id, I](_, n, ns)))
    case VList(lv: List[Value[Id]]) => VList(lv map (liftValue[Id, I](_, n, ns)))
    case _ => v
  }
  
  /** Integral instance for FDif[V], where V itself has an Integral instance */
  abstract class FDifAsIntegral[V: Integral] extends DifAsIntegral[V,QName,FDif[V]] with Integral[FDif[V]] {
    def binaryOp(op: String, l: FDif[V], r: FDif[V], h: (V,V) => V, c: QName => V): FDif[V] = {
      require(l.coeff.isEmpty || r.coeff.isEmpty || l.coeff.keySet == r.coeff.keySet, 
        s"Cannot $op FDifs with different sets of names.")
      FDif(h(l.head, r.head), (l.coeff.keySet union r.coeff.keySet).map(k => k -> c(k)).toMap)
    }
    /* Integral instance */
    def abs(x: FDif[V]): FDif[V] = throw Errors.NotImplemented("FDif.abs")
    def add(l: FDif[V], r: FDif[V]): FDif[V] =
      binaryOp("add", l, r, _ + _, k => l(k) + r(k))
    def sub(l: FDif[V], r: FDif[V]): FDif[V] =
      binaryOp("subtract", l, r, _ - _, k => l(k) - r(k))
    def neg(x: FDif[V]): FDif[V] =
      FDif(- x.head, x.coeff.mapValues(-_))
    def mul(l: FDif[V], r: FDif[V]): FDif[V] =
      binaryOp("multiply", l, r, _ * _, k => l(k) * r.head + l.head * r(k))
    def fromInt(x: Int): FDif[V] = FDif.constant(evVIsIntegral fromInt x)
    def zero: FDif[V] = FDif constant zeroOfV
    def one: FDif[V] = FDif constant oneOfV
    def isConstant(x: FDif[V]): Boolean = x.coeff.values.forall(_ == zeroOfV)
    def ==(a: FDif[V], b: FDif[V]): Boolean = {
      require(a.coeff.keys == b.coeff.keys, "Cannot compare FDifs with different sets of names.")
      a.coeff.keys.forall(i => a(i) == b(i))
    }
    def !=(a: FDif[V], b: FDif[V]): Boolean = {
      require(a.coeff.keys == b.coeff.keys, "Cannot compare FDifs with different sets of names.")
      a.coeff.keys.exists(i => a(i) != b(i))
    }
  }
  
  /** Real instance for FDif[V], where V itself has a Real instance */
  abstract class FDifAsReal[V: Real] extends FDifAsIntegral[V] with Real[FDif[V]] {
    def fromDouble(x: Double): FDif[V] = FDif.constant(evVIsReal fromDouble x)

    /* Constants */
    val evVIsReal = implicitly[Real[V]]

    /* Real instance */
    def div(l: FDif[V], r: FDif[V]): FDif[V] = {
      require(!r.head.isZero, "Division by zero is not allowed.")
      binaryOp("add", l, r, _/_, k => (l(k)*r.head - l.head*r(k)) / r.head.square)
    }
    
    /* Power functions */
    def pow(l: FDif[V], r: FDif[V]): FDif[V] = {
      if (isConstant(r))
        if (isConstant(l))
          powOfRealOnReal(l.head, r.head)
        else
          powOnReal(l, r.head)
      else if (isConstant(l))
        powOfReal(l.head, r)
      else
        exp(mul(r, log(l)))
    }
    def pow(l: FDif[V], r: Int): FDif[V] =
      pow(l, evVIsReal.fromInt(r))
    def pow(l: FDif[V], r: V): FDif[V] = {
      if (isConstant(l))
        powOfRealOnReal(l.head, r)
      else
        powOnReal(l, r)
    }
    def pow(l: V, r: FDif[V]): FDif[V] = {
      if (isConstant(r))
        powOfRealOnReal(l, r.head)
      else
        powOfReal(l, r)
    }
    def powOnReal(l: FDif[V], r: V) : FDif[V] = {
      if (r == oneOfV)
        l
      else if (r == zeroOfV) {
        require(!(l.head == zeroOfV), s"pow is not applicable to ($l,$r) as 0^0 is not defined.")
        one }
      else {
        val tmp = r * (l.head ^ (r - oneOfV))
        FDif(l.head ^ r, l.coeff.mapValues(_ * tmp))
      }
    }
    def powOfReal(l: V, r: FDif[V]) : FDif[V] = {
      if (l == oneOfV)
        one
      else if (l == zeroOfV) {
        require(!(r.head == zeroOfV), s"pow is not applicable to ($l,$r) as 0^0 is not defined.")
        zero }
      else {
        val lonr = l ^ r.head
        val tmp = lonr * l.log
        FDif(lonr, r.coeff.mapValues(_ * tmp))
      }
    }
    def powOfRealOnReal(l: V, r: V) : FDif[V] = {
      FDif constant (l ^ r)
    }

    /* Square and Square root */
    def square(x: FDif[V]): FDif[V] = {
      val twox = evVIsIntegral.fromInt(2) * x.head
      FDif(x.head.square, x.coeff.mapValues(_ * twox))
    }
    def sqrt(x: FDif[V]): FDif[V] = {
      if (isZero(x)) zero else {
        require(!x.head.isZero, "Square root of zero (non-constant) is not allowed.")
        val sqrtx = x.head.sqrt
        FDif(sqrtx, x.coeff.mapValues(_ / (evVIsIntegral.fromInt(2) * sqrtx)))
      }
    }

    /* Exponential functions */
    def exp(x: FDif[V]): FDif[V] = {
      val expx = x.head.exp
      FDif(expx, x.coeff.mapValues(_ * expx))
    }
    def log(x: FDif[V]): FDif[V] = {
      require(!x.head.isZero, "Logarithm of zero is not allowed.")
      FDif(x.head.log, x.coeff.mapValues(_ / x.head))
    }

    /* Trigonometric functions */
    def sin(x: FDif[V]): FDif[V] = {
      val cosx = x.head.cos      
      FDif(x.head.sin, x.coeff.mapValues(_ * cosx))
    } 
    def cos(x: FDif[V]): FDif[V] = {
      val minsinx = - x.head.sin     
      FDif(x.head.cos, x.coeff.mapValues(_ * minsinx))
    } 
    def tan(x: FDif[V]): FDif[V] = {
      val tan2p1 = oneOfV + (x.head.tan).square
      FDif(x.head.tan, x.coeff.mapValues(_ * tan2p1))
    }
    def acos(x: FDif[V]): FDif[V] = {
      val tmp = - oneOfV / (oneOfV - x.head.square).sqrt
      FDif(x.head.acos, x.coeff.mapValues(_ * tmp))
    }
    def asin(x: FDif[V]): FDif[V] = {
      val tmp = oneOfV / (oneOfV - x.head.square).sqrt
      FDif(x.head.asin, x.coeff.mapValues(_ * tmp))
    }
    def atan(x: FDif[V]): FDif[V] = {
      val tmp = oneOfV / (oneOfV + x.head.square)
      FDif(x.head.atan, x.coeff.mapValues(_ * tmp))
    }
  }

  implicit object intFDifIsIntegral extends FDifAsIntegral[Int] {
    def groundValue(v: FDif[Int]) = GIntFDif(v)
  }
  implicit object doubleFDifIsReal extends FDifAsReal[Double] {
    def groundValue(v: FDif[Double]) = GDoubleFDif(v)
  }
  implicit object intervalFDifIsReal extends FDifAsReal[Interval] {
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