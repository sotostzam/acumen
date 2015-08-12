package acumen

import interpreters.enclosure.Interval
import util.ASTUtil
import acumen.interpreters.Common.RichStore

/**
 * Automatic Differentiation.
 *
 * Based on description from book "Validated Numerics" by
 * Warwick Tucker, pages 121 and 122
 * 
 * and on the note
 * "Automatic Differentiation Rules for Univariate Taylor Series" by
 * Ferenc A. Bartha
 */
object TAD extends App {

  /** Integral instance for TDif[V], where V itself has an Integral instance */
  abstract class TDifAsIntegral[V: Integral] extends DifAsIntegral[V,TDif[V]] with Integral[TDif[V]] {
    override def dif(v: Seq[V]): TDif[V] = TDif(v)
    /* Caches */
    val mulCache = collection.mutable.HashMap[(TDif[V], TDif[V]), TDif[V]]()
    /* Integral instance */
    def mul(l: TDif[V], r: TDif[V]): TDif[V] =
      mulCache.getOrElseUpdate((l, r),
        TDif((for (k <- 0 until l.size) yield ((0 to k).foldLeft(zeroOfV) {
          case (sum, i) => sum + (l(i) * r(k - i))
        })).toVector))
    def fromInt(x: Int): TDif[V] = TDif.constant(evVIsIntegral fromInt x)
    lazy val zero: TDif[V] = TDif.fill(zeroOfV)
    lazy val one: TDif[V] = TDif.constant(oneOfV)
    /** Return index of first non-zero coefficient. When none exists, returns -1. */
    def firstNonZero(x: TDif[V]): Int = x.coeff.indexWhere(_ != zeroOfV)
  }
  
  /** Real instance for TDif[V], where V itself has a Real instance */
  abstract class TDifAsReal[V: Real] extends TDifAsIntegral[V] with Real[TDif[V]] {
    def fromDouble(x: Double): TDif[V] = TDif.constant(evVIsReal fromDouble x)
    override def isValidInt(x: TDif[V]): Boolean = x.coeff.head.isValidInt && isConstant(x)
    /* Caches */
    val divCache = collection.mutable.HashMap[(TDif[V], TDif[V]), TDif[V]]()
    val powCache = collection.mutable.HashMap[(TDif[V], TDif[V]), TDif[V]]()    
    val sinAndCosCache = collection.mutable.HashMap[TDif[V], (/*sin*/TDif[V], /*cos*/TDif[V])]()
    val tanCache = collection.mutable.HashMap[TDif[V], TDif[V]]()
    val acosCache = collection.mutable.HashMap[TDif[V], TDif[V]]()
    val asinCache = collection.mutable.HashMap[TDif[V], TDif[V]]()
    val atanCache = collection.mutable.HashMap[TDif[V], TDif[V]]()
    val expCache = collection.mutable.HashMap[TDif[V], TDif[V]]()
    val logCache = collection.mutable.HashMap[TDif[V], TDif[V]]()
    val squareCache = collection.mutable.HashMap[TDif[V], TDif[V]]()
    val sqrtCache = collection.mutable.HashMap[TDif[V], TDif[V]]()
    /* Constants */
    val evVIsReal = implicitly[Real[V]]
    
    /* Real instance */
    def div(l: TDif[V], r: TDif[V]): TDif[V] =
      divCache.getOrElseUpdate((l, r), TDif {
        val n = l.size
        val k0 = firstNonZero(r)
        val l0 = firstNonZero(l)
        require((k0 > -1), "Division by zero is not allowed.")
        require((k0 <= l0 || l0 == -1), s"First non-vanishing coefficient of $r must not be higher order than the first non-vanishing coefficient of $l.")
        val coeff = new collection.mutable.ArraySeq[V](n)
        coeff(0) = l(k0) / r(k0)
        for (k <- 1 to n - k0 - 1)
          coeff(k) = (l(k + k0) - (0 to k - 1).foldLeft(zeroOfV) {
            case (sum, i) => sum + (coeff(i) * r(k - i + k0))
          }) / r(k0)
        for (k <- n - k0 to n - 1)
          coeff(k) = - (k - (n - 1) + k0 to k - 1).foldLeft(zeroOfV) {
            case (sum, i) => sum + (coeff(i) * r(k - i + k0))
          } / r(k0)
        coeff.toVector
      })
      
    /* Power functions */
    /** General interface for power */
    def pow(l: TDif[V], r: TDif[V]): TDif[V] =  
      powCache.getOrElseUpdate((l, r), {
        // r is constant
        if (isConstant(r))
          if (r(0) == oneOfV)    l        else
          if (r(0) == zeroOfV) { require(!(l == zero), s"pow is not applicable to ($l,$r) as 0^0 is not defined.")
                                 one }
          else                   powOnReal(l, r)
        // r is not constant   
        else 
          if (l == zero)         { require(!(r(0) == zeroOfV), s"pow is not applicable to ($l,$r) as 0^0 is not defined.")
                                 zero }
          else                   exp(mul(r, log(l)))
      })
      
    /** Square root */
    def sqrt(x: TDif[V]): TDif[V] =
      sqrtCache.getOrElseUpdate(x, if (x == zero) zero else TDif { // We might call sqrt directly not only through power, so we check for zero
        val n  = x.size
        val k0  = firstNonZero(x) // n >= k0 because x != zero
        require((k0 % 2 == 0), s"First non-vanishing coefficient must be an even power in $x in order to expand the sqrt function.")
        val x0 = x(k0)
        val k0d2 = k0 / 2
        val coeff = new collection.mutable.ArraySeq[V](n)
        for (k <- 0 to k0d2 - 1) coeff(k) = zeroOfV        // the first k0/2 coefficients are zero
        coeff(k0d2) = x0.sqrt                              // the first non-zero coefficient of the result
        for (k <- 1 + k0 to n - 1 + k0d2) {                // ----------------------------------------------
          val kEnd = (k + k%2 - 2) / 2
                                                           // possibly  non-zero coefficients k0/2 + 1 .. n - 1
          coeff(k - k0d2) = ((if (k < n) x(k) else zeroOfV) - evVIsIntegral.fromInt(2) * ((k0d2 + 1 to kEnd).foldLeft(zeroOfV) { 
            case (sum, i) => sum + coeff(i) * coeff(k - i)  
          }) + (if (k % 2 == 0) - coeff(k / 2).square else zeroOfV )) / (evVIsIntegral.fromInt(2) * coeff(k0d2)) // FIXME optimize substraction of zeroOfV
        } 
        coeff.toVector        
      })
    
    /** Square */
    def square(x: TDif[V]): TDif[V] =
      squareCache.getOrElseUpdate(x, TDif {
        val n = x.size
        val coeff = new collection.mutable.ArraySeq[V](n)
        coeff(0) = x(0).square
        for (k <- 1 until n) {
          val kEnd = (k + k%2 - 2)/2
          // 2 x(i)x(j), i != j
          coeff(k) = evVIsIntegral.fromInt(2) * ((0 to kEnd).foldLeft(zeroOfV) {
            case (sum, i) => sum + x(i) * x(k - i)
          // x(k/2)^2 if k was even
          }) + (if (k % 2 == 0) x(k/2).square else zeroOfV) // FIXME should adding zeroOfV optimized? 
        }
        coeff.toVector
      })
    
    /** Integer power by squaring */
    private def powBySquare(l: TDif[V], n: Int): TDif[V] =
      mul(square(powOnInt(l, n/2)), (if (n % 2 == 1) l else one)) // FIXME should multiplying with one be optimized? 
    
    /** Power with integer exponent */
    private def powOnInt(l: TDif[V], n: Int) : TDif[V] = {
        if (n == 0) one                       else 
        if (n == 1) l                         else
        if (n == 2) square(l)                 else
        if (n <  0) div(one, powOnInt(l, -n)) else
        powBySquare(l, n)
    }
    
    /** Power with real exponent
     *  l != zero, r != zero, r != one */
    private def powOnReal(l: TDif[V], r: TDif[V]) : TDif[V] = {
      if (r(0).isValidInt) powOnInt(l, r(0).toInt) else {
        TDif {
          val n  = l.size
          val k0  = firstNonZero(l) // n >= k0 because l != zero
          val lk0 = l(k0)
          val a = r(0) // FIXME This is a constant! Should not be lifted in the first place.
          val k0V = a * evVIsIntegral.fromInt(k0)
          require((k0 == 0 || k0V.isValidInt), s"pow is not applicable to ($l,$r). Expanding around 0 needs the product of the exponent and of the index of the first non-zero coefficient to be an integer.")
          val ak0 = k0V.toInt
          val coeff = new collection.mutable.ArraySeq[V](n)
          for (k <- 0 to ak0 - 1) coeff(k) = zeroOfV         // the first a * k0 coefficients are zero
          coeff(ak0) = lk0 ^ a                               // the first non-zero coefficient of the result
          for (k <- ak0 + 1 to n - 1) {                      // ----------------------------------------------
            val kL = evVIsIntegral.fromInt(k)
            val ak0L = evVIsIntegral.fromInt(ak0)            // possibly  non-zero coefficients a * k0 + 1 .. n - 1
            coeff(k) = ((1 to Math.min(k, n - 1 - k0)).foldLeft(zeroOfV) { 
              case (sum, i) =>  
                val iL = evVIsIntegral.fromInt(i)
                sum + ( (a + oneOfV) * iL / (kL - ak0L) - oneOfV ) * l(i + k0) * coeff(k - i)  
            }) /  lk0
          } 
          coeff.toVector
        }
      }
    }
    
    /** Exponential function */
    def exp(x: TDif[V]): TDif[V] =
      expCache.getOrElseUpdate(x, TDif {
        val n = x.size
        val coeff = new collection.mutable.ArraySeq[V](n)
        coeff(0) = x(0).exp
        for (k <- 1 until n)
          coeff(k) = ((1 to k).foldLeft(zeroOfV) {
            case (sum, i) => sum + evVIsIntegral.fromInt(i) * x(i) * coeff(k-i)
          }) / evVIsIntegral.fromInt(k)
        coeff.toVector
      })
    
    /** Natural logarithm */
    def log(x: TDif[V]): TDif[V] =
      logCache.getOrElseUpdate(x, TDif {
        require(firstNonZero(x) == 0, "Not possible to expand log around 0.")
        val n = x.size
        val coeff = new collection.mutable.ArraySeq[V](n)
        val x0 = x(0)
        coeff(0) = x0.log
        for (k <- 1 until n) {
          coeff(k) = (x(k) - ((1 to k - 1).foldLeft(zeroOfV) {
            case (sum, i) => sum + evVIsIntegral.fromInt(i) * coeff(i) * x(k-i)
          }) / evVIsIntegral.fromInt(k)) / x0
        }
        coeff.toVector
      })
    
    /* Trigonometric functions */
    def sin(x: TDif[V]): TDif[V] = sinAndCos(x)._1
    def cos(x: TDif[V]): TDif[V] = sinAndCos(x)._2
    private def sinAndCos(x: TDif[V]): (TDif[V],TDif[V]) =
      sinAndCosCache.getOrElseUpdate(x,{
        val n = x.size
        val sinCoeff = new collection.mutable.ArraySeq[V](n)
        val cosCoeff = new collection.mutable.ArraySeq[V](n)
        sinCoeff(0) = x(0).sin
        cosCoeff(0) = x(0).cos
        for (k <- 1 until n) {
          val (sck, cck) = (1 to k).foldLeft(zeroOfV, zeroOfV) {
            case ((sckSum, cckSum), i) => 
              val ixi = evVIsIntegral.fromInt(i) * x(i)
              ( sckSum + ixi * cosCoeff(k - i)
              , cckSum + ixi * sinCoeff(k - i) )
          }
          val kL = evVIsIntegral.fromInt(k)
          sinCoeff(k) =  sck / kL
          cosCoeff(k) = -cck / kL
        }
        (TDif(sinCoeff.toVector), TDif(cosCoeff.toVector))
      })
    def tan(x: TDif[V]): TDif[V] = 
      tanCache.getOrElseUpdate(x, TDif {
        val n = x.size
        val coeff = new collection.mutable.ArraySeq[V](n)
        coeff(0) = x(0).tan
        val cos2 = square(cos(x))
        for (k <- 1 until n) {
          coeff(k) = (x(k) - ((1 to k-1).foldLeft(zeroOfV) {
            case (sum, i) => 
              sum + evVIsIntegral.fromInt(i) * coeff(i) * cos2(k - i)
          }) / evVIsIntegral.fromInt(k)) / (x(0).cos).square
        }
        coeff.toVector
      })
    def acos(x: TDif[V]): TDif[V] =
      acosCache.getOrElseUpdate(x, TDif{
        val n = x.size
        val coeff = new collection.mutable.ArraySeq[V](n)
        coeff(0) = x(0).acos
        val c = sqrt(sub(one, square(x)))
        for (k <- 1 until n) {
          coeff(k) = - (x(k) + ((1 to k-1).foldLeft(zeroOfV) {
            case (sum, i) => 
              sum + evVIsIntegral.fromInt(i) * coeff(i) * c(k - i)
          }) / evVIsIntegral.fromInt(k)) / (oneOfV - x(0).square).sqrt
        }
        coeff.toVector
      })
    def asin(x: TDif[V]): TDif[V] =
      asinCache.getOrElseUpdate(x, TDif{
        val n = x.size
        val coeff = new collection.mutable.ArraySeq[V](n)
        coeff(0) = x(0).asin
        val c = sqrt(sub(one, square(x)))
        for (k <- 1 until n) {
          coeff(k) = (x(k) - ((1 to k-1).foldLeft(zeroOfV) {
            case (sum, i) => 
              sum + evVIsIntegral.fromInt(i) * coeff(i) * c(k - i)
          }) / evVIsIntegral.fromInt(k)) / (oneOfV - x(0).square).sqrt
        }
        coeff.toVector
      })
    def atan(x: TDif[V]): TDif[V] =
      atanCache.getOrElseUpdate(x, TDif{
        val n = x.size
        val coeff = new collection.mutable.ArraySeq[V](n)
        coeff(0) = x(0).atan
        val c = add(one, square(x))
        for (k <- 1 until n) {
          coeff(k) = (x(k) - ((1 to k-1).foldLeft(zeroOfV) {
            case (sum, i) => 
              sum + evVIsIntegral.fromInt(i) * coeff(i) * c(k - i)
          }) / evVIsIntegral.fromInt(k)) / (oneOfV + x(0).square)
        }
        coeff.toVector
      })
  }
  implicit object IntDifIsIntegral extends TDifAsIntegral[Int] {
    def groundValue(v: TDif[Int]) = GIntDif(v)
  }
  implicit object DoubleDifIsReal extends TDifAsReal[Double] {
    def groundValue(v: TDif[Double]) = GDoubleDif(v)
  }
  implicit object IntervalDifIsReal extends TDifAsReal[Interval] {
    def groundValue(v: TDif[Interval]) = GIntervalDif(v)
  }
  
  /** Representation of a number and its time derivatives. */
  case class TDif[V](coeff: Seq[V]) extends Dif[V]
  object TDif {
    val dim: Int = 10 // FIXME Expose this as a simulator parameter
    /** Lift a constant value of type A to a TDif. */
    def constant[V: Integral](a: V) = padWithZeros(Vector(a, implicitly[Integral[V]].zero))
    /** Lift the variable (that we differentiate w.r. to) of type V to a TDif. The value a is the current value of the variable. */
    def variable[V: Integral](a: V) = padWithZeros(Vector(a, implicitly[Integral[V]].one))
    /** Use a sequence of values of type V as the coefficients of a TDif.
     *  The sequence is padded to dim with the zero of V. */
    def apply[V: Integral](as: V*) = padWithZeros(as.toVector)
    /** Use a repeated sequence of the value a as the coefficients of a TDif. */
    def fill[V: Integral](a: V) = new TDif(Vector.fill(dim)(a))
    /** Create a TDif by filling the missing positions up to dim with the zero of V. */
    private def padWithZeros[V: Integral](v: Vector[V]): TDif[V] = TDif(v.padTo(dim, implicitly[Integral[V]].zero))
  }
  
  /** Lift all numeric values in a store into TDifs */
  def lift[S,Id](st: RichStore[S,Id]): S = st map liftValue 
  
  /** Lift all numeric values in an Expr into TDifs */
  def lift(e: Expr): Expr = new acumen.util.ASTMap {
    override def mapExpr(e: Expr): Expr = (e match {
      case Lit(gv) => Lit(liftGroundValue(gv))
      case _ => super.mapExpr(e)
    }).setPos(e.pos)
  }.mapExpr(e)
  
  private def liftGroundValue(gv: GroundValue): GroundValue = gv match {
    case GDouble(d) => GDoubleDif(TDif constant d)
    case GInt(i) => GDoubleDif(TDif constant i)
    case _ => gv
  }
  
  /** Lift all the values inside a Value[Id] into TDifs */
  private def liftValue[Id](v: Value[Id]): Value[Id] = v match {
    case VLit(gv) => VLit(liftGroundValue(gv))
    case VVector(lv: List[Value[Id]]) => VVector(lv map liftValue)
    case VList(lv: List[Value[Id]]) => VList(lv map liftValue)
    case _ => v
  }
  
  /** Lower all TDif values in a RichStore into the corresponding numeric value */
  def lower[S,Id](st: RichStore[S,Id]): S = st map lowerValue
  
  /** Lower all TDif values in an Expr into the corresponding numeric value */
  def lower(e: Expr): Expr = new acumen.util.ASTMap {
    override def mapExpr(e: Expr): Expr = (e match {
      case Lit(gv) => Lit(lowerGroundValue(gv))
      case _ => super.mapExpr(e)
    }).setPos(e.pos)
  }.mapExpr(e)
  
  private def lowerGroundValue(gd: GroundValue): GroundValue = gd match {
    case GIntDif(TDif(v)) => GInt(v(0))
    case GDoubleDif(TDif(v)) => val v0 = v(0)
      if (DoubleIsReal isValidInt v0) GInt(DoubleIsReal toInt v0) else GDouble(v0)
    case _ => gd
  }
  
  /** Lower all the values inside a Value[Id] from TDifs */
  private def lowerValue[Id](v: Value[Id]): Value[Id] = v match {
    case VLit(gv) => VLit(lowerGroundValue(gv))
    case VVector(lv: List[Value[Id]]) => VVector(lv map lowerValue)
    case VList(lv: List[Value[Id]]) => VList(lv map lowerValue)
    case _ => v
  }
  

}
