package acumen

import interpreters.enclosure.Interval

import util.ASTUtil

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
object AD extends App {

  // FIXME Should this really be a partial ordering? 
  //       Do the properties hold for intervals?
  /**
   * Type class with operations that have sensible
   * implementations for Int as well as all the
   * other numeric types.
   */
  trait Integral[V] extends PartialOrdering[V] {
    def add(l: V, r: V): V
    def sub(l: V, r: V): V
    def mul(l: V, r: V): V
    def neg(x: V): V
    def fromInt(i: Int): V
    def toInt(x: V): Int
    def toDouble(x: V): Double
    def zero: V
    def one: V
    def isValidInt(x: V): Boolean
    def isValidDouble(x: V): Boolean
  }

  /**
   * Type class with operations that have sensible
   * implementations for numeric types that represent
   * real numbers. 
   */
  trait Real[V] extends Integral[V] {
    def div(l: V, r: V): V
    def pow(l: V, r: V): V
    def sin(x: V): V
    def cos(x: V): V
    def tan(x: V): V
    def acos(x: V): V
    def asin(x: V): V
    def atan(x: V): V
    def exp(x: V): V
    def log(x: V): V
    def square(x: V): V
    def sqrt(x: V): V
    def fromDouble(i: Double): V
  }
  
  /** Syntactic sugar for Integral operations */
  implicit class IntegralOps[V](val l: V)(implicit ev: Integral[V]) {
    def +(r: V): V = ev.add(l, r)
    def -(r: V): V = ev.sub(l, r)
    def *(r: V): V = ev.mul(l, r)
    def unary_- = ev.neg(l)
    def < (r: V): Boolean = ev.lt(l, r)
    def > (r: V): Boolean = ev.gt(l, r)
    def <= (r: V): Boolean = ev.lteq(l, r)
    def >= (r: V): Boolean = ev.gteq(l, r)
    def zero: V = ev.zero
    def one: V = ev.one
    def isValidInt: Boolean = ev.isValidInt(l)
    def toInt: Int = ev.toInt(l)
    def isValidDouble: Boolean = ev.isValidDouble(l)
    def toDouble: Double = ev.toDouble(l)
  }

  /** Syntactic sugar for Real operations */
  implicit class RealOps[V](val l: V)(implicit ev: Real[V]) {
    def /(r: V): V = ev.div(l, r)
    def ^(r: V): V = ev.pow(l, r)
    def sin: V = ev.sin(l)
    def cos: V = ev.cos(l)
    def tan: V = ev.tan(l)
    def acos: V = ev.acos(l)
    def asin: V = ev.asin(l)
    def atan: V = ev.atan(l)
    def exp: V = ev.exp(l)
    def log: V = ev.log(l)
    def square: V = ev.square(l)
    def sqrt: V = ev.sqrt(l)
  }

  /** Integral instance for Int */
  implicit object IntIsIntegral extends Integral[Int] {
    def add(l: Int, r: Int): Int = l + r
    def sub(l: Int, r: Int): Int = l - r
    def mul(l: Int, r: Int): Int = l * r
    def neg(x: Int): Int = -x
    def fromInt(x: Int): Int = x
    def zero: Int = 0
    def one: Int = 1
    def tryCompare(l: Int, r: Int): Option[Int] = Some(l compareTo r)
    def lteq(l: Int, r: Int): Boolean = l <= r
    def isValidInt(x: Int): Boolean = true
    def toInt(x: Int): Int = x
    def isValidDouble(x: Int): Boolean = true
    def toDouble(x: Int): Double = x.toDouble
  }

  /** Real instance for Double */
  implicit object DoubleIsReal extends Real[Double] {
    def add(l: Double, r: Double): Double = l + r
    def sub(l: Double, r: Double): Double = l - r
    def mul(l: Double, r: Double): Double = l * r
    def div(l: Double, r: Double): Double = l / r
    def pow(l: Double, r: Double): Double = Math.pow(l, r)
    def neg(x: Double): Double = -x
    def sin(x: Double): Double = Math.sin(x)
    def cos(x: Double): Double = Math.cos(x)
    def tan(x: Double): Double = Math.tan(x)
    def acos(x: Double): Double = Math.acos(x)
    def asin(x: Double): Double = Math.asin(x)
    def atan(x: Double): Double = Math.atan(x)
    def exp(x: Double): Double = Math.exp(x)
    def log(x: Double): Double = Math.log(x)
    def square(x: Double): Double = x * x
    def sqrt(x: Double): Double = Math.sqrt(x)
    def fromInt(x: Int): Double = x
    def fromDouble(x: Double): Double = x
    def zero: Double = 0
    def one: Double = 1
    def tryCompare(l: Double, r: Double): Option[Int] = Some(l compareTo r)
    def lteq(l: Double, r: Double): Boolean = l <= r
    def isValidInt(x: Double): Boolean = (Math.floor(x) == Math.ceil(x))
    def toInt(x: Double): Int = x.toInt
    def isValidDouble(x: Double): Boolean = true
    def toDouble(x: Double): Double = x
  }

  /** Real instance for Interval */
  implicit object IntervalIsReal extends Real[Interval] {
    def add(l: Interval, r: Interval): Interval = l + r
    def sub(l: Interval, r: Interval): Interval = l - r
    def mul(l: Interval, r: Interval): Interval = l * r
    def div(l: Interval, r: Interval): Interval = l / r
    def pow(l: Interval, r: Interval): Interval = l pow r
    def neg(x: Interval): Interval = -x
    def sin(x: Interval): Interval = x.sin
    def cos(x: Interval): Interval = x.cos
    def tan(x: Interval): Interval = x.tan
    def acos(x: Interval): Interval = x.acos
    def asin(x: Interval): Interval = x.asin
    def atan(x: Interval): Interval = x.atan
    def exp(x: Interval): Interval = x.exp
    def log(x: Interval): Interval = x.log
    def square(x: Interval): Interval = x.square
    def sqrt(x: Interval): Interval = x.sqrt
    def fromInt(x: Int): Interval = Interval(x)
    def fromDouble(x: Double): Interval = Interval(x)
    def zero: Interval = Interval.zero
    def one: Interval = Interval.one
    def isValidInt(x: Interval): Boolean = ((x.lo).doubleValueFloor == (x.hi).doubleValueCeiling) // FIXME Check if this is ok
    def toInt(x: Interval): Int = ((x.lo).doubleValueFloor).toInt // FIXME Check if this is ok
    def isValidDouble(x: Interval): Boolean = (x.lo == x.hi)
    def toDouble(x: Interval): Double = (x.lo).doubleValueFloor
    // FIXME Check if something needs to be overridden
    def lteq(l: Interval, r: Interval): Boolean = l lessThanOrEqualTo r
    def tryCompare(l: Interval, r: Interval): Option[Int] =
      if (l == r)         Some(0)
      else if (lteq(l,r)) Some(1) 
      else if (lteq(r,l)) Some(-1)
      else                None
  }

  /** Integral instance for Dif[V], where V itself has an Integral instance */
  class DifAsIntegral[V: Integral] extends Integral[Dif[V]] {
    /* Caches */
    val mulCache = collection.mutable.HashMap[(Dif[V], Dif[V]), Dif[V]]()
    /* Constants */
    val evVIsIntegral = implicitly[Integral[V]]
    val zeroOfV = evVIsIntegral.zero
    val oneOfV = evVIsIntegral.one
    /* Integral instance */
    def add(l: Dif[V], r: Dif[V]): Dif[V] = Dif((l.coeff, r.coeff).zipped.map(_ + _))
    def sub(l: Dif[V], r: Dif[V]): Dif[V] = Dif((l.coeff, r.coeff).zipped.map(_ - _))
    def mul(l: Dif[V], r: Dif[V]): Dif[V] =
      // FIXME Implement using mutable data, but keep the immutable interface
      mulCache.getOrElseUpdate((l, r),
        Dif((for (k <- 0 until l.size) yield ((0 to k).foldLeft(zeroOfV) {
          case (sum, i) => sum + (l(i) * r(k - i))
        })).toVector))
    def neg(x: Dif[V]): Dif[V] = Dif(x.coeff.map(- _))
    def fromInt(x: Int): Dif[V] = Dif.constant(evVIsIntegral fromInt x)
    // FIXME Test these definitions
    lazy val zero: Dif[V] = Dif.fill(zeroOfV)
    lazy val one: Dif[V] = Dif.constant(oneOfV)
    def toInt(x: Dif[V]): Int = x(0).toInt
    def toDouble(x: Dif[V]): Double = x(0).toDouble
    def isValidInt(x: Dif[V]): Boolean = x(0).isValidInt && isConstant(x)
    def isValidDouble(x: Dif[V]): Boolean = x(0).isValidDouble && isConstant(x)
    def isConstant(x: Dif[V]): Boolean = x.coeff.tail.forall(_ == zeroOfV)
    /** Return index of first non-zero coefficient. When none exists, returns -1. */
    def firstNonZero(x: Dif[V]): Int = x.coeff.indexWhere(_ != zeroOfV)
    def tryCompare(l: Dif[V], r: Dif[V]): Option[Int] = evVIsIntegral.tryCompare(l(0), r(0))
    def lteq(l: Dif[V], r: Dif[V]): Boolean = evVIsIntegral.lteq(l(0), r(0))
  }
  
  /** Real instance for Dif[V], where V itself has a Real instance */
  class DifAsReal[V: Real] extends DifAsIntegral[V] with Real[Dif[V]] {
    def fromDouble(x: Double): Dif[V] = Dif.constant(evVIsReal fromDouble x)
    override def isValidInt(x: Dif[V]): Boolean = ???
    /* Caches */
    val divCache = collection.mutable.HashMap[(Dif[V], Dif[V]), Dif[V]]()
    val powCache = collection.mutable.HashMap[(Dif[V], Dif[V]), Dif[V]]()    
    val sinAndCosCache = collection.mutable.HashMap[Dif[V], (/*sin*/Dif[V], /*cos*/Dif[V])]()
    val tanCache = collection.mutable.HashMap[Dif[V], Dif[V]]()
    val acosCache = collection.mutable.HashMap[Dif[V], Dif[V]]()
    val asinCache = collection.mutable.HashMap[Dif[V], Dif[V]]()
    val atanCache = collection.mutable.HashMap[Dif[V], Dif[V]]()
    val expCache = collection.mutable.HashMap[Dif[V], Dif[V]]()
    val logCache = collection.mutable.HashMap[Dif[V], Dif[V]]()
    val squareCache = collection.mutable.HashMap[Dif[V], Dif[V]]()
    val sqrtCache = collection.mutable.HashMap[Dif[V], Dif[V]]()
    /* Constants */
    val evVIsReal = implicitly[Real[V]]
    
    /* Real instance */
    def div(l: Dif[V], r: Dif[V]): Dif[V] =
      divCache.getOrElseUpdate((l, r), Dif {
        val n = l.size
        val k0 = firstNonZero(r)
        require((k0 > -1), "Division by zero is not allowed.")
        require((k0 <= firstNonZero(l)), "First non-vanishing coefficient of $r must not be higher order than the first non-vanishing coefficient of $l.")
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
    def pow(l: Dif[V], r: Dif[V]): Dif[V] =  
      powCache.getOrElseUpdate((l, r), {
        // FIXME these checks might be optimized e.g. zero, one are all constants, is that more efficient check?
        if (r == one)      l                 else
        if (r == zero) {   require(!(l == zero), "pow is not applicable to ($l,$r) as 0^0 is not defined.")
                           one             } else
        if (l == zero)     zero              else 
        if (isConstant(r)) powOnReal(l, r)   else exp(mul(r, log(l)))
      })
    
    /** Square root */
    def sqrt(x: Dif[V]): Dif[V] =
      sqrtCache.getOrElseUpdate(x, if (x == zero) zero else Dif { // We might call sqrt directly not only through power, so we check for zero
        val n  = x.size
        val k0  = firstNonZero(x) // n >= k0 because x != zero
        require((k0 % 2 == 0), "First non-vanishing coefficient must be an even power in $x in order to expand the sqrt function.")
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
          }) - (if (k % 2 == 0) - coeff(k / 2).square else zeroOfV )) / (evVIsIntegral.fromInt(2) * coeff(k0d2)) // FIXME optimize substraction of zeroOfV
        } 
        coeff.toVector        
      })
    
    /** Square */
    def square(x: Dif[V]): Dif[V] =
      squareCache.getOrElseUpdate(x, Dif {
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
    def powBySquare(l: Dif[V], n: Int): Dif[V] =
      mul(square(powByInt(l, n/2)), (if (n % 2 == 1) l else one)) // FIXME should multiplying with one be optimized? 
    
    /** Power with integer exponent */
    def powByInt(l: Dif[V], n: Int) : Dif[V] = {
        if (n == 0) one                       else // FIXME if powByInt is only called by powByReal that is only called by pow, then we do not need to check here for l==0 case 
        if (n == 1) l                         else
        if (n == 2) square(l)                 else
        if (n <  0) div(one, powByInt(l, -n)) else
        powBySquare(l, n)
    }
    
    /** Power with real exponent
     *  l != zero, r != zero, r != one */
    def powOnReal(l: Dif[V], r: Dif[V]) : Dif[V] = {
      if (r(0).isValidInt) powByInt(l, r(0).toInt) else 
      /*if (r(0).isValidDouble && r(0).toDouble == 0.5) sqrt(l)
      else*/ {
        Dif {
          val n  = l.size
          val k0  = firstNonZero(l) // n >= k0 because l != zero
          val lk0 = l(k0)
          val a = r(0) // FIXME This is a constant! Should not be lifted in the first place.
          val k0V = a * evVIsIntegral.fromInt(k0)
          require((k0 == 0 || k0V.isValidInt), "pow is not applicable to ($l,$r). Expanding around 0 needs the product of the exponent and of the index of the first non-zero coefficient to be an integer.")
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
    def exp(x: Dif[V]): Dif[V] =
      expCache.getOrElseUpdate(x, Dif {
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
    def log(x: Dif[V]): Dif[V] =
      logCache.getOrElseUpdate(x, Dif {
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
    def sin(x: Dif[V]): Dif[V] = sinAndCos(x)._1
    def cos(x: Dif[V]): Dif[V] = sinAndCos(x)._2
    private def sinAndCos(x: Dif[V]): (Dif[V],Dif[V]) =
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
        (Dif(sinCoeff.toVector), Dif(cosCoeff.toVector))
      })
    def tan(x: Dif[V]): Dif[V] = 
      tanCache.getOrElseUpdate(x, Dif {
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
    def acos(x: Dif[V]): Dif[V] =
      acosCache.getOrElseUpdate(x, Dif{
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
    def asin(x: Dif[V]): Dif[V] =
      asinCache.getOrElseUpdate(x, Dif{
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
    def atan(x: Dif[V]): Dif[V] =
      atanCache.getOrElseUpdate(x, Dif{
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
  implicit object IntDifIsIntegral extends DifAsIntegral[Int]
  implicit object DoubleDifIsReal extends DifAsReal[Double]
  implicit object IntervalDifIsReal extends DifAsReal[Interval]
  
  /** Representation of a number and its derivatives. */
  case class Dif[V](coeff: Vector[V]) {
    import Dif._
    require(coeff.size == dim, s"Tried to create Dif with incompatible dimension (${coeff.size} ~= $dim).")
    def apply(i: Int) = coeff(i)
    def size: Int = coeff.size
  }
  object Dif {
    val dim: Int = 10 // FIXME Expose this as a simulator parameter
    /** Lift a constant value of type A to a Dif. */
    def constant[V: Integral](a: V) = padWithZeros(Vector(a, implicitly[Integral[V]].zero))
    /** Lift the variable (that we differentiate w.r. to) of type V to a Dif. The value a is the current value of the variable. */
    def variable[V: Integral](a: V) = padWithZeros(Vector(a, implicitly[Integral[V]].one))
    /** Use a sequence of values of type V as the coefficients of a Dif.
     *  The sequence is padded to dim with the zero of V. */
    def apply[V: Integral](as: V*) = padWithZeros(as.toVector)
    /** Use a repeated sequence of the value a as the coefficients of a Dif. */
    def fill[V: Integral](a: V) = Dif(Vector.fill(dim)(a))
    /** Create a Dif by filling the missing positions up to dim with the zero of V. */
    private def padWithZeros[V: Integral](v: Vector[V]): Dif[V] = Dif(v.padTo(dim, implicitly[Integral[V]].zero))
  }
  
  /** Lift all numeric values in a CStore into Difs */
  def lift(st: CStore): CStore = st.mapValues(_.map{
    case nv@(Name(n,_),_) if interpreters.Common.specialFields.contains(n) => nv 
    case (n, VLit(GInt(i))) => (n, VLit(GIntDif(Dif constant i)))
    case (n, VLit(GDouble(d))) => (n, VLit(GDoubleDif(Dif constant d)))
    case v => v 
  })
  
  /** Lift all numeric values in an Expr into Difs */
  def lift(e: Expr): Expr = new acumen.util.ASTMap {
    override def mapExpr(e: Expr): Expr = (e match {
      case Lit(GInt(i)) => Lit(GIntDif(Dif.constant(i)))
      case Lit(GDouble(d)) => Lit(GDoubleDif(Dif.constant(d)))
      case _ => super.mapExpr(e)
    }).setPos(e.pos)
  }.mapExpr(e)
 
  /** Lower all Dif values in a CStore into the corresponding numeric value */
  def lower(st: CStore): CStore = {
    st.mapValues(_.mapValues{
      case VLit(GIntDif(Dif(v))) => VLit(GInt(v(0)))
      case VLit(GDoubleDif(Dif(v))) => VLit(GDouble(v(0)))
      case v => v 
    }) 
  }
  
  /** Lower all Dif values in an Expr into the corresponding numeric value */
  def lower(e: Expr): Expr = new acumen.util.ASTMap {
    override def mapExpr(e: Expr): Expr = (e match {
      case Lit(GIntDif(Dif(v))) => Lit(GInt(v(0)))
      case Lit(GDoubleDif(Dif(v))) => Lit(GDouble(v(0)))
      case _ => super.mapExpr(e)
    }).setPos(e.pos)
  }.mapExpr(e)

}
