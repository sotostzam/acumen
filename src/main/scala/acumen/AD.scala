package acumen

import interpreters.enclosure.Interval

import util.ASTUtil

/**
 * Taylor series arithmetic.
 *
 * Based on description from book "Validated Numerics" by
 * Warrick Tucker, pages 121 and 122.
 */
object AD extends App {

  // FIXME Should this really be a partial ordering? 
  //       Do the properties hold for intervals?
  trait Integral[V] extends PartialOrdering[V] {
    def add(l: V, r: V): V
    def sub(l: V, r: V): V
    def mul(l: V, r: V): V
    def div(l: V, r: V): V
    def pow(l: V, r: V): V
    def neg(x: V): V
    def fromInt(i: Int): V
    def zero: V
    def one: V
  }

  trait Real[V] extends Integral[V] {
    def sin(x: V): V
    def cos(x: V): V
    def tan(x: V): V
    def exp(x: V): V
    def log(x: V): V
    def fromDouble(i: Double): V
  }
  
  implicit class IntegralOps[V](val l: V)(implicit ev: Integral[V]) {
    def +(r: V): V = ev.add(l, r)
    def -(r: V): V = ev.sub(l, r)
    def *(r: V): V = ev.mul(l, r)
    def /(r: V): V = ev.div(l, r)
    def ^(r: V): V = ev.pow(l, r)
    def unary_- = ev.neg(l)
    def < (r: V): Boolean = ev.lt(l, r)
    def > (r: V): Boolean = ev.gt(l, r)
    def <= (r: V): Boolean = ev.lteq(l, r)
    def >= (r: V): Boolean = ev.gteq(l, r)
    def zero: V = ev.zero
    def one: V = ev.one
  }

  implicit class RealOps[V](val l: V)(implicit ev: Real[V]) {
    def sin: V = ev.sin(l)
    def cos: V = ev.cos(l)
    def tan: V = ev.tan(l)
    def exp: V = ev.exp(l)
    def log: V = ev.log(l)
  }

  implicit object IntIsIntegral extends Integral[Int] {
    def add(l: Int, r: Int): Int = l + r
    def sub(l: Int, r: Int): Int = l - r
    def mul(l: Int, r: Int): Int = l * r
    def div(l: Int, r: Int): Int = l / r
    def pow(l: Int, r: Int): Int = Math.pow(l,r).toInt // FIXME Re-implement using Int
    def neg(x: Int): Int = -x
    def fromInt(x: Int): Int = x
    def zero: Int = 0
    def one: Int = 1
    def tryCompare(l: Int, r: Int): Option[Int] = Some(l compareTo r)
    def lteq(l: Int, r: Int): Boolean = l <= r
  }

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
    def exp(x: Double): Double = Math.exp(x)
    def log(x: Double): Double = Math.log(x)
    def fromInt(x: Int): Double = x
    def fromDouble(x: Double): Double = x
    def zero: Double = 0
    def one: Double = 1
    def tryCompare(l: Double, r: Double): Option[Int] = Some(l compareTo r)
    def lteq(l: Double, r: Double): Boolean = l <= r
  }

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
    def exp(x: Interval): Interval = x.exp
    def log(x: Interval): Interval = x.log
    def fromInt(x: Int): Interval = Interval(x)
    def fromDouble(x: Double): Interval = Interval(x)
    def zero: Interval = Interval.zero
    def one: Interval = Interval.one
    // FIXME Check if something needs to be overridden
    def lteq(l: Interval, r: Interval): Boolean = l lessThanOrEqualTo r
    def tryCompare(l: Interval, r: Interval): Option[Int] =
      if (l == r)         Some(0)
      else if (lteq(l,r)) Some(1) 
      else if (lteq(r,l)) Some(-1)
      else                None
  }

  class DifAsIntegral[V: Integral] extends Integral[Dif[V]] {
    /* Caches */
    val mulCache = collection.mutable.HashMap[(Dif[V], Dif[V]), Dif[V]]()
    val divCache = collection.mutable.HashMap[(Dif[V], Dif[V]), Dif[V]]()
    val powCache = collection.mutable.HashMap[(Dif[V], Dif[V]), Dif[V]]()    
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
    def div(l: Dif[V], r: Dif[V]): Dif[V] =
      // FIXME Extend using l’Hopital’s rule
      divCache.getOrElseUpdate((l, r), Dif {
        val n = l.size
        val coeff = new collection.mutable.ArraySeq[V](n)
        coeff(0) = l(0) / r(0)
        for (k <- 1 until n)
          coeff(k) = (l(k) - (0 to k - 1).foldLeft(zeroOfV) {
            case (sum, i) => sum + (coeff(i) * r(k - i))
          }) / r(0)
        coeff.toVector
      })
    def pow(l: Dif[V], r: Dif[V]): Dif[V] = {
      val l0 = l(0)
      val rc = r(0) // FIXME This is a constant! Should not be lifted in the first place.
      require(!(l0 == zeroOfV && rc > zeroOfV), "pow is not applicable to ($l,$r). $l may not be equal to 0.")
      powCache.getOrElseUpdate((l, r), Dif {
        val n = l.size
        val coeff = new collection.mutable.ArraySeq[V](n)
        coeff(0) = l0 ^ rc
        for (k <- 1 until n) {
          val kL = evVIsIntegral.fromInt(k)          
          coeff(k) = ((1 to k).foldLeft(zeroOfV) {
            case (sum, i) =>  
              sum + (((((rc + oneOfV) * evVIsIntegral.fromInt(i)) / kL) - oneOfV) * l(i) * coeff(k - i)) 
          }) / l0
        }
        coeff.toVector
      })
    }
    def neg(x: Dif[V]): Dif[V] = Dif(x.coeff.map(y => -y))
    def fromInt(x: Int): Dif[V] = Dif.constant(evVIsIntegral fromInt x)
    // FIXME Test these definitions
    def zero: Dif[V] = Dif.fill(zeroOfV)
    def one: Dif[V] = Dif.constant(oneOfV)
    def tryCompare(l: Dif[V], r: Dif[V]): Option[Int] = evVIsIntegral.tryCompare(l(0), r(0))
    def lteq(l: Dif[V], r: Dif[V]): Boolean = evVIsIntegral.lteq(l(0), r(0))
  }
  
  class DifAsReal[V: Real] extends DifAsIntegral[V] with Real[Dif[V]] {
    /* Caches */
    val sinAndCosCache = collection.mutable.HashMap[Dif[V], (/*sin*/Dif[V], /*cos*/Dif[V])]()
    val tanCache = collection.mutable.HashMap[Dif[V], Dif[V]]()
    val expCache = collection.mutable.HashMap[Dif[V], Dif[V]]()
    val logCache = collection.mutable.HashMap[Dif[V], Dif[V]]()
    /* Constants */
    val evVIsReal = implicitly[Real[V]]
    /* Real instance */
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
        val cos0 = x(0).cos
        val cos02 = cos0 * cos0 // FIXME Use cos0^2 instead
        for (k <- 1 until n) {
          coeff(k) = (x(k) - ((1 to k-1).foldLeft(zeroOfV) {
            case (sum, i) => 
              val c = cos(x)(k - i)
              sum + evVIsIntegral.fromInt(i) * coeff(i) * c * c // FIXME Use c^2 instead
          }) / evVIsIntegral.fromInt(k)) / cos02
        }
        coeff.toVector
      })
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
        val n = x.size
        val coeff = new collection.mutable.ArraySeq[V](n)
        val x0 = x(0)
        coeff(0) = x0.log
        for (k <- 1 until n) {
          val kL = evVIsIntegral.fromInt(k)          
          coeff(k) = (x(k) - ((1 to k - 1).foldLeft(zeroOfV) {
            case (sum, i) => sum + evVIsIntegral.fromInt(i) * coeff(i) * x(k-i)
          }) / kL) / x0
        }
        coeff.toVector
      })
    // TODO Add square function according to Griewank book
    def fromDouble(x: Double): Dif[V] = Dif.constant(evVIsReal fromDouble x)
  }
  implicit object IntDifIsIntegral extends DifAsIntegral[Int]
  implicit object DoubleDifIsReal extends DifAsReal[Double]
  implicit object IntervalDifIsReal extends DifAsReal[Interval]
  
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
    /** Lift a variable of type V to a Dif. The value a is the current value of the variable. */
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
