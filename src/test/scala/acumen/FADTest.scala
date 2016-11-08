package acumen

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.Gen
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck.Properties
import testutil.Generators._
import interpreters.enclosure.Interval
import interpreters.enclosure.Generators._
import FAD._
import acumen.interpreters.reference2015.Interpreter.FieldImpl

object FADTest extends Properties("FAD") {

  /* Utilities */
  
  val evDoubleFDifIsIntegral = implicitly[Integral[FDif[Double]]]
  val evDoubleFDifIsReal = implicitly[Real[FDif[Double]]]
      
  val evIntervalFDifIsIntegral = implicitly[Integral[FDif[Interval]]]
  val evIntervalFDifIsReal = implicitly[Real[FDif[Interval]]]
  
  def fromIntD(i: Int) = evDoubleFDifIsIntegral.fromInt(i)
  val zeroD = evDoubleFDifIsIntegral.zero
  val oneD = evDoubleFDifIsIntegral.one
  val piD = evDoubleFDifIsReal.fromDouble(Math.PI)
               
  trait Similar[T] {
    def close(l: T, r: T): Boolean
    def in(l: T, r: T): Boolean
  }
  implicit object doubleIsSimilar extends Similar[Double] {
    def close(l: Double, r: Double): Boolean = 
      Math.abs(l - r) <= 10e-6 * Math.min(Math abs l, Math abs r)
    def in(l: Double, r: Double): Boolean =
      l == r
  }
  implicit object dntervalIsSimilar extends Similar[Interval] {
    def close(l: Interval, r: Interval): Boolean = 
      (l intersect r).isDefined
    def in(l: Interval, r: Interval): Boolean =
      r contains l
  }
  implicit class SimilarOps[V](val l: V)(implicit ev: Similar[V]) {
    def ~=(r: V): Boolean = ev.close(l, r)
    def in(r: V): Boolean = ev.in(l, r)
  }
  
  implicit class FDifIsSimilar[T : Similar](l: FDif[T]) {
    /** Compare two FDifs coefficient-wise using comp, ignoring first n coefficients */
    def similar(r: FDif[T], comp: (T,T) => Boolean, msg: String, n: Int): Boolean =
      if (l.head != r.head)
        sys.error(s"\n\nFound $msg at head :\n\n" +
            s"Left coefficient:\n${l.head}\n\nRight coefficient:\n${r.head}\n\n" +
            s"Full left Dif:\n$l\n\nFull right Dif:\n$r\n\n")
      else if (!(l.coeff.isEmpty || r.coeff.isEmpty || l.coeff.keySet == r.coeff.keySet))
        sys.error("Key sets do not match: \n\t" + l.coeff.keySet + "\n\t" + r.coeff.keySet)
      else l.coeff.keySet.map(k => ((l(k), r(k)), k)).forall {
        case ((lc, rc), i) =>
          if (comp(lc, rc)) true
          else sys.error(s"\n\nFound $msg at key $i:\n\n" +
              s"Left coefficient:\n$lc\n\nRight coefficient:\n$rc\n\n" +
              s"Full left Dif:\n$l\n\nFull right Dif:\n$r\n\n")
    }
    def ~=(r: FDif[T]): Boolean = similar(r, (lc:T,rc:T) => lc ~= rc, "dissimilarity", 0)
    /** Ignore first n coefficients */
    def ~=(n: Int)(r: FDif[T]): Boolean = similar(r, (lc:T,rc:T) => lc ~= rc, "dissimilarity", n)
    def in(r: FDif[T]): Boolean = similar(r, (lc:T,rc:T) => lc in rc, "non-inclusion", 0)
  }
  
  /* Properties of FDif[Double] as Integral */
  
  property("FDif[Double].add: x+x ~= 2*x") =
    forAll(listOf1(genQName)) { (ns: List[QName]) =>
      forAll(genConstantFDif[Double](ns)) { (x: FDif[Double]) =>
        (x + x) ~= (FDif.constant(2d) * x)
      }
    }
  
  property("FDif[Double].add: 1+x > x") = 
    forAll(listOf1(genQName)) { (ns: List[QName]) =>
      forAll(genConstantFDif[Double](ns)) { (x: FDif[Double]) =>
        (oneD + x) > x
      }
    }
  
  property("FDif[Double].mul: x*x >= x") = 
    forAll(listOf1(genQName)) { (ns: List[QName]) =>
      forAll(genConstantFDif[Double](ns)) { (x: FDif[Double]) =>
        (x * x) >= x
      }
    }
  
  property("FDif[Double].mul: x*1 ~= x && 1*x ~= x") =
    forAll(listOf1(genQName)) { (ns: List[QName]) =>
      forAll(genConstantFDif[Double](ns)) { (x: FDif[Double]) =>
        ((x * oneD) ~= x) && ((oneD * x) ~= x)
      }
    }
  
  property("FDif[Double].div: x/1 ~= x") = 
    forAll(listOf1(genQName)) { (ns: List[QName]) =>
      forAll(genConstantFDif[Double](ns)) { (x: FDif[Double]) =>
        (x / oneD) ~= x
      }
    }
  
  property("Dif[Double].div: x/x ~= 1") = 
    forAll(listOf1(genQName)) { (ns: List[QName]) =>
      forAll(genConstantFDif[Double](ns)) { (x: FDif[Double]) =>
        (x.head != 0d) ==> ((x / x) ~= oneD)
      }
    }
  
  /* Other properties */

  property("lift leaves no base number types") =
    forAll(arbitrary[StaticGroundValue], listOf1(arbitrary[(CId, Name)])) { (gv, ns) =>
      forAll(oneOf(ns)) {
        case (id, n) =>
          lift(Lit(gv), ns.map { case (id1, n1) => QName(id1, n1) })(doubleIsReal) match {
            case Lit(_: GDouble | _: GInt | _: GInterval | _: GConstantRealEnclosure) => false
            case _ => true
          }
      }
    }

  /* Generators */
  
  def genConstantFDif[I: Integral](ns: List[QName]): Gen[FDif[I]] = {
    val ev = implicitly[Integral[I]]    
    for {
      n <- oneOf(ns)
      c <- choose(-1000,1000)
    } yield FDif(ev fromInt c, ns.map(m => m -> (if (m == n) ev.one else ev.zero)).toMap)
  }

  def genQName(): Gen[QName] =
    for {
      id <- arbitrary[CId]
      n <- arbitrary[Name]
    } yield QName(id, n)
  
  
}
