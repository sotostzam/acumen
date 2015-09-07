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

object FADTest extends Properties("AD") {

  /* Utilities */
  
  val evDoubleFDifIsIntegral = implicitly[Integral[FDif[Double]]]
  val evDoubleFDifIsReal = implicitly[Real[FDif[Double]]]
      
  val evIntervalFDifIsIntegral = implicitly[Integral[FDif[Interval]]]
  val evIntervalFDifIsReal = implicitly[Real[FDif[Interval]]]
               
  trait Similar[T] {
    def close(l: T, r: T): Boolean
    def in(l: T, r: T): Boolean
  }
  implicit object DoubleIsSimilar extends Similar[Double] {
    def close(l: Double, r: Double): Boolean = 
      Math.abs(l - r) <= 10e-6 * Math.min(Math abs l, Math abs r)
    def in(l: Double, r: Double): Boolean =
      l == r
  }
  implicit object IntervalIsSimilar extends Similar[Interval] {
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
      else if (l.coeff.keySet != r.coeff.keySet)
        sys.error("Key sets do not match!")
      else l.coeff.keySet.map(k => ((l.coeff(k), r.coeff(k)), k)).forall {
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
   
  /* Other properties */
  
  property("lift leaves no base number types") = 
    forAll(arbitrary[GroundValue], listOf1(arbitrary[(CId,Name)])) { (gv, ns) =>
      forAll(oneOf(ns)) { case (id,n) =>
        lift[CId,Double](Lit(gv))(DoubleIsReal, QName(id,n), ns.map{ case (id1,n1) => QName(id1,n1) }) match {
          case Lit(_: GDouble | _: GInt | _: GInterval) => false
          case _ => true
        } 
      }
    }

}
