package acumen.interpreters.enclosure

import scala.annotation.unchecked
import Interval._
import acumen.{GDouble, VLit}
import net.java.jinterval.rational._
import net.java.jinterval.interval.set._

import scala.math
import org.apache.commons.math3




/**
 * Represent an interval which can be split.
 * Constructor requires a list of points where to split in addition to the SetInterval
 */
case class SplitInterval(private val i: Interval,
                         private val is: IntervalSplitter)
extends Interval(ic.numsToInterval(i.lo, i.hi)) {
  // Return the list of the subintervals described by the list of points p
  def subIntervals: List[(Interval, Some[Interval])] = is.subIntervals

  // Return the list of subpoints resulting from taking the bounds of the subintervals
  def subPoints: List[Real] = is.subPoints
}

object SplitInterval {
  /** SplitInterval from a basic Interval. */
  def apply(i: Interval): SplitInterval = SplitInterval(i, List(1.0))
  /** SplitInterval from an Interval and a number of split*/
  def apply(i: Interval, n: Int): SplitInterval = SplitInterval(i, List.fill(n)(1.0))
  /** Split with a list a relative weights (negative weights mean ignored subinterval) */
  def apply(i: Interval, ws: ImplRealList) =
    new SplitInterval(i, IntervalSplitter(i, ws.l))
  /** Split with the raw list of points (bounds and inner) and the list of "keep" booleans */
  def apply(ps: ImplRealList, keep: List[Boolean] = List.empty, probas: List[Interval] = List.empty) =
    new SplitInterval(
      Interval(ps.l.head, ps.l.last),
      IntervalSplitter(ps.l,
                       if(keep.nonEmpty) keep else List.fill(ps.l.size - 1)(true),
                       if(probas.nonEmpty) probas else List.fill(ps.l.size - 1)(Interval(0, 1))))

  case class ImplRealList(l: List[Real])
  implicit def DoubleToRealList(vals: List[Double]): ImplRealList = ImplRealList(vals map ExtendedRational.valueOf)
  implicit def RealToRealList(vals: List[Real]): ImplRealList = ImplRealList(vals)
}

/**
  * Wrap all the splitting information and methods relative to a splitInterval
  * ps: List of all points (bounds and inner)
  * keep: List whose n-th boolean tells if the n-th subinterval must be kept
  */
class IntervalSplitter(ps: List[Real], keep: List[Boolean], probas: List[Interval]) {
  require((ps.length < 3) || ((ps zip ps.tail) forall {case (a, b) => a le b}),
    "Splitter points must be in increasing order: " + ps.mkString("[", ", ", "]"))
  require(ps.length == keep.length + 1 && ps.length == probas.length + 1,
    "The list of points must be one element longer than the keep and the probas lists")
  require(probas forall (Interval(0, 1) contains _),
    "All probabilities must be between 0 and 1")
  require((probas reduce (_ + _)).lo le ExtendedRational.one,
    "The sum of the minimal probabilities of each subinterval must be <= 1")

  def subIntervals: List[(Interval, Some[Interval])] = {
    val allZipped = ps zip ps.tail map { case (l, h) => Interval(l, h) } zip keep zip probas
    allZipped flatMap {case ((i, k), p) => if(k) Some(i, Some(p)) else None}
  }

  //No probability attached if split in points
  def subPoints: List[Real] = {
    //The n-th point (starting from 0) is a bound of a kept interval if
    //the (n-1)-th or the n-th keep boolean is "true" (interval before or after)
    def bound = (x: Int) => Math.max(Math.min(x, keep.size - 1), 0)
    ps.zipWithIndex.filter{case (_, i) =>
      keep(bound(i - 1)) || keep(bound(i))}
      .unzip._1
  }
}


//@Masoumeh >
class BooleanIntervalSplitter(ps: List[Real], keep: List[Boolean], probas: List[Interval]) extends IntervalSplitter(ps, keep, probas) {
  override def subIntervals: List[(Interval, Some[Interval])] = {
    val allZipped = keep map { 
        case true => Interval(1,1)
        case false => Interval(0,0)} zip probas
    allZipped flatMap {case (i, p) => Some(i, Some(p))}
  }
}

object BooleanIntervalSplitter {   
   def apply(ps: ImplRealList, keep: List[Boolean], probas: List[Interval]): BooleanIntervalSplitter = 
       new BooleanIntervalSplitter(ps.l, keep, probas)
   
  case class ImplRealList(l: List[Real])
  implicit def DoubleToRealList(vals: List[Double]): ImplRealList = ImplRealList(vals map ExtendedRational.valueOf)
  implicit def RealToRealList(vals: List[Real]): ImplRealList = ImplRealList(vals)   
}
//< @Masoumeh


object IntervalSplitter {
  /** Splitter from the raw list of points (bounds and inner) and the list of "keep" booleans */
  def apply(ps: ImplRealList, keep: List[Boolean], probas: List[Interval]): IntervalSplitter =
    new IntervalSplitter(ps.l, keep, probas)

  /** Splitter from the list a relative weights (negative weights mean ignored subinterval) */
  def apply(i: Interval, ws: ImplRealList): IntervalSplitter = {
    require(ws.l exists (0 !=), "Must give at least one non zero weight")

    //keep list
    val keep = ws.l map ExtendedRational.zero.le
    //The splitting with weights do not lead to distributed probabilities so [0 .. 1] is used
    val probas = List.fill(keep.size)(Interval(0, 1))
    //absolute value of weights, total weight, original interval length
    val aws = ws.l map ExtendedRationalOps.abs
    def tw = aws.foldLeft(ExtendedRational.zero.asInstanceOf[Real])(ExtendedRationalOps.add)
    def l = ExtendedRationalOps.sub(i.hi, i.lo)

    //List of sums of weights up to the index
    def sws = (0 to aws.length).toList.map(n => (aws take n).foldLeft
    (ExtendedRational.zero.asInstanceOf[Real])((t: Real, w: Real) => ExtendedRationalOps.add(t, w)))

    //Conversion of the cumulated weights to a list of points
    //mapped function: weight => lo + weight / r with r = tw / l
    val r = if (l ne ExtendedRational.zero()) ExtendedRationalOps.div(tw, l) else ExtendedRational.POSITIVE_INFINITY
    new IntervalSplitter(sws map (weight => ExtendedRationalOps.add(i.lo, ExtendedRationalOps.div(weight, r))), keep, probas)
  }

  case class ImplRealList(l: List[Real])
  implicit def DoubleToRealList(vals: List[Double]): ImplRealList = ImplRealList(vals map ExtendedRational.valueOf)
  implicit def RealToRealList(vals: List[Real]): ImplRealList = ImplRealList(vals)
}


/**
  * SplitterDistribution extensions are classes based on a distribution and convertible to a splitInterval
  * by calling their apply method with the number of splits required.
  * Be careful, these classes do not extend Interval and must be converted before using them in places
  * where an Interval is expected
  */
trait SplitterDistribution {
  def apply(centered: Double, n: Int): SplitInterval = {
    val (points, keep, probas) = producePoints(centered, n)
    SplitInterval(points, keep, probas)
  }

  /**
    * Produce the list of the points resulting from the split in respect of the distribution which implement this trait
    * @param central truncation of the distribution around the middle point (in [0 .. 1], 0 and or 1 excluded depending on the distribution)
    * @param n number of splits to perform
    * @return the list of points, the list of keep flags (cf ExprSplitterDistribution) and the list of probabilities attached to each subinterval)
    */
  def producePoints(central: Double, n: Int): (List[Real], List[Boolean], List[Interval]) = {
    require(central > 0 && central <= 1, s"central argument must be between 0 and 1: $central%")
    require(n > 0, s"The number of split must be positive: splitby $n")
    val lo = icdf((1 - central) / 2)
    val hi = icdf((1 + central) / 2)
    val intermediateWeights = {
      val loWeight = cdf(lo)
      val hiWeight = cdf(hi)
      val subIntWeight = (hiWeight - loWeight) / n
      (1 until n) map (i => loWeight + (i * subIntWeight))
    }
    //List of the points which bounds the subintervals
    val points = (intermediateWeights.foldLeft(List(lo))((res, s) => res ::: List(icdf(s))) ::: List(hi)) map ExtendedRational.valueOf
    //All the subintervals are kept in case of a distribution splitting
    val keep = List.fill(points.size - 1)(true)
    //All the subintervals are equiprobable and the global probability is equal to the central parameter
    val probas = List.fill(keep.size)(Interval(
      ExtendedRationalOps.mul(
        ExtendedRational.valueOf(central),
        ExtendedRationalOps.div(
          ExtendedRational.one,
          ExtendedRational.valueOf(keep.size)))))
    (points, keep, probas)
  }
  def dist: org.apache.commons.math3.distribution.AbstractRealDistribution
  def cdf(x: Double): Double = dist.cumulativeProbability(x)
  def icdf(p: Double): Double = dist.inverseCumulativeProbability(p)
}

case class UniformDistribution(lo: Double, hi: Double) extends SplitterDistribution {
  require(lo < hi, s"The lower bound must be lower than the higher bound: $lo > $hi")
  //Not rigorously correct but does the trick to perform the splitting
  def dist = new org.apache.commons.math3.distribution.UniformRealDistribution(lo, hi)
}

object UniformDistribution {
  def apply(lo: Double, hi: Double, c: Double, n: Int): SplitInterval =
    UniformDistribution(lo, hi)(c, n)
}

case class NormalDistribution(mu: Double, sigmaSquared: Double) extends SplitterDistribution {
  require(sigmaSquared > 0, s"sigma^2 must be positive: $sigmaSquared")
  
  def dist = new org.apache.commons.math3.distribution.NormalDistribution(mu, Math.sqrt(sigmaSquared))

}

object NormalDistribution {
  def apply(mu: Double, sigmaSquared: Double, centered: Double, n: Int): SplitInterval =
    NormalDistribution(mu, sigmaSquared)(centered, n)
}



case class BetaDistribution(lo: Double, hi: Double, a: Double, b: Double) extends SplitterDistribution {
  require(lo < hi, s"The lower bound must be lower than the higher bound: $lo > $hi")
  require(a > 0 && b > 0, s"the parameters of the beta function must be positive: alpha = $a, beta = $b")

  def dist = new org.apache.commons.math3.distribution.BetaDistribution(a, b)
  override def cdf(x: Double): Double = dist.cumulativeProbability((x - lo) / (hi - lo))
  override def icdf(p: Double): Double = dist.inverseCumulativeProbability(p) * (hi - lo) + lo
}

object BetaDistribution {
  def apply(lo: Double, hi: Double, a: Double, b: Double, c: Double, n: Int): SplitInterval =
    BetaDistribution(lo, hi, a, b)(c, n)
}