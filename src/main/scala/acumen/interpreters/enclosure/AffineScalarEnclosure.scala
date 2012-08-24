package acumen.interpreters.enclosure

import Types._
import Util._

/**
 * Type used to approximate expressions over a given domain.
 *
 * Implementation note: each variable is stored as a projection over an
 * interval of the form [0,_]. This simplifies the implementation of
 * enclosure operations because variables become monotonically increasing
 * over such intervals, which e.g. allows for easy extraction of the bounds
 * of the enclosure.
 */
case class AffineScalarEnclosure private[enclosure] (
  private[enclosure] val domain: Box,
  /* To save wasteful shifting during enclosure operations the internal
   * representation of the domain is such that each variable is non-negative.
   * 
   * Implementation note: this will make it possible to e.g. compute bounds 
   * of the enclosure by simply taking the corresponding bounds of the 
   * constant term and coefficients. */
  private[enclosure] val normalizedDomain: Box,
  private[enclosure] val constant: Interval,
  private[enclosure] val coefficients: Box) {

  /** The number of variables the enclosure depends on. */
  def arity = coefficients.size

  /** The number of variables in the domain of the enclosure. */
  def dimension = domain.size

  /** The lower bound enclosure of this enclosure. */
  def low = AffineScalarEnclosure(domain, normalizedDomain, constant.low, coefficients.mapValues(_.low))

  /** The high bound enclosure of this enclosure. */
  def high = AffineScalarEnclosure(domain, normalizedDomain, constant.high, coefficients.mapValues(_.high))

  /**
   * Evaluate the enclosure at the box x.
   *
   * Precondition: the box must be non-empty.
   *
   * Note that the box should be component-wise contained within the domain
   * of this enclosure for the approximation guarantees to hold.
   *
   * E.g. [t-0.25,t] is a valid enclosure of t^2 over [0,1] but not outside
   * [0,1]. E.g. [1.75,2] does not contain the value 4 of t^2 at t=2.
   *
   * Implementation note: the algorithm relies on the extreme values of the
   * enclosure occurring at the corners of the box. This is the case for e.g.
   * affine enclosures.
   */
  def apply(x: Box) = {
    require(x.nonEmpty)
    /* It is essential to evaluate the enclosure over the original domain.
     * To avoid unnecessary errors the argument is shifted to the normalized
     * domain rather than the enclosure to the original domain. */
    val c :: cs = Box.corners(x.map {
      case (name, value) => name -> (value - domain(name).low)
    })
    val lo = low
    val hi = high
    cs.foldLeft((lo evalThinAtThin c) /\ (hi evalThinAtThin c)) {
      case (res, corner) => res /\ (lo evalThinAtThin corner) /\ (hi evalThinAtThin corner)
    }
  }
  /**
   * Naive evaluation of this enclosure at a box.
   *
   * Precondition: the box x must be component-wise within the normalized
   * domain of this enclosure.
   *
   * To minimize errors due to interval arithmetic information loss both this
   * interval and the box should be thin, i.e. each interval should have zero
   * width.
   */
  private def evalThinAtThin(x: Box): Interval = {
    require(x.forall { case (name, interval) => normalizedDomain(name) contains interval })
    coefficients.foldLeft(constant) {
      case (res, (name, coefficient)) => res + coefficient * normalizedDomain(name)
    }
  }

  /**
   * Get the range of the enclosure.
   *
   * Since the enclosure is a safe approximation of any contained function
   * the range also safely approximates the range of any such function.
   */
  def range = this(domain)

  /**
   * Containment of enclosures.
   *
   * Implementation note: representing enclosures over normalized domains
   * allows us to test containment by constant- and coefficient-wise
   * containment.
   */
  def contains(that: AffineScalarEnclosure)(implicit r: Rounding) =
    (zipDefault(coefficients, that.coefficients, Interval(0)).forall {
      case (l, r) => l contains r
    }) && (constant contains that.constant)

  /* Arithmetic operations */

  /**
   * Creates a new enclosure by applying a binary operator to the values of "this" and "that" enclosure.
   * In the cases when one of the enclosures does not contain a variable which the other does, the
   * interval [0,0] is used as a default.
   */
  def zipWith(f: (Interval, Interval) => Interval)(that: AffineScalarEnclosure)(implicit r: Rounding) =
    AffineScalarEnclosure(domain, normalizedDomain,
      f(constant, that.constant),
      (this.coefficients.keySet union that.coefficients.keySet).map { k =>
        (k, f(
          this.coefficients.getOrElse(k, Interval(0)),
          that.coefficients.getOrElse(k, Interval(0))))
      }.toMap)

  /** Addition of enclosures. */
  def +(that: AffineScalarEnclosure)(implicit r: Rounding): AffineScalarEnclosure = zipWith(_ + _)(that)
  def +(that: Interval)(implicit r: Rounding): AffineScalarEnclosure = AffineScalarEnclosure(domain, normalizedDomain, constant + that, coefficients)

  /** Subtraction of enclosures. */
  def -(that: AffineScalarEnclosure)(implicit r: Rounding): AffineScalarEnclosure = zipWith(_ - _)(that)

}
object AffineScalarEnclosure {

  /** Convenience method, normalizes the domain. */
  private[enclosure] def apply(domain: Box, constant: Interval, coefficients: Box): AffineScalarEnclosure =
    AffineScalarEnclosure(domain, Box.normalize(domain), constant, coefficients)

  /** Lifts a constant interval to a constant enclosure. */
  def apply(domain: Box, constant: Interval): AffineScalarEnclosure = {
    AffineScalarEnclosure(domain, constant, Box.empty)
  }

  /**
   * Lifts a variable "name" in the domain to an identity function over the
   * corresponding interval.
   */
  def apply(domain: Box, name: VarName)(implicit r: Rounding): AffineScalarEnclosure = {
    /* Implementation note: The constant term needs to be domain(name).low 
     * because the internal representation is over the normalized domain. */
    AffineScalarEnclosure(domain, domain(name).low, Map(name -> Interval(1)))
  }

}
