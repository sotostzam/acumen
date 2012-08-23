package acumen.interpreters.enclosure

import Types._

/**
 * Type used to approximate expressions over a given domain.
 *
 * Implementation note: each variable is stored as a projection over an
 * interval of the form [0,_]. This simplifies the implementation of
 * enclosure operations because variables become monotonically increasing
 * over such intervals, which e.g. allows for easy extraction of the bounds
 * of the enclosure.
 */
case class AffineScalarEnclosure private (
  private val domain: Box,
  private val constant: Interval,
  private val coefficients: Box) {

  /* To save wasteful shifting during enclosure operations the internal
   * representation of the domain is such that each variable is non-negative.
   * 
   * Implementation note: this will make it possible to e.g. compute bounds 
   * of the enclosure by simply taking the corresponding bounds of the 
   * constant term and coefficients. */
  private val normalizedDomain = Box.normalize(domain)

  /** The number of variables the enclosure depends on. */
  def arity = coefficients.size

  /** The number of variables in the domain of the enclosure. */
  def dimension = domain.size

  /** The lower bound enclosure of this enclosure. */
  def low = AffineScalarEnclosure(domain, constant.low, coefficients.mapValues(_.low))

  /** The high bound enclosure of this enclosure. */
  def high = AffineScalarEnclosure(domain, constant.high, coefficients.mapValues(_.high))
  
  /** 
   * Evaluate the enclosure at the box x.
   *  
   * Note that the box should be contained within the domain of this enclosure
   * for the approximation guarantees to hold. 
   *  
   * E.g. [t-0.25,t] is a valid enclosure of t^2 over [0,1] but not outside 
   * [0,1]. E.g. [1.75,2] does not contain the value 4 of t^2 at t=2.
   */
  def apply(x:Box) = {} // TODO needed to test low and high methods.
  
  /**
   * Get the range of the enclosure.
   * 
   * Since the enclosure is a safe approximation of any contained function
   * the range also safely approximates the range of any such function.
   */
  def range = {} // TODO needed for precise evaluation, i.e. implementation of apply.

}
object AffineScalarEnclosure {

  /** Lifts a constant interval to a constant enclosure. */
  def apply(domain: Box, constant: Interval): AffineScalarEnclosure =
    AffineScalarEnclosure(domain, constant, Box.empty)

  /**
   * Lifts a variable "name" in the domain to an identity function over the
   * corresponding interval.
   */
  def apply(domain: Box, name: VarName)(implicit r: Rounding): AffineScalarEnclosure = {
    /* 
     * Implementation note: The constant term needs to be domain(name).low 
     * because the internal representation is over the normalized domain. 
     */
    AffineScalarEnclosure(domain, domain(name).low, Map(name -> Interval(1)))
  }

}
