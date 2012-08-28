package acumen.interpreters.enclosure

import Interval._

/**
 * Type to represent functions of a single variable.
 *
 * Implementation notes: see the implementation notes for
 * AffineScalarEnclosure.
 */
case class UnivariateAffineScalarEnclosure private[enclosure] (
  private[enclosure]domain: Interval,
  private[enclosure]normalizedDomain: Interval,
  private[enclosure]constant: Interval,
  private[enclosure]coefficient: Interval)(implicit rnd: Rounding) {
  assert(normalizedDomain.low equalTo 0,
    "The low end-point of the normalizedDomain should be zero!")

  /** The low bound enclosure of this enclosure. */
  def low = UnivariateAffineScalarEnclosure(domain, normalizedDomain, constant.low, coefficient.low)

  /** The high bound enclosure of this enclosure. */
  def high = UnivariateAffineScalarEnclosure(domain, normalizedDomain, constant.high, coefficient.high)

  /**
   * Evaluate the enclosure at the interval x.
   *
   * Precondition: x must be contained within the domain to the enclosure.
   */
  def apply(x: Interval) = {
    assert(domain contains x, "Enclosures must be evaluated over sub-intervals of their domain.")
    /* Since the enclosure is represented over the normalizedDomain, the argument interval must 
     * be translated into the normalizedDomain. */
    val (xlo, xhi) = (x - domain.low).bounds
    val lo = low
    val hi = high
    min(lo evalThinAtThin xlo, lo evalThinAtThin xhi) /\ max(hi evalThinAtThin xlo, hi evalThinAtThin xhi)
  }

  /**
   * Naive evaluation of this enclosure at a box.
   *
   * Precondition: the box x must be contained within the normalized domain
   * of this enclosure.
   *
   * To minimize errors due to interval arithmetic information loss both this
   * interval and the box should be thin, i.e. each interval should have zero
   * width.
   */
  private def evalThinAtThin(x: Interval)(implicit rnd: Rounding): Interval = {
    assert(normalizedDomain contains x,
      "The enclosure must be evaluated over sub-intervals of the normalizedDomain.")
    constant + coefficient * x
  }

  /**
   * Get the range of the enclosure.
   *
   * Since the enclosure is a safe approximation of any contained function
   * the range also safely approximates the range of any such function.
   */
  def range(implicit rnd: Rounding): Interval = this(domain)

  /**
   * Containment of enclosures.
   *
   * Implementation note: representing enclosures over normalized domains
   * allows us to test containment by constant- and coefficient-wise
   * containment.
   */
  def contains(that: UnivariateAffineScalarEnclosure)(implicit rnd: Rounding) = {
    val lodiffnonneg = (low - that.low).range lessThanOrEqualTo Interval(0)
    val hidiffnonneg = (that.high - high).range lessThanOrEqualTo Interval(0)
    lodiffnonneg && hidiffnonneg
  }

  /* Arithmetic operations */

  /** Subtraction of enclosures. */
  private def -(that: UnivariateAffineScalarEnclosure)(implicit rnd: Rounding) =
    UnivariateAffineScalarEnclosure(
      domain,
      normalizedDomain,
      constant - that.constant,
      coefficient - that.coefficient)

  /**
   * Compute the union of the enclosures.
   *
   * Implementation note: given that a line goes though the points
   * (xl,yl) and (xh,yl) points (x,y) on the line satisfy:
   *   y = yl + (x - xl)*(yh-yl)/(xh-xl)
   * which, under the assumption that xl = 0, simplifies to
   *   y = yl + x*(yh-yl)/xh.
   *
   * Note that this assumes that the domain interval is not thin!
   */
  def union(that: UnivariateAffineScalarEnclosure) = {
    assert(this.domain == that.domain && this.normalizedDomain == that.normalizedDomain,
      "Union is only defined for enclosures with equal domains.")
    if (domain.width equalTo 0)
      sys.error("not implemented") //TODO Implement handling thin domain!
    else {
      val thislo = low
      val thishi = high
      val thatlo = that.low
      val thathi = that.high
      val (l, h) = normalizedDomain.bounds
      val minl = min(thislo(l), thatlo(l))
      val maxl = max(thishi(l), thathi(l))
      val minh = min(thislo(h), thatlo(h))
      val maxh = max(thishi(h), thathi(h))
      val const = minl /\ maxl
      val coeff = ((minh - minl) / h) /\ ((maxh - maxl) / h)
      UnivariateAffineScalarEnclosure(domain, normalizedDomain, const, coeff)
    }
  }
}
object UnivariateAffineScalarEnclosure {

  /**
   * Conversion from AffineScalarEnclosure.
   *
   * Precondition: "that" must have domain and normliazedDomain of size one.
   */
  def apply(that: AffineScalarEnclosure)(implicit rnd: Rounding): UnivariateAffineScalarEnclosure = {
    assert(that.domain.size == 1,
      "Univariate enclosures have domain of size 1.")
    assert(that.normalizedDomain.size == 1,
      "Univariate enclosures have normalizedDomain of size 1.")
    val name = that.domain.keys.head
    UnivariateAffineScalarEnclosure(
      that.domain(name),
      that.normalizedDomain(name),
      that.constant,
      that.coefficients(name))
  }

}
