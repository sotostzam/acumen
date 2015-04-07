package acumen.interpreters.enclosure.affine

import acumen.interpreters.enclosure.Interval._
import acumen.ui.interpreter.Enclosure
import java.awt.Color
import acumen.interpreters.enclosure.Interval

/**
 * Type to represent functions of a single variable.
 *
 * Implementation notes: see the implementation notes for
 * AffineScalarEnclosure.
 */
case class PaddedUnivariateAffineScalarEnclosure private[enclosure] (
  //  private[enclosure]
  override val domain: Interval,
  override val normalizedDomain: Interval,
  override val constant: Interval,
  override val coefficient: Interval,
  padding: Interval => Interval)
    extends UnivariateAffineScalarEnclosure {
  assert(normalizedDomain.low equalTo 0,
    "The low end-point of the normalizedDomain should be zero!")

  def fromPadded: UnivariateAffineScalarEnclosure =
    UnivariateAffineScalarEnclosure(
      domain,
      normalizedDomain,
      constant,
      coefficient)

  /** The low bound enclosure of this enclosure. */
  override def low =
    PaddedUnivariateAffineScalarEnclosure(
      domain,
      normalizedDomain,
      constant.low,
      coefficient.low,
      padding)

  /** The high bound enclosure of this enclosure. */
  override def high =
    PaddedUnivariateAffineScalarEnclosure(
      domain,
      normalizedDomain,
      constant.high,
      coefficient.high,
      padding)

  override def isConstant = (coefficient equalTo 0) &&
    (padding(domain.low) equalTo padding(domain.high))

  /**
   * Evaluate the enclosure at the interval x.
   *
   * Precondition: x must be contained within the domain to the enclosure.
   */
  override def apply(x: Interval) = {
    val xtranslated = x - domain.low
    evalThinAtThin(xtranslated) + padding(xtranslated)
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
  private def evalThinAtThin(x: Interval): Interval = {
    //    assert(normalizedDomain contains x,
    //      "The enclosure must be evaluated over sub-intervals of the normalizedDomain.")
    constant + coefficient * x
  }

  /**
   * Get the range of the enclosure.
   *
   * Since the enclosure is a safe approximation of any contained function
   * the range also safely approximates the range of any such function.
   */
  override def range: Interval = this(domain)

  /**
   * Containment of enclosures.
   *
   * Note: This implementation relies on the fact that "this" and
   * "that" are affine enclosures, meaning that containment can
   * be decided by comparing the upper and lower bounds of the
   * enclosures at the domain's end-points.
   */
  override def contains(that: UnivariateAffineScalarEnclosure) = {
    assert(this.domain == that.domain, "Containment is only defined for enclosures over the same domain.")
    val lo = domain.low
    val hi = domain.high
    val paddedThat = that.asInstanceOf[PaddedUnivariateAffineScalarEnclosure]
    val thisLo = this.low
    val thatLo = paddedThat.low
    val thisHi = this.high
    val thatHi = paddedThat.high
    val boundAtLo = // Ensure that "this" bounds "that" at domin.low
      (thisLo(lo) lessThanOrEqualTo (thatLo(lo))) &&
        (thatHi(lo) lessThanOrEqualTo (thisHi(lo)))
    val boundAtHi = // Ensure that "this" bounds "that" at domin.low
      (thisLo(hi) lessThanOrEqualTo (thatLo(hi))) &&
        (thatHi(hi) lessThanOrEqualTo (thisHi(hi)))
    boundAtHi && boundAtLo
  }

  //TODO Update the below comment.
  /**
   * Compute the union of `this` and `that` enclosure.
   *
   * Implementation note: TODO
   */
  override def union(that: UnivariateAffineScalarEnclosure) = {
    assert(this.domain == that.domain, "Union can only be taken of enclosures over the same domain.")
    val lo = domain.low
    val hi = domain.high
    def newPadding(x: Interval) =
      padding(x) /\ that.asInstanceOf[PaddedUnivariateAffineScalarEnclosure].padding(x)
    if (lo == hi)
      PaddedUnivariateAffineScalarEnclosure(
        domain,
        normalizedDomain,
        this(lo) /\ that(lo),
        Interval.zero,
        newPadding(_))
    else {
      val thisLo = this.low
      val thatLo = that.low
      val thisHi = this.high
      val thatHi = that.high
      val minAtLo = min(thisLo(lo), thatLo(lo))
      val maxAtLo = max(thisHi(lo), thatHi(lo))
      val minAtHi = min(thisLo(hi), thatLo(hi))
      val maxAtHi = max(thisHi(hi), thatHi(hi))
      val width = domain.width
      val coeffMin = (minAtHi - minAtLo) / width
      val coeffMax = (maxAtHi - maxAtLo) / width
      PaddedUnivariateAffineScalarEnclosure(
        domain,
        normalizedDomain,
        minAtLo /\ maxAtLo,
        coeffMin /\ coeffMax,
        newPadding(_))
    }
  }

  /**
   * Shorter version of union.
   *
   * FIXME: should wrap thinning unions in non-widening ones!
   */
  override def newUnion(that: UnivariateAffineScalarEnclosure): PaddedUnivariateAffineScalarEnclosure = {
    assert(this.domain == that.domain, "Union can only be taken of enclosures over the same domain.")
    val domLow = domain.low
    val domHigh = domain.high
    def newPadding(x: Interval) =
      padding(x) /\ that.asInstanceOf[PaddedUnivariateAffineScalarEnclosure].padding(x)
    if (domLow equalTo domHigh)
      PaddedUnivariateAffineScalarEnclosure(
        domain,
        normalizedDomain,
        this.constant /\ that.constant,
        Interval.zero,
        newPadding)
    else {
      val resConstant = this.constant /\ that.constant
      val resAtDomHigh = this(domHigh) /\ that(domHigh)
      val resCoefficientLow = resAtDomHigh.low - resConstant.low
      val resCoefficientHigh = resAtDomHigh.high - resConstant.high
      val resCoefficient = resCoefficientLow /\ resCoefficientHigh
      PaddedUnivariateAffineScalarEnclosure(
        domain,
        normalizedDomain,
        resConstant,
        resCoefficient,
        newPadding)
    }
  }

  /**
   * The restriction of `this` enclosure to a sub-interval of its domain.
   *
   * Returns an enclosure with the same constant and coefficient as `this`
   * enclosure, redefined over a sub-interval of its domain.
   */
  //TODO Add property
  override def restrictTo(subDomain: Interval): PaddedUnivariateAffineScalarEnclosure = {
    require((domain contains subDomain) && (normalizedDomain contains 0 /\ subDomain.width.high))
    PaddedUnivariateAffineScalarEnclosure(
      subDomain,
      constant + (coefficient * (subDomain.low - domain.low)),
      coefficient,
      padding)
  }

  // TODO improve description
  /** For plotting */
  override def toEnclosure: Enclosure = {
    val lo = low
    val hi = high
    val left = domain.low
    val right = domain.high
    val loLeft: Double = lo(left).loDouble
    val hiLeft: Double = hi(left).hiDouble
    val loRight: Double = lo(right).loDouble
    val hiRight: Double = hi(right).hiDouble
    Enclosure(loLeft, hiLeft, loRight, hiRight)
  }

}

// TODO improve how the plotting is done
object PaddedUnivariateAffineScalarEnclosure {

  def apply(e: UnivariateAffineScalarEnclosure, padding: Interval => Interval): PaddedUnivariateAffineScalarEnclosure =
    PaddedUnivariateAffineScalarEnclosure(
      e.domain,
      e.normalizedDomain,
      e.constant,
      e.coefficient,
      padding)

  /** Convenience method, normalizes the domain. */
  //  private[enclosure] 
  def apply(domain: Interval, constant: Interval, coefficient: Interval, padding: Interval => Interval): PaddedUnivariateAffineScalarEnclosure =
    PaddedUnivariateAffineScalarEnclosure(
      domain, 0 /\ domain.width.high,
      constant,
      coefficient,
      padding)

  /** Lifts a constant interval to a constant enclosure. */
  def apply(domain: Interval, constant: Interval, padding: Interval => Interval): PaddedUnivariateAffineScalarEnclosure = {
    PaddedUnivariateAffineScalarEnclosure(
      domain,
      constant,
      0,
      padding)
  }

}
