package acumen.interpreters.enclosure

import Interval._
import acumen.ui.interpreter.Enclosure
import java.awt.Color

/**
 * Type to represent functions of a single variable.
 *
 * Implementation notes: see the implementation notes for
 * AffineScalarEnclosure.
 */
case class UnivariateAffineScalarEnclosure private[enclosure] (
  //  private[enclosure]
  domain: Interval,
  private[enclosure]normalizedDomain: Interval,
  private[enclosure]constant: Interval,
  private[enclosure]coefficient: Interval)(implicit rnd: Rounding) {
  assert(normalizedDomain.low equalTo 0,
    "The low end-point of the normalizedDomain should be zero!")

  /** The low bound enclosure of this enclosure. */
  def low = UnivariateAffineScalarEnclosure(domain, normalizedDomain, constant.low, coefficient.low)

  /** The high bound enclosure of this enclosure. */
  def high = UnivariateAffineScalarEnclosure(domain, normalizedDomain, constant.high, coefficient.high)

  def isConstant = coefficient equalTo 0

  // FIXME commented out assertion FOR_NOW to be able to plot
  /**
   * Evaluate the enclosure at the interval x.
   *
   * Precondition: x must be contained within the domain to the enclosure.
   */
  def apply(x: Interval) = {
    //    assert(domain contains x, 
    //        "Enclosures must be evaluated over sub-intervals of their domain." +
    //        "\ndomain:          " + domain + 
    //        "\nx:               " + x + 
    //        "domain contains x: " + (domain contains x))
    /* Since the enclosure is represented over the normalizedDomain, the argument interval must 
     * be translated into the normalizedDomain. */
    val (xlo, xhi) = (x - domain.low).bounds
    val lo = low
    val hi = high
    min(lo evalThinAtThin xlo, lo evalThinAtThin xhi) /\ max(hi evalThinAtThin xlo, hi evalThinAtThin xhi)
  }

  // FIXME commented out assertion FOR_NOW to be able to plot
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
  def range(implicit rnd: Rounding): Interval = this(domain)

  /**
   * Containment of enclosures.
   *
   * Note: This implementation relies on the fact that "this" and
   * "that" are affine enclosures, meaning that containment can
   * be decided by comparing the upper and lower bounds of the
   * enclosures at the domain's end-points.
   */
  def contains(that: UnivariateAffineScalarEnclosure)(implicit rnd: Rounding) = {
    assert(this.domain == that.domain, "Containment is only defined for enclosures over the same domain.")
    val lo = domain.low
    val hi = domain.high
    val thisLo = this.low
    val thatLo = that.low
    val thisHi = this.high
    val thatHi = that.high
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
  def union(that: UnivariateAffineScalarEnclosure) = {
    assert(this.domain == that.domain, "Union can only be taken of enclosures over the same domain.")
    val lo = domain.low
    val hi = domain.high
    if (lo == hi)
      UnivariateAffineScalarEnclosure(domain, normalizedDomain, this(lo) /\ that(lo), Interval(0))
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
      UnivariateAffineScalarEnclosure(domain, normalizedDomain, minAtLo /\ maxAtLo, coeffMin /\ coeffMax)
    }
  }

  /**
   * Shorter version of union. 
   * 
   * FIXME: should wrap thinning unions in non-widening ones!
   */
  def newUnion(that: UnivariateAffineScalarEnclosure) = {
    assert(this.domain == that.domain, "Union can only be taken of enclosures over the same domain.")
    val domLow = domain.low
    val domHigh = domain.high
    if (domLow equalTo domHigh)
      UnivariateAffineScalarEnclosure(domain, normalizedDomain, this.constant /\ that.constant, Interval(0))
    else {
      val resConstant = this.constant /\ that.constant
      val resAtDomHigh = this(domHigh) /\ that(domHigh)
      val resCoefficientLow = resAtDomHigh.low - resConstant.low
      val resCoefficientHigh = resAtDomHigh.high - resConstant.high
      val resCoefficient = resCoefficientLow /\ resCoefficientHigh
      UnivariateAffineScalarEnclosure(domain, normalizedDomain, resConstant, resCoefficient)
    }
  }

  /**
   * The restriction of `this` enclosure to a sub-interval of its domain.
   *
   * Returns an enclosure with the same constant and coefficient as `this`
   * enclosure, redefined over a sub-interval of its domain.
   */
  //TODO Add property
  def restrictTo(subDomain: Interval)(implicit rnd: Rounding): UnivariateAffineScalarEnclosure = {
    require((domain contains subDomain) && (normalizedDomain contains 0 /\ subDomain.width.high))
    UnivariateAffineScalarEnclosure(subDomain, 0 /\ subDomain.width.high, constant, coefficient)
  }

  /* Arithmetic operations */

  /** Negation of enclosures. */
  def unary_-(implicit rnd: Rounding) =
    UnivariateAffineScalarEnclosure(domain, normalizedDomain, -constant, -coefficient)

  /** Addition of enclosures. */
  def +(that: UnivariateAffineScalarEnclosure)(implicit rnd: Rounding) = {
    assert(this.domain == that.domain, "Only enclosures over the same domain can be added.")
    UnivariateAffineScalarEnclosure(
      domain,
      normalizedDomain,
      constant + that.constant,
      coefficient + that.coefficient)
  }

  /** Subtraction of enclosures. */
  def -(that: UnivariateAffineScalarEnclosure)(implicit rnd: Rounding) = {
    assert(this.domain == that.domain, "Only enclosures over the same domain can be subtracted.")
    UnivariateAffineScalarEnclosure(
      domain,
      normalizedDomain,
      constant - that.constant,
      coefficient - that.coefficient)
  }

  /**
   * Multiplication of enclosures.
   *
   * Implementation note: to approximate the product (a+b*t)*(c+d*t) of two enclosures in the variable t we:
   * (i)   translate the product from [l,h] to [-1,1] using t => m+r*t, with m = (l+h)/2 and r = (h-l)/2
   * (ii)  reduce the quadratic term t*t to [0,1]
   * (iii) translate the resulting affine enclosure to [0,h-l] using t => -1+t/r
   */
  def *(that: UnivariateAffineScalarEnclosure)(implicit rnd: Rounding) = {
    assert(this.domain == that.domain, "Only enclosures over the same domain can be multiplied.")
    val (l, h) = domain.bounds // domain of t
    val r = (h - l) / 2 // domain radius
    val m = (l + h) / 2 // domain midpoint
    val a = this.constant // this = a+b*t
    val b = this.coefficient
    val c = that.constant // this = c+d*t
    val d = that.coefficient
    val e = a + b * r
    val f = c + d * r
    val coeff = e * d + f * b
    val const = e * f + b * d * r * r * Interval(0, 1) - coeff * r
    UnivariateAffineScalarEnclosure(domain, normalizedDomain, const, coeff)
  }

  /**
   * Division of enclosures.
   *
   * Precondition: "that" must currently be a constant enclosure.
   *
   * Implementation note: TODO
   */
  def /(that: UnivariateAffineScalarEnclosure)(implicit rnd: Rounding): UnivariateAffineScalarEnclosure = {
    assert(this.domain == that.domain, "Only enclosures over the same domain can be divided.")
    assert(that.coefficient equalTo 0, "Only constant enclosures can currently be divisors.")
    UnivariateAffineScalarEnclosure(
      domain,
      normalizedDomain,
      constant / that.constant,
      coefficient / that.coefficient)
  }
  def /(that: Interval)(implicit rnd: Rounding): UnivariateAffineScalarEnclosure =
    UnivariateAffineScalarEnclosure(domain, normalizedDomain, constant / that, coefficient / that)

  // TODO improve description
  /** For plotting */
  def toEnclosure: Enclosure = {
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
object UnivariateAffineScalarEnclosure {

  /** Convenience method, normalizes the domain. */
  //  private[enclosure] 
  def apply(domain: Interval, constant: Interval, coefficient: Interval)(implicit rnd: Rounding): UnivariateAffineScalarEnclosure =
    UnivariateAffineScalarEnclosure(domain, 0 /\ domain.width.high, constant, coefficient)

  /** Lifts a constant interval to a constant enclosure. */
  def apply(domain: Interval, constant: Interval)(implicit rnd: Rounding): UnivariateAffineScalarEnclosure = {
    UnivariateAffineScalarEnclosure(domain, constant, 0)
  }

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

  // def plot(them: (Color, UnivariateAffineScalarEnclosure)*)(implicit rnd: Rounding): Unit =
  //   plot("Picard plotter")(them: _*)

  // private def plot(frametitle: String)(them: (Color, UnivariateAffineScalarEnclosure)*)(implicit rnd: Rounding) = {
  //   val f = createFrame(frametitle)
  //   for ((color, it) <- them) {
  //     def low(t: Double) = it.low(t) match { case Interval(lo, _) => lo.doubleValue }
  //     def high(t: Double) = it.high(t) match { case Interval(_, hi) => hi.doubleValue }
  //     val dom = it.domain
  //     val (lo, hi) = dom match { case Interval(l, h) => (l.doubleValue, h.doubleValue) }
  //     addEnclosure(lo, hi, high, low, 0, color, "", AbstractFrame.wrap(f), null)
  //   }
  //   f.pack
  // }

}
