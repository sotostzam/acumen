package acumen.interpreters.enclosure

import Interval._
import Types._
import Util._
import org.jfree.ui.ApplicationFrame
import java.awt.Color

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
  private[enclosure]domain: Box,
  /* To save wasteful shifting during enclosure operations the internal
   * representation of the domain is such that each variable is non-negative.
   * 
   * Implementation note: this will make it possible to e.g. compute bounds 
   * of the enclosure by simply taking the corresponding bounds of the 
   * constant term and coefficients. */
  private[enclosure]normalizedDomain: Box,
  private[enclosure]constant: Interval,
  private[enclosure]coefficients: Box) {
  assert(coefficients.keySet subsetOf domain.keySet, "The variable of each coefficient must occur in the domain.")
  assert(domain.keySet == normalizedDomain.keySet, "The variables of the normalized domain must coincide with those of the domain.")

  /** The number of variables the enclosure depends on. */
  def arity = coefficients.size

  /** The number of variables in the domain of the enclosure. */
  def dimension = domain.size

  /** The low bound enclosure of this enclosure. */
  def low = AffineScalarEnclosure(domain, normalizedDomain, constant.low, coefficients.mapValues(_.low))

  /** The high bound enclosure of this enclosure. */
  def high = AffineScalarEnclosure(domain, normalizedDomain, constant.high, coefficients.mapValues(_.high))

  //  /** Compose `that` enclosure into variable `name` in `this` enclosure. */
  //  // TODO first simple version, improve using monotonicity analysis!
  //  def compose(name: VarName, that: AffineScalarEnclosure)(implicit rnd: Rounding): AffineScalarEnclosure = {
  //    require(domain.keySet contains name, "The composition variable must be in the domain of the outer enclosure!")
  //    require(domain(name) contains that.range, "The inner enclosure must map into the domain of the composition variable!")
  //    require((domain.keySet.intersect(that.domain.keySet) - name).forall(n => domain(n).equalTo(that.domain(n))),
  //      "The domains of shared variables of the composed enclosures must conincide!")
  //    if (coefficients contains name) {
  //      val coeff = coefficients(name)
  //      val dom = 
  //    } else this
  //  }

  /**
   * Get the linear terms of this enclosure.
   *
   * Implementation note: this is achieved buy setting the constant term to [0,0].
   */
  def linearTerms(implicit end: Rounding) = AffineScalarEnclosure(domain, normalizedDomain, 0, coefficients)

  /** Get the constant term of this enclosure. */
  def constantTerm(implicit end: Rounding) = AffineScalarEnclosure(domain, normalizedDomain, constant, Box())

  // TODO write tests!
  // TODO add better description!
  /**
   * Contracts the domain by back-propagating the interval ran.
   *
   * The algorithm uses HC4 style back-propagation, see "Revising hull and box consistency"
   * by Benhamou et al, which contracts the domains of variables in an expression using the
   * constraint that the value of the expression should lie within the ran interval.
   */
  def contractDomain(desiredRange: Interval)(implicit rnd: Rounding) = {
    def contractDom(name: VarName): Interval = {
      //      println("--------------")
      //      println("VarName:       " + name)

      val contractedRange = range \/ desiredRange
      val nameDomain = normalizedDomain(name)
      val nameCoefficient = coefficients.getOrElse(name, Interval(0))
      // The name term is equal to the range minus the other terms
      val nameTerm = -AffineScalarEnclosure(domain, normalizedDomain, constant - contractedRange, coefficients - name).range
      val contrctedNameTerm = nameTerm \/ (nameCoefficient * nameDomain)
      if (nameCoefficient isZero) normalizedDomain(name)
      else normalizedDomain(name) \/ (contrctedNameTerm / nameCoefficient)
    }
    //    println("desired range: " + desiredRange)
    //    println("actual range:  " + this.range)
    //    println("coefficients:  " + this.coefficients)
    //    if (range disjointFrom desiredRange) domain
    //    else
    normalizedDomain.keys.foldLeft(normalizedDomain) { case (box, name) => box + (name -> (contractDom(name) + domain(name).low)) }
  }

  /**
   * Enclose the union of the two enclosures.
   *
   * Precondition: both enclosures have to have dimension 1.
   */
  def union(that: AffineScalarEnclosure)(implicit rnd: Rounding) = {
    assert(this.domain == that.domain, "Union can only be taken of enclosures over the same domain.")
    assert(this.dimension == that.dimension, "Union is only implemented for enclosures of dimension 1.")
    val Seq(lo, hi) = domain.corners
    if (lo == hi) this(lo) /\ that(lo)
    else {
      val thisLo = this.low
      val thatLo = that.low
      val thisHi = this.high
      val thatHi = that.high
      val minAtLo = min(thisLo(lo), thatLo(lo))
      val maxAtLo = max(thisHi(lo), thatHi(lo))
      val minAtHi = min(thisLo(hi), thatLo(hi))
      val maxAtHi = max(thisHi(hi), thatHi(hi))
      val width = domain.values.head.width
      val coeffLo = (minAtHi - minAtLo) / width
      val coeffHi = (maxAtHi - maxAtLo) / width
      // FIXME ...
    }
  }

  /**
   * Evaluate the enclosure at the box x.
   *
   * Precondition: the box must have a domain for each coefficient name.
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
  def apply(x: Box)(implicit rnd: Rounding) = {
    assert(coefficients.keySet subsetOf x.keySet,
      "An enclosure can only be evaluated over a box that has a domain for each variable.")
    assert(x.forall { case (name, interval) => domain(name) contains interval },
      "The argument must be contained in the domain.")

    /* It is essential to evaluate the enclosure over the original domain.
     * To avoid unnecessary errors the argument is shifted to the normalized
     * domain rather than the enclosure to the original domain. 
     * 
     * Implementation note: cannot use normalize because it does not take the 
     * current domain of the enclosure into consideration. E.g. normalize maps
     * thin intervals to [0,0]! */
    val nx = x.map { case (name, value) => name -> (value - (domain(name).low)) }
    //    lazy val wastefulUnnecessaryComputation = {
    //      val c :: cs = Box.corners(nx)
    //      val lo = low
    //      val hi = high
    //      cs.foldLeft((lo evalThinAtThin c) /\ (hi evalThinAtThin c)) {
    //        case (res, corner) => res /\ (lo evalThinAtThin corner) /\ (hi evalThinAtThin corner)
    //      }
    //    }
    //    wastefulUnnecessaryComputation
    evalThinAtThin(nx)
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
  private def evalThinAtThin(x: Box)(implicit rnd: Rounding): Interval = {
    assert(x.forall { case (name, interval) => normalizedDomain(name) contains interval },
      "The argument must be contained in the normalized domain.")
    coefficients.foldLeft(constant) { case (res, (name, coeff)) => res + coeff * x(name) }
  }

  /**
   * Get the range of the enclosure.
   *
   * Since the enclosure is a safe approximation of any contained function
   * the range also safely approximates the range of any such function.
   */
  def range(implicit rnd: Rounding): Interval = this(domain)

  /**
   * Produce an enclosure without the variable "name" that approximates this enclosure.
   *
   * Implementation note: the domain of name is assumed to be non-negative.
   *
   * property: (monotonicity of domain collapsing): The enclosure which is being collapsed
   * must be point-wise included in the enclosure obtained by collapsing.
   */
  private def collapse(name: VarName)(implicit rnd: Rounding) =
    AffineScalarEnclosure(
      domain - name,
      normalizedDomain - name,
      constant + coefficients.getOrElse(name, Interval(0)) * normalizedDomain.getOrElse(name, Interval(0)),
      coefficients - name)

  def collapse(names: VarName*)(implicit rnd: Rounding): AffineScalarEnclosure =
    names.foldLeft(this)((res, name) => res.collapse(name))

  /**
   * Containment of enclosures.
   *
   * Implementation note: representing enclosures over normalized domains
   * allows us to test containment by constant- and coefficient-wise
   * containment.
   */
  def contains(that: AffineScalarEnclosure)(implicit rnd: Rounding) = {
    assert(this.domain == that.domain,
      "Containment is only defined for enclosures over the same domain.")
    ((this.low - that.low).range lessThanOrEqualTo 0) &&
      ((that.high - this.high).range lessThanOrEqualTo 0)
    //    /* We take the corners of the normalized domain as that is where we will
    //     * be checking for containments. */
    //    val thisLo = this.low
    //    val thatLo = that.low
    //    val thisHi = this.high
    //    val thatHi = that.high
    //    Box.corners(normalizedDomain).forall { c =>
    //      val thisBoundsThatAtCorner = // Ensure that "this" bounds "that" at this corner
    //        (thisLo.evalThinAtThin(c) lessThanOrEqualTo (thatLo.evalThinAtThin(c))) &&
    //          (thatHi.evalThinAtThin(c) lessThanOrEqualTo (thisHi.evalThinAtThin(c)))
    //      thisBoundsThatAtCorner
    //    }
  }

  /** Pads the enclosure by delta. The result is an enclosure that contains this enclosure. */
  def plusMinus(delta: Interval)(implicit rnd: Rounding) = this + (-delta) /\ delta
  def plusMinus(delta: Double)(implicit rnd: Rounding): AffineScalarEnclosure =
    this plusMinus Interval(delta)

  /* Arithmetic operations */

  /** Negation of enclosures. */
  def unary_-(implicit r: Rounding): AffineScalarEnclosure =
    AffineScalarEnclosure(domain, normalizedDomain, -constant, coefficients.mapValues(-_))

  /**
   * Creates a new enclosure by applying a binary operator to the values of "this" and "that" enclosure.
   * In the cases when one of the enclosures does not contain a variable which the other does, the
   * interval [0,0] is used as a default.
   */
  private def zipWith(f: (Interval, Interval) => Interval)(that: AffineScalarEnclosure)(implicit rnd: Rounding) =
    AffineScalarEnclosure(domain, normalizedDomain,
      f(constant, that.constant),
      (this.coefficients.keySet union that.coefficients.keySet).map { k =>
        (k, f(
          this.coefficients.getOrElse(k, Interval(0)),
          that.coefficients.getOrElse(k, Interval(0))))
      }.toMap)

  /** Addition of enclosures. */
  def +(that: AffineScalarEnclosure)(implicit rnd: Rounding): AffineScalarEnclosure = zipWith(_ + _)(that)
  def +(that: Interval)(implicit rnd: Rounding): AffineScalarEnclosure = AffineScalarEnclosure(domain, normalizedDomain, constant + that, coefficients)
  def +(that: Double)(implicit rnd: Rounding): AffineScalarEnclosure = this + Interval(that)
  def +(that: Int)(implicit rnd: Rounding): AffineScalarEnclosure = this + Interval(that)

  /** Subtraction of enclosures. */
  def -(that: AffineScalarEnclosure)(implicit rnd: Rounding): AffineScalarEnclosure = zipWith(_ - _)(that)
  def -(that: Interval)(implicit rnd: Rounding): AffineScalarEnclosure = AffineScalarEnclosure(domain, normalizedDomain, constant - that, coefficients)
  def -(that: Double)(implicit rnd: Rounding): AffineScalarEnclosure = this - Interval(that)
  def -(that: Int)(implicit rnd: Rounding): AffineScalarEnclosure = this - Interval(that)

  // TODO update the description
  /**
   * Multiplication of enclosures.
   *
   * The case of multiplying two enclosures is computed as follows:
   *
   * (a_0 + a_1*x_1 + ... + a_n*x_n) * (b_0 + b_1*x_1 + ... + b_n*x_n)
   * ==
   * a_0*b_0 +
   * a_0*sum{ b_i*x_i | 1 <= i <= n } + b_0*sum{ a_i*x_i | 1 <= i <= n } +
   * sum{ a_i*b_i*x_i^2 | 1 <= i <= n } +
   * sum{ a_i*b_j*x_i*x_j | 1 <= i,j <= n and i != j }
   */
  def *(that: AffineScalarEnclosure)(implicit rnd: Rounding): AffineScalarEnclosure = {
    assert(domain == that.domain, "Multiplication is defined on enclosures with identical domains.")
    val const = constant * (that.constant)
    val linear = (that.linearTerms * constant) + (this.linearTerms * that.constant)
    val mixeds =
      for ((name1, coeff1) <- (this.linearTerms.coefficients); (name2, coeff2) <- (that.linearTerms.coefficients))
        yield AffineScalarEnclosure.mixed(normalizedDomain, name1, name2) * coeff1 * coeff2
    mixeds.foldLeft(linear + const)(_ + _)
  }
  def *(that: Interval)(implicit rnd: Rounding) = AffineScalarEnclosure(domain, normalizedDomain, constant * that, coefficients.mapValues(_ * that))
  def *(that: Double)(implicit rnd: Rounding): AffineScalarEnclosure = this * Interval(that)
  def *(that: Int)(implicit rnd: Rounding): AffineScalarEnclosure = this * Interval(that)

  /** Division of enclosures. */
  def /(that: Interval)(implicit rnd: Rounding) = AffineScalarEnclosure(domain, normalizedDomain, constant / that, coefficients.mapValues(_ / that))
  def /(that: Double)(implicit rnd: Rounding): AffineScalarEnclosure = this / Interval(that)
  def /(that: Int)(implicit rnd: Rounding): AffineScalarEnclosure = this / Interval(that)

  /**
   * An enclosure for the primitive function (w.r.t. "name") of this enclosure over the
   * domain, normalized to be 0 at the low end-point of the domain of "name".
   */
  def primitive(name: VarName)(implicit rnd: Rounding) = {
    require(domain.contains(name))
    val coeff = coefficients.getOrElse(name, Interval(0))
    val quadr = AffineScalarEnclosure.quadratic(normalizedDomain, name) * (coeff / 2)
    val prods = (coefficients - name).map { case (n, c) => AffineScalarEnclosure.mixed(normalizedDomain, name, n) * c }
    val nonlinear = prods.foldLeft(quadr) { case (res, f) => res + f }
    val linear = (AffineScalarEnclosure(normalizedDomain, name) * constant)
    val nonconst = linear + nonlinear
    AffineScalarEnclosure(domain, normalizedDomain, nonconst.constant, nonconst.coefficients)
  }

  /** Returns an enclosure with the same affine interval function as the enclosure, defined over a sub-box of the domain. */
  //TODO Add property
  def restrictTo(subDomain: Box)(implicit rnd: Rounding): AffineScalarEnclosure = {
    require((domain contains subDomain) &&
      ((Box.normalize(domain) contains Box.normalize(subDomain))))
    AffineScalarEnclosure(subDomain, constant, coefficients)
  }

  private def quadratic(name: VarName)(implicit rnd: Rounding) = AffineScalarEnclosure.quadratic(domain, name)
  private def mixed(name1: VarName, name2: VarName)(implicit rnd: Rounding) = AffineScalarEnclosure.mixed(domain, name1, name2)

}

// TODO improve how the plotting is done
object AffineScalarEnclosure {

  /** Convenience method, normalizes the domain. */
  private[enclosure] def apply(domain: Box, constant: Interval, coefficients: Box)(implicit rnd: Rounding): AffineScalarEnclosure =
    AffineScalarEnclosure(domain, Box.normalize(domain), constant, coefficients)

  /** Lifts a constant interval to a constant enclosure. */
  def apply(domain: Box, constant: Interval)(implicit rnd: Rounding): AffineScalarEnclosure = {
    AffineScalarEnclosure(domain, constant, Box.empty)
  }

  /** Lifts a variable "name" in the domain to an identity function over the corresponding interval. */
  def apply(domain: Box, name: VarName)(implicit rnd: Rounding): AffineScalarEnclosure = {
    assert(domain contains name,
      "Projecting is only possible for variables in the domain.")
    /* Implementation note: The constant term needs to be domain(name).low 
     * because the internal representation is over the normalized domain. */
    AffineScalarEnclosure(domain, domain(name).low, Map(name -> Interval(1)))
  }

  /**
   * Degree reduction for "pure terms"
   *
   * map x^2 from [a,b] to [-1,1] using x => 0.25*((b-a)*x+a+b)^2
   * map t^2 => 0.5*(T_2+1) and then T_2 => [-1,1] (or just t^2 => [0,1])
   * map t from [-1,1] to [0,b-a] using t => (2*t+a-b)/(b-a)
   *
   * AffineIntervalFunction enclosure over doms of the quadratic monomial in variable name
   */
  def quadratic(domain: Box, name: VarName)(implicit rnd: Rounding) =
    mixed(domain, name, name) // TODO remove quadratic eventually
  //  {
  //    require(domain.contains(name))
  //    val (a, b) = domain(name).bounds
  //    val width = b - a
  //    val coeff = b + a
  //    // This corresponds to translating to [-1,1], representing in Chebyshev basis, 
  //    // collapsing the undesired (quadratic) term and translating to the normalized domain.
  //    val const = (0 /\ (width * width) / 4) - (width * coeff / 2) + (coeff * coeff / 4)
  //    AffineScalarEnclosure(domain, const, Box(name -> coeff))
  //  }

  /**
   * Enclose the mixed monomial in variables name1 and name2 over the domain.
   *
   * Implementation note: the approximation is obtained using degree reduction as follows:
   *
   * (i)   map variables x over [ax,bx] to [-1,1] using x => rx*x + mx
   * (ii)  collapse mixed terms c*x*y to c*[-1,1]
   * (iii) map variables x from [-1,1] to [0,bx-ax] using x => x/rx - 1
   *
   * where mx = (ax+bx)/2 is the mid-point of the domain of x
   * and   rx = (bx-ax)/2 is the radius of the domain of x.
   */
  def mixed(domain: Box, name1: VarName, name2: VarName)(implicit rnd: Rounding) = {
    val collapsedName1Name2 = (if (name1 == name2) 0 else -1) /\ 1 // approximation of the nonlinear term over [-1,1]x[-1,1]
    val (a1, b1) = domain(name1).bounds
    val (a2, b2) = domain(name2).bounds
    val m1 = (a1 + b1) / 2 // mid-point of domain of name1
    val r1 = (b1 - a1) / 2 // radius of domain of name1
    val m2 = (a2 + b2) / 2 // mid-point of domain of name2
    val r2 = (b2 - a2) / 2 // radius of domain of name2
    val const = -r1 * m2 - r2 * m1 + m1 * m2 + r1 * r2 * collapsedName1Name2
    if (name1 == name2) AffineScalarEnclosure(domain, const, Box(name1 -> 2 * m1))
    else AffineScalarEnclosure(domain, const, Box(name1 -> m2, name2 -> m1))
  }

}
