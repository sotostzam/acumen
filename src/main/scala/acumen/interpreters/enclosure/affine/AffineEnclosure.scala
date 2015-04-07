package acumen.interpreters.enclosure.affine

import acumen.interpreters.enclosure.Types._
import acumen.interpreters.enclosure.Interval
import acumen.interpreters.enclosure.Box

/**
 * Type used to approximate vector-valued functions over a domain.
 *
 * Implementation note: see the implementation note for AffineScalarEnclosure.
 */
case class AffineEnclosure private[enclosure] (
  private[enclosure]domain: Box,
  private[enclosure]normalizedDomain: Box,
  //TODO check if it would help to extend MapProxy here as in Box.!
  private[enclosure]components: Map[VarName, AffineScalarEnclosure]) {
  assert(components.forall { case (_, e) => domain == e.domain },
    "The domain of the affine enclosure must conicide with the domain " +
      "of each component affine scalar enclosure.")
  assert(components.forall { case (_, e) => normalizedDomain == e.normalizedDomain },
    "The normalizedDomain of the affine enclosure must conicide with the " +
      "normalizedDomain of each component affine scalar enclosure.")

  /** Get the "name" component of the enclosure. */
  def apply(name: VarName): AffineScalarEnclosure = components(name)

  /** Get the constant box of the enclosure. */
  def constantTerm =
    AffineEnclosure(domain, components.keys.map { n => (n, components(n).constant) }.toMap)

  /** Get the linear term of the enclosure. */
  def linearTerms =
    AffineEnclosure(domain, components.keys.map { n => (n, components(n).linearTerms) }.toMap)

  /**
   * Evaluate the enclosure at the box x.
   *
   * Precondition: the box must have a domain for each coefficient name.
   *
   * Implementation note: see the implementation note for
   * AffineScalarEnclosure.apply.
   */
  def apply(x: Box): Box = {
    assert(components.forall { case (_, component) => component.coefficients.keySet subsetOf x.keySet },
      "An enclosure can only be evaluated over a box that has a domain for each coefficients variable.")
    components.mapValues(_(x))
  }

  /**
   * Get the range box of the enclosure.
   *
   * Since the enclosure is a safe approximation of any contained function
   * the range box also safely approximates the range of any such function.
   */
  def range: Box = this(domain)

  /**
   * Produce an enclosure without the variable "name" and that approximates
   * this enclosure.
   *
   * Implementation note: see the implementation note for AffineScalarEnclosure.collapse.
   */
  private def collapseName(name: VarName) = {
    val collapsedDomain = domain - name
    val collapsedNormalizedDomain = normalizedDomain - name
    AffineEnclosure(collapsedDomain, collapsedNormalizedDomain, components.mapValues(_.collapse(name)))
  }

  def collapse(names: VarName*): AffineEnclosure = {
    val nl = names.toList
    if (nl.size > 5) collapseFast(nl)
    else nl.foldLeft(this)((res, name) => res.collapseName(name))
  }

  /** Implementation based on (encapsulated) mutable data structures. */
  def collapseFast(names: List[VarName]): AffineEnclosure = {
    val zero = Interval.zero
    val boxCache = collection.mutable.Map[Box, Box]()
    def collapseNames(b: Box) = boxCache.getOrElseUpdate(b, 
      ((collection.mutable.Map[VarName, Interval]() ++ b) -- names).toMap)
    AffineEnclosure( 
      collapseNames(domain),
      collapseNames(normalizedDomain),
      components.mapValues { c =>
        AffineScalarEnclosure( collapseNames(c.domain)
                             , collapseNames(c.normalizedDomain)
                             , names.foldLeft(c.constant)((con, n) =>
                                 con + c.coefficients.getOrElse(n, zero) * 
                                       c.normalizedDomain.getOrElse(n, zero))
                             , collapseNames(c.coefficients) )
      })
  }

  /**
   * Component-wise containment of enclosures.
   *
   * Implementation note: see the implementation note for AffineScalarEnclosure.contains.
   */
  def contains(that: AffineEnclosure) = {
    assert(components.keySet == that.components.keySet,
      "The component names of this and that enclosure have to coincide.")
    components.keys.forall(name => components(name) contains that.components(name))
  }

  /** Pads each component of the enclosure by delta. The result is an enclosure that contains this enclosure. */
  def plusMinus(delta: Interval) = map(_.plusMinus(delta))
  def plusMinus(delta: Double): AffineEnclosure =
    this plusMinus Interval(delta)

  /* Arithmetic operations */

  /** Negation of enclosures. */
  def unary_-():AffineEnclosure = map(-_)

  /** Addition of enclosures. */
  def +(that: AffineEnclosure): AffineEnclosure = zipWith(_ + _)(that)
  def +(that: Interval): AffineEnclosure = this + that
  def +(that: Double): AffineEnclosure = this + Interval(that)
  def +(that: Int): AffineEnclosure = this + Interval(that)

  /** Subtraction of enclosures. */
  def -(that: AffineEnclosure): AffineEnclosure = zipWith(_ - _)(that)
  def -(that: Interval): AffineEnclosure = this + that
  def -(that: Double): AffineEnclosure = this - Interval(that)
  def -(that: Int): AffineEnclosure = this - Interval(that)

  /** Multiplication of enclosures. */
  def *(that: AffineEnclosure): AffineEnclosure = zipWith(_ * _)(that)
  def *(that: Interval): AffineEnclosure = this * that
  def *(that: Double): AffineEnclosure = this * Interval(that)
  def *(that: Int): AffineEnclosure = this * Interval(that)

  /** Division of enclosures. */
  def /(that: Interval) = map(_ / that)
  def /(that: Double): AffineEnclosure = this / Interval(that)
  def /(that: Int): AffineEnclosure = this / Interval(that)

  /**
   * An enclosure for the component-wise primitive function (w.r.t. "name") of this enclosure
   * over the domain, normalized to be 0 at the low end-point of the domain of "name".
   */
  def primitive(name: VarName) = {
    assert(domain.contains(name),
      "Primitive function must be taken w.r.t. a variable that is present in the domain of the enclosure.")
    map(_.primitive(name))
  }

  /**
   * Creates a new enclosure by applying the function f component-wise to the components
   * the enclosure.
   */
  // TODO add property
  private def map(f: AffineScalarEnclosure => AffineScalarEnclosure) =
    AffineEnclosure(domain, normalizedDomain, components.mapValues(f))

  /**
   * Creates a new enclosure by applying the function f component-wise to the components of
   * "this" and "that" enclosure. In the cases when one of the enclosures does not contain a
   * variable which the other does, the constant zero scalar enclosure is used as a default.
   */
  // TODO add property
  private def zipWith(f: (AffineScalarEnclosure, AffineScalarEnclosure) => AffineScalarEnclosure)(that: AffineEnclosure) = {
    val zero = AffineScalarEnclosure(domain, 0)
    val names = components.keySet union that.components.keySet
    val comps = names.map { name => name -> f(components.getOrElse(name, zero), that.components.getOrElse(name, zero)) }
    AffineEnclosure(domain, normalizedDomain, comps.toMap)
  }

  /**
   * Returns an enclosure with the same affine interval functions as the enclosure, defined
   * over a sub-box of the domain.
   */
  def restrictTo(subDomain: Box): AffineEnclosure = {
    assert(domain contains subDomain,
      "Restriction is only defined for subDomains that are sub-boxes of the enclsure's domain.")
    map(_.restrictTo(subDomain))
  }

}

object AffineEnclosure {

  /** Convenience method, normalizes the domain. */
  private[enclosure] def apply(domain: Box, components: Map[VarName, AffineScalarEnclosure]): AffineEnclosure =
    AffineEnclosure(domain, Box.normalize(domain), components)

  /** Lifts a constant interval box to a constant enclosure. */
  def apply(domain: Box, constant: Box): AffineEnclosure = {
    AffineEnclosure(domain, constant.mapValues(interval => AffineScalarEnclosure(domain, interval)))
  }

  //TODO Check that the below comment is correct.
  /**
   * Lifts a collection "names" of variables in the domain to a thin enclosure consisting of identity
   * functions over the corresponding box.
   */
  def apply(domain: Box, names: VarName*): AffineEnclosure = {
    assert(names.forall(name => domain contains name),
      "Projecting is only possible for variables in the domain.")
    /* Implementation note: The constant term needs to be domain(name).low 
     * because the internal representation is over the normalized domain. */
    AffineEnclosure(domain, names.map(name => name -> AffineScalarEnclosure(domain, name)).toMap)
  }

}
