package acumen.interpreters.enclosure.affine

import acumen.interpreters.enclosure.Rounding
import acumen.interpreters.enclosure.Types._
import acumen.interpreters.enclosure.Interval
import acumen.interpreters.enclosure.Box

/**
 * Type to represent vector-valued functions of a single
 * variable where the value is determined by a function
 * approximated by the super type UnivariateAffineEnclosure
 * and an additional padding.
 *
 * TODO: add note on padding!
 *
 * Implementation notes: see the implementation notes for
 * UnivariateAffineEnclosure.
 */
case class PaddedUnivariateAffineEnclosure private[affine] (
  override val domain: Interval,
  override val normalizedDomain: Interval,
  override val components: Map[VarName, PaddedUnivariateAffineScalarEnclosure],
  padding: Interval => Interval)(implicit rnd: Rounding)
    extends UnivariateAffineEnclosure {

  override def rounding = rnd

  /** The low bound enclosure of this enclosure. */
  override def low =
    PaddedUnivariateAffineEnclosure(
      domain,
      normalizedDomain,
      components.mapValues(_.low),
      padding)

  /** The high bound enclosure of this enclosure. */
  override def high =
    PaddedUnivariateAffineEnclosure(
      domain,
      normalizedDomain,
      components.mapValues(_.high),
      padding)

  override def isConstant = components.values.forall(_.isConstant)

  /** Get the "name" component of the enclosure. */
  override def apply(name: VarName): PaddedUnivariateAffineScalarEnclosure = components(name)

  override def varNames = components.keys

  /**
   * Evaluate the enclosure at the interval x.
   *
   * Precondition: x must be contained within the domain to the enclosure.
   */
  override def apply(x: Interval): Box = {
    assert(domain contains x, "Enclosures must be evaluated over sub-intervals of their domain.")
    /* Since the enclosure is represented over the normalizedDomain, the argument interval must 
     * be translated into the normalizedDomain. */
    components.mapValues(_(x))
  }

  /**
   * Get the range box of the enclosure.
   *
   * Since the enclosure is a safe approximation of any contained function
   * the range also safely approximates the range of any such function.
   */
  override def range(implicit rnd: Rounding): Box = this(domain)

  /** Component-wise containment of enclosures. */
  override def contains(that: UnivariateAffineEnclosure)(implicit rnd: Rounding): Boolean = {
    assert(domain == that.domain, "Containment can only be tested for enclosures with equal domains.")
    assert(components.keySet == that.components.keySet, "Containment can only be tested for enclosures with equal component names.")
    components.forall { case (name, component) => component contains that.components(name) }
  }

  //TODO Update the below comment.

  /**
   * Compute the union of the enclosures.
   *
   * Implementation note: see the implementation note for union on univariate scalar enclosures.
   * Precondition: Note that this assumes that the domain interval is not thin!
   */
  override def union(that: UnivariateAffineEnclosure): UnivariateAffineEnclosure = {
    assert(this.domain == that.domain, "Union can only be taken of enclosures over the same domain.")
    assert(components.keySet == that.components.keySet, "Union can only be taken with enclosures with equal component names.")
    UnivariateAffineEnclosure(domain, normalizedDomain,
      components.map { case (name, component) => name -> (component union (that.components(name))) })
  }

  override def restrictTo(subDomain: Interval)(implicit rnd: Rounding): UnivariateAffineEnclosure = {
    require((domain contains subDomain) && (normalizedDomain contains Interval(0) /\ subDomain.width.high))
    PaddedUnivariateAffineEnclosure(subDomain, components.mapValues(_.restrictTo(subDomain)), padding)
  }

}
object PaddedUnivariateAffineEnclosure {

  def apply(e: UnivariateAffineEnclosure, padding: Interval => Interval): PaddedUnivariateAffineEnclosure =
    PaddedUnivariateAffineEnclosure(
      e.domain,
      e.normalizedDomain,
      e.components.mapValues(PaddedUnivariateAffineScalarEnclosure(_, padding)),
      padding)(e.rounding)

  /** Convenience method, normalizes the domain. */
  def apply(domain: Interval, components: Map[VarName, PaddedUnivariateAffineScalarEnclosure], padding: Interval => Interval)(implicit rnd: Rounding): UnivariateAffineEnclosure =
    PaddedUnivariateAffineEnclosure(
      domain,
      Interval(0) /\ domain.width.high,
      components,
      padding)

}
