package acumen.interpreters.enclosure.affine

import acumen.interpreters.enclosure.Interval._
import acumen.interpreters.enclosure.Types._
import acumen.interpreters.enclosure.Util._
import org.jfree.ui.ApplicationFrame
import java.awt.Color
import acumen.interpreters.enclosure.Rounding
import acumen.interpreters.enclosure.Interval
import acumen.interpreters.enclosure.Box

/**
 * Type to represent vector-valued functions of a single variable.
 *
 * Implementation notes: see the implementation notes for
 * AffineEnclosure.
 */
abstract class UnivariateAffineEnclosure private[affine](implicit rnd: Rounding) {

  def domain: Interval
  private[affine] def normalizedDomain: Interval
  // private[enclosure] 
  def components: Map[VarName, UnivariateAffineScalarEnclosure]

  assert(normalizedDomain.low equalTo 0, "The low end-point of the normalizedDomain should be zero!")

  def rounding = rnd

  /** The low bound enclosure of this enclosure. */
  def low = UnivariateAffineEnclosure(domain, normalizedDomain, components.mapValues(_.low))

  /** The high bound enclosure of this enclosure. */
  def high = UnivariateAffineEnclosure(domain, normalizedDomain, components.mapValues(_.high))

  def isConstant = components.values.forall(_.isConstant)

  /** Get the "name" component of the enclosure. */
  def apply(name: VarName): UnivariateAffineScalarEnclosure = components(name)

  def varNames = components.keys

  /**
   * Evaluate the enclosure at the interval x.
   *
   * Precondition: x must be contained within the domain to the enclosure.
   */
  def apply(x: Interval): Box = {
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
  def range(implicit rnd: Rounding): Box = this(domain)

  /** Component-wise containment of enclosures. */
  def contains(that: UnivariateAffineEnclosure)(implicit rnd: Rounding): Boolean = {
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
  def union(that: UnivariateAffineEnclosure): UnivariateAffineEnclosure = {
    assert(this.domain == that.domain, "Union can only be taken of enclosures over the same domain.")
    assert(components.keySet == that.components.keySet, "Union can only be taken with enclosures with equal component names.")
    UnivariateAffineEnclosure(domain, normalizedDomain,
      components.map { case (name, component) => name -> (component union (that.components(name))) })
  }

  def restrictTo(subDomain: Interval)(implicit rnd: Rounding): UnivariateAffineEnclosure = {
    require((domain contains subDomain) && (normalizedDomain contains 0 /\ subDomain.width.high))
    UnivariateAffineEnclosure(subDomain, components.mapValues(_.restrictTo(subDomain)))
  }

  def improvesOn(that: UnivariateAffineEnclosure, at: Interval, improvement: Double)(implicit rnd: Rounding) = {
    val normOld = that(at).l1Norm
    val normNew = this(at).l1Norm
    normOld - normNew greaterThan improvement
  }

}

case class UnivariateAffineEnclosureImpl private[affine] (
    domain: Interval,
    private[affine]normalizedDomain: Interval,
    //  private[enclosure]
    components: Map[VarName, UnivariateAffineScalarEnclosure])(implicit rnd: Rounding) 
     extends UnivariateAffineEnclosure
{
  override def productPrefix = "UnivariateAffineEnclosure"
}

object UnivariateAffineEnclosure {

  def apply(domain: Interval,
            normalizedDomain: Interval,
            components: Map[VarName, UnivariateAffineScalarEnclosure])(implicit rnd: Rounding): UnivariateAffineEnclosure = 
    UnivariateAffineEnclosureImpl(domain,normalizedDomain,components)

  /** Convenience method, normalizes the domain. */
  //  private[enclosure] 
  def apply(domain: Interval, components: Map[VarName, UnivariateAffineScalarEnclosure])(implicit rnd: Rounding): UnivariateAffineEnclosure =
    UnivariateAffineEnclosure(domain, 0 /\ domain.width.high, components)

  /** Lifts a constant interval box to a constant enclosure. */
  def apply(domain: Interval, constant: Box)(implicit rnd: Rounding): UnivariateAffineEnclosure = {
    UnivariateAffineEnclosure(domain, constant.mapValues(UnivariateAffineScalarEnclosure(domain, _)))
  }

  /**
   * Conversion from AffineEnclosure.
   *
   * Precondition: "that" must have domain and normliazedDomain of size one.
   */
  def apply(that: AffineEnclosure)(implicit rnd: Rounding): UnivariateAffineEnclosure = {
    assert(that.domain.size == 1, "Univariate enclosures have domain of size 1.")
    assert(that.normalizedDomain.size == 1, "Univariate enclosures have normalizedDomain of size 1.")
    val name = that.domain.keys.head
    UnivariateAffineEnclosure(
      that.domain(name),
      that.normalizedDomain(name),
      that.components.mapValues(UnivariateAffineScalarEnclosure(_)))
  }

  /** Takes the union of them enclosures */
  def unionThem(them: Seq[UnivariateAffineEnclosure]): Seq[UnivariateAffineEnclosure] =
    if (them isEmpty) them
    else Seq(them.tail.fold(them.head)(_ union _))

}
