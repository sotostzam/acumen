package acumen.interpreters.enclosure.affine

import acumen.interpreters.enclosure.Rounding
import acumen.interpreters.enclosure.Types._
import acumen.interpreters.enclosure.Interval
import acumen.interpreters.enclosure.affine.UnivariateAffineEnclosure
import acumen.interpreters.enclosure.affine.UnivariateAffineScalarEnclosure

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
  private[affine] override val normalizedDomain: Interval,
  override val components: Map[VarName, UnivariateAffineScalarEnclosure],
  padding: Interval => Interval)(implicit rnd: Rounding)
    extends UnivariateAffineEnclosure(domain, normalizedDomain, components) {

  System.err.println("NOT IMPLEMENTED");

}
object PaddedUnivariateAffineEnclosure {
  def apply(e: UnivariateAffineEnclosure, padding: Interval => Interval): PaddedUnivariateAffineEnclosure =
    PaddedUnivariateAffineEnclosure(e.domain, e.normalizedDomain, e.components, padding)(e.rounding)

}