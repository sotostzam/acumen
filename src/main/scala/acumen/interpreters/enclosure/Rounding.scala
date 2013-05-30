package acumen.interpreters.enclosure

/**
 * A class for setting precision for interval end-points. An
 * instance of this class will be passed around as an implicit
 * parameter providing the rounding "towards -oo and +oo" modes
 * needed for the directed rounded BigDecimal arithmetic used to
 * implement outward rounded interval arithmetic.
 * @param precision of the BigDecimal values computed by methods
 * taking the up and dn MathContexts provided by the instance.
 * @define up MathContext with the given precision and rounding
 * modes "towards +oo".
 * @define dn MathContext with the given precision and rounding
 * modes "towards -oo".
 */
case class Rounding(ps: Parameters) {

  def precision = ps.bigDecimalDigits

  val up: java.math.MathContext = new java.math.MathContext(ps.bigDecimalDigits, java.math.RoundingMode.CEILING)
  val dn: java.math.MathContext = new java.math.MathContext(ps.bigDecimalDigits, java.math.RoundingMode.FLOOR)

  lazy val transcendentals = new Transcendentals(ps)

}
