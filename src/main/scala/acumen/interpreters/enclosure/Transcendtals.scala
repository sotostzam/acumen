package acumen.interpreters.enclosure

import Types.Mode
import acumen.interpreters.enclosure.affine.UnivariateAffineEnclosure
//import sun.tools.tree.GreaterOrEqualExpression

case class Transcendentals(ps: Parameters) {

  implicit val rnd = ps.rnd

  private val pi = Interval.pi

  /** Computes the interval sine function. */
  def sin(x: Interval) = cos((pi / 2) - x)

  /**
   * Computes the interval cosine function by solving
   *
   *   y''(t) = -y(t) with y(0) = 1 and y'(0) = 0
   *
   * for t in [0,pi/2] and using periodicity and symmetry properties
   * to compute values for arbitrary values by shifting and
   * reflecting into the domain of the piecewise solution.
   *
   * Note: This implementation will fail when used with `x` with
   * end-points exceeding in modulus 2*maxInt. To fix this the
   * shifting of `x` into [0,2pi] has to be improved.
   */
  def cos(x: Interval) = {

    // 1. Compute enclosure table for cosine

    // 1.1 Create a field

    val field = Field(Map( // y'' = -y translated to first-order system:
      "y" -> Variable("y'"), // (y)' = y'
      "y'" -> -Variable("y"))); // (y')' = -y
    val mode = Mode("mode"); // token mode
    val hysys = HybridSystem(
      Set(mode), // single mode continuous system
      Set(), // no events
      Map(mode -> True), // trivial domain invariant
      Map(mode -> field), // single field 
      Map(), // no guards
      Map()); // no resets

    // 1.2 Solve it over [0,1.6]

    val domain = Interval(0, 1.6) // Where we will compute the solution
    val initialCondition = Box(
      "y" -> Interval(1), // y(0) = 1
      "y'" -> Interval(0)); // y'(0) = 0
    val initialConditionStateEnclosure =
      new StateEnclosure(Map(mode -> Some(initialCondition)));

    /**
     * Type for representing the "enclosure table" for the interval
     * cosine function.
     */
    type PiecewiseEnclosure = Seq[UnivariateAffineEnclosure]

    /**
     * Compute the range of the piecewise function over an interval.
     *
     * Note: The variable name y whose range over `it` is returned is
     * hard-coded. Make it parameter when factoring out for reuse.
     */
    def eval(it: PiecewiseEnclosure)(at: Interval): Interval = {
      require(it.nonEmpty && it.tail.map(_.domain).fold(it.head.domain)(_ /\ _).contains(at),
        "the argument must be contained in the union of the pieces' domains")
      val res = it.filterNot(_.domain.disjointFrom(at)).map(e => e(e.domain \/ at)("y"))
      res.tail.fold(res.head)(_ /\ _)
    }

    val table: PiecewiseEnclosure =
      Interpreter.strategy.enclosePiecewise(
        ps,
        hysys,
        domain,
        initialConditionStateEnclosure,
        Interpreter.defaultInterpreterCallbacks)

    // quadrants
    val firstQuadrant = Interval(0) /\ (pi / 2)
    val secondQuadrant = (pi / 2) /\ pi

    // cosine over [0,2pi]
    def cos2pi(x: Interval) = {
      val dom = Interval(0) /\ pi * 2
      require(dom contains x, x + " is not contained in " + dom)
      val firstTwoQuadrants = Interval(0) /\ pi
      val secondTwoQuadrants = pi /\ (pi * 2)

      if (firstTwoQuadrants contains x) cospi(x)
      else if (secondTwoQuadrants contains x) -cospi((x - pi) \/ firstTwoQuadrants)
      else cospi(x.low) /\ -cospi(x.high - pi) /\ -1
    }

    // cosine over [0,pi]
    def cospi(x: Interval) = {
      val dom = firstQuadrant /\ secondQuadrant
      require(dom contains x, x + " is not contained in " + dom)
      val high =
        if (firstQuadrant contains x.low) cosFirstQuadrant(x.low)
        else cosSecondQuadrant(x.low)
      val low =
        if (firstQuadrant contains x.high) cosFirstQuadrant(x.high)
        else cosSecondQuadrant(x.high)
      low /\ high
    }

    // cosine over [pi/2,pi]
    def cosSecondQuadrant(x: Interval) = {
      require(secondQuadrant contains x, x + " is not contined in " + secondQuadrant)
      -cosFirstQuadrant((pi - x) \/ firstQuadrant)
    }

    // 2. Use co-enc

    // cosine over [0,pi/2]
    def cosFirstQuadrant(x: Interval) = {
      require(firstQuadrant contains x, x + " is not contined in " + firstQuadrant)
      Interval(0, 1) \/ eval(table)(x)
    }

    // returned result
    if (x.width greaterThanOrEqualTo pi * 2) Interval(-1, 1)
    else if ((Interval(0) /\ pi * 2) contains x) cos2pi(x)
    else {
      val absx = x.abs // cosine is an even function 
      var i = 0
      while (!(Interval(0) /\ pi * 2 contains (absx - (pi * i)))) i += 1
      val shiftedAbsx = absx - (pi * i)
      cos2pi(shiftedAbsx) * (if (i % 2 == 0) 1 else -1)
    }

  }

}
object Transcendentals extends Transcendentals(Parameters.default) with Application {

  println("sin(pi/4) = " + sin(pi / 4))
  println("cos(pi/4) = " + cos(pi / 4))

}
