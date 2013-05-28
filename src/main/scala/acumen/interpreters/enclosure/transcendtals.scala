package acumen.interpreters.enclosure

import Types.Mode
import acumen.interpreters.enclosure.affine.UnivariateAffineEnclosure

case class Transcendentals(ps: Parameters) {

  implicit val rnd = ps.rnd

  // 1. Compute co-enc for co-sine

  // 1.1 Create a field

  val field = Field(Map("x" -> Variable("x'"), "x'" -> -Variable("x")));
  val mode = Mode("mode");
  val hysys = HybridSystem(
    Set(mode),
    Set(),
    Map(mode -> True),
    Map(mode -> field),
    Map(),
    Map());

  // 1.2 Solve it over [0,1.6]

  val inter = Interval(0, 1.6) // Where we will compute the solution

  val icbox = Box("x" -> Interval(1), "x'" -> Interval(0));
  val icsta = new StateEnclosure(Map(mode -> Some(icbox)));

  type PiecewiseEnclosure = Seq[UnivariateAffineEnclosure]

  lazy val coenc: PiecewiseEnclosure = Interpreter.strategy.enclosePiecewise(
    ps,
    hysys,
    inter,
    icsta,
    Interpreter.defaultInterpreterCallbacks)

  // 2. Use co-enc

  def eval(it: PiecewiseEnclosure)(at: Interval): Interval = {
    require(it.nonEmpty && it.tail.map(_.domain).fold(it.head.domain)(_ /\ _).contains(at),
      "the argument must be contained in the union of the pieces' domains")
    val res = it.filterNot(_.domain.disjointFrom(at)).map(e => e(e.domain \/ at)("x"))
    res.tail.fold(res.head)(_ /\ _)
  }

  // quadrants
  private val pi = Interval.pi
  private val firstQuadrant = Interval(0) /\ (pi / 2)
  private val secondQuadrant = (pi / 2) /\ pi

  def cos(x: Interval) =
    if (x.width greaterThanOrEqualTo pi * 2) Interval(-1, 1)
    else {
      val low =

        null
    }

  private def shiftInto2pi(x:Interval) = {
    
  }
  
  private def cos2pi(x: Interval) = {
    val dom = Interval(0) /\ pi * 2
    require(dom contains x, x + " is not contained in " + dom)
    val firstTwoQuadrants = Interval(0) /\ pi
    val secondTwoQuadrants = pi /\ (pi * 2)
    if (firstTwoQuadrants contains x) cospi(x)
    else if (secondTwoQuadrants contains x) -cospi(x)
    else cospi(x.low) /\ -cospi(x.high) /\ -1
  }

  private def cospi(x: Interval) = {
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

  private def cosFirstQuadrant(x: Interval) = {
    require(firstQuadrant contains x, x + " is not contined in " + firstQuadrant)
    eval(coenc)(x)
  }

  private def cosSecondQuadrant(x: Interval) = {
    require(secondQuadrant contains x, x + " is not contined in " + secondQuadrant)
    -cosFirstQuadrant((pi - x) \/ firstQuadrant)
  }

}
object Transcendentals extends Transcendentals(Parameters.default) with Application {

  val its = coenc

//  for (it <- its) println(it)

  println(cosSecondQuadrant(Interval.pi - Interval(1)))

}
