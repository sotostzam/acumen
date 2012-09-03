package acumen.interpreters.enclosure.solver

import acumen.interpreters.enclosure._
import acumen.interpreters.enclosure.Relation._
import acumen.interpreters.enclosure.Types._
import ResetMap._
import sun.tools.tree.LessOrEqualExpression

/** TODO add description */
// TODO add tests, 
// e.g. for invariants that must hold for relations between the 
// modes, events, reset maps, fields , domains and guards in a HybridSystem
object Systems {

  /** TODO add description */
  def BB(g: Double, c: Double)(implicit rnd: Rounding) = {
    val fly = Mode("Fly")
    val bounce = Event("Bounce", fly, fly)
    val flyDomain = domain(nonNegative(Variable("x")))
    def flyField(implicit rnd: Rounding) = Field(Map("x" -> Variable("x'"), "x'" -> -Constant(g)))
    val bounceGuard = guard(
      equalToZero(Variable("x")),
      nonPositive(Variable("x'")))
    def bounceReset = ResetMap(Map("x" -> Variable("x"), "x'" -> -Constant(c) * Variable("x'")))
    HybridSystem(
      Set(fly),
      Set(bounce),
      Map(fly -> flyDomain),
      Map(fly -> flyField),
      Map(bounce -> bounceGuard),
      Map(bounce -> bounceReset))
  }

  /** TODO add description */
  def TT(v1: Double, v2: Double, w: Double)(implicit rnd: Rounding) = {
    val fillLeft = Mode("FillLeft")
    val fillRight = Mode("FillRight")
    val leftToRight = Event("LeftToRight", fillLeft, fillRight)
    val rightToLeft = Event("RightToLeft", fillRight, fillLeft)
    val fillLeftDomain = domain(
      nonNegative(Variable("left")),
      nonNegative(Variable("right")))
    val fillRightDomain = domain(
      nonNegative(Variable("left")),
      nonNegative(Variable("right")))
    def fillLeftField = Field(Map("left" -> (Constant(w) - Constant(v1)), "right" -> -Constant(v2)))
    def fillRightField = Field(Map("left" -> -Constant(v1), "right" -> (Constant(w) - Constant(v2))))
    val leftToRightGuard = guard(
      equalToZero(Variable("right")))
    val rightToLeftGuard = guard(
      equalToZero(Variable("left")))
    def leftToRightReset = ResetMap(Map("left" -> Variable("left"), "right" -> Variable("right")))
    def rightToLeftReset = ResetMap(Map("left" -> Variable("left"), "right" -> Variable("right")))
    HybridSystem(
      Set(fillLeft, fillRight),
      Set(leftToRight, rightToLeft),
      Map(fillLeft -> fillLeftDomain, fillRight -> fillRightDomain),
      Map(fillLeft -> fillLeftField, fillRight -> fillRightField),
      Map(leftToRight -> leftToRightGuard, rightToLeft -> rightToLeftGuard),
      Map(leftToRight -> leftToRightReset, rightToLeft -> rightToLeftReset))
  }

  /** TODO add description */
  def Saw(implicit rnd: Rounding) = {
    val down = Mode("Down")
    val zero = Event("Zero", down, down)
    val downDomain = domain(nonNegative(Variable("x")))
    val downField = Field(Map("x" -> Constant(-1)))
    val zeroGuard = guard(equalToZero(Variable("x")))
    val zeroReset = ResetMap(Map("x" -> Constant(1)))
    HybridSystem(
      Set(down),
      Set(zero),
      Map(down -> downDomain),
      Map(down -> downField),
      Map(zero -> zeroGuard),
      Map(zero -> zeroReset))
  }

  /**
   * Event-less (continuous) system for debugging.
   *
   * Simulates the equation x' = -x.
   *
   * Solution approximates e^(-t)
   */
  def Exp(implicit rnd: Rounding) = {
    val exp = Mode("Exp")
    val never = Event("Never", exp, exp)
    HybridSystem(
      Set(exp),
      Set(never), // no events
      Map(exp -> domain(nonNegative(Variable("x")))), // constant true domain invariant
      Map(exp -> Field(Map("x" -> Multiply(Constant(-1), Variable("x"))))),
      Map(never -> guard(nonNegative(Variable("x")))),
      Map(never -> ResetMap(Map("x" -> Variable("x")))))
  }

}
