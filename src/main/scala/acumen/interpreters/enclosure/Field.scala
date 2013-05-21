package acumen.interpreters.enclosure

import acumen.interpreters.enclosure.Types._
import acumen.interpreters.enclosure.affine.AffineEnclosure

/**
 * Type for representing fields of differential equations.
 *
 * Implementation note: TODO
 */
// TODO write tests.
case class Field(components: Map[VarName, Expression])(implicit rnd: Rounding) {

  lazy val variables = components.values.flatMap(_.varNames)

  /**
   * Apply the field to an enclosure.
   *
   * Implementation note: TODO
   */
  def apply(x: AffineEnclosure)(implicit rnd: Rounding) = {
    AffineEnclosure(x.domain, x.normalizedDomain, components.mapValues(_(x)))
  }

  /**
   * The components of the Jacobian matrix of the field.
   */
  private def jacobian(component: VarName, variable: VarName)(implicit rnd: Rounding): Expression = {
    require(components.keySet contains component, this + " must have a " + component + " component!")
    components(component).dif(variable)
  }

  /**
   * The Jacobian matrix of the field.
   *
   * Note: FIXME re-implement sharing sub-expressions between symbolic differentiation of components.
   *       FIXME do not compute duplicates in this symmetric matrix.
   */
  lazy val jacobian: Map[VarName, Map[VarName, Expression]] =
    components.keys.map(component =>
      component -> components.keys.map(variable =>
        variable -> jacobian(component, variable)).toMap).toMap

  /**
   * The logarithmic norm for the max norm evaluated in the Jacobian of the field.
   */
  def jacobianLogMaxNorm(b: Box)(implicit rnd: Rounding): Interval =
    Interval.max(components.keys.map(component => {
      (components.keySet - component).foldLeft(jacobian(component)(component)(b)) {
        case (res, variable) => res + jacobian(component)(variable)(b)
      }
    }))

}

object FieldApp extends App {

  implicit val rnd = Rounding(10)

  val field = Field(Map("x" -> Variable("x'"), "x'" -> -(Variable("x") + Variable("x'"))))
  val b = Box("x" -> Interval(-1, 1), "x'" -> Interval(-1, 1))

  for (component <- field.components.keys) {
    for (variable <- field.components.keys) {
      print(field.jacobian(component)(variable)(b) + " ")
    }
    println("")
  }
  println("")

  println(field.jacobianLogMaxNorm(b))

}


