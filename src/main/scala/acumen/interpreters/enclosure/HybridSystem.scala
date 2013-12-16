package acumen.interpreters.enclosure

import acumen.interpreters.enclosure._
import acumen.interpreters.enclosure.Types._
import acumen.interpreters.enclosure.Relation._
import acumen.interpreters.enclosure.UnaryRelationName._
import acumen.interpreters.enclosure.ResetMap

case class HybridSystem(
    modes: Set[Mode],
    events: Set[Event],
    domains: Map[Mode, Domain],
    fields: Map[Mode, Field],
    //  flows: Map[Mode, Flow],
    guards: Map[Event, Guard],
    resets: Map[Event, ResetMap]) {

  def addMode(mode: Mode, domain: Domain, field: Field): HybridSystem =
    HybridSystem(
      modes + mode,
      events,
      domains + (mode -> domain),
      fields + (mode -> field),
      guards,
      resets)

  def addEvent(event: Event, guard: Guard, reset: ResetMap): HybridSystem =
    HybridSystem(
      modes,
      events + event,
      domains,
      fields,
      guards + (event -> guard),
      resets + (event -> reset))

  // FIXME: DEPRECATED
  // defined in section 6.1
  // property: guardPrime(e)(x) implies not(domains(e)(x))
  //  def guardPrime(event: Event): Guard =
  //    guard(guards(event).conjuncts.flatMap {
  //      case UnaryRelation(n, e) => n match {
  //        case NonPositive => Set(UnaryRelation(Negative, e))
  //        case NonNegative => Set(UnaryRelation(Positive, e))
  //        case EqualToZero =>
  //          domains(event.sigma).conjuncts.filter(_.isNonStrict) flatMap {
  //            case UnaryRelation(NonPositive, e) => Set(positive(e))
  //            case UnaryRelation(NonNegative, e) => Set(negative(e))
  //            case _ => sys.error("the impossible just happened in guardPrime")
  //          }
  //      }
  //    }: _*)

  // described in 6.3
  // computes the set of variables that variable i depends on via fields(e) 
  // property: if i is not in dependentVariables(e)(i) then 
  //           changing x(i) does not change fields(e)(x)
  // FIXME does not detect algebraic loops!!!
  def dependentVariables(f: Field)(name: VarName): Set[VarName] =
    dependentVariables0(f)(f.components(name).variables)
  def dependentVariables0(f: Field)(seen: Set[VarName]): Set[VarName] = {
    val deps = seen union seen.flatMap(name => f.components(name).variables)
    if (seen == deps) seen else dependentVariables0(f)(deps)
  }

}

object HybridSystem {

  def empty = HybridSystem(
    Set[Mode](),
    Set[Event](),
    Map[Mode, Domain](),
    Map[Mode, Field](),
    Map[Event, Guard](),
    Map[Event, ResetMap]())

}

object HybridSystemApp extends App {

  import HybridSystem._
  implicit val rnd = Parameters.default.rnd
  val a = Variable("a")
  val b = Variable("b")
  val c = Variable("c")
  val f = Field(Map("a" -> b, "b" -> a))
  println(empty.dependentVariables(f)("a"))
  println(empty.dependentVariables(f)("b"))

}
