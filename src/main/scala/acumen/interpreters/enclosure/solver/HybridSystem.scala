package acumen.interpreters.enclosure.solver

import acumen.interpreters.enclosure._
import acumen.interpreters.enclosure.Types._
import acumen.interpreters.enclosure.Relation._
import acumen.interpreters.enclosure.UnaryRelationName._

case class HybridSystem(
  modes: Set[Mode],
  events: Set[Event],
  domains: Map[Mode, Domain],
  fields: Map[Mode, Field],
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

  // defined in section 6.1
  // property: guardPrime(e)(x) implies not(domains(e)(x))
  def guardPrime(event: Event): Guard =
    guard(guards(event).conjuncts.flatMap {
      case UnaryRelation(n, e) => n match {
        case NonPositive => Set(UnaryRelation(Negative, e))
        case NonNegative => Set(UnaryRelation(Positive, e))
        case EqualToZero =>
          domains(event.sigma).conjuncts.filter(_.isNonStrict) flatMap {
            case UnaryRelation(NonPositive, e) => Set(positive(e))
            case UnaryRelation(NonNegative, e) => Set(negative(e))
            case _ => sys.error("the impossible just happened in guardPrime")
          }
      }
    }: _*)

  // described in 6.3
  // computes the set of variables that variable i depends on via fields(e) 
  // property: if i is not in dependentVariables(e)(i) then 
  //           changing x(i) does not change fields(e)(x)
  def dependentVariables(f: Field)(name: VarName): Set[VarName] = {
    var current = Set[VarName]()
    var next = variables(f.components(name))
    while (current != next) {
      current = next
      next ++= current.flatMap(n => dependentVariables(f)(n))
    }
    current
  }

  // helper function for dependentVariables
  // the set of indices of variables in e
  private def variables(e: Expression): Set[VarName] = e match {
    case Constant(_) => Set()
    case Variable(name) => Set(name)
    case Negate(arg) => variables(arg)
    case Plus(l, r) => variables(l) ++ variables(r)
    case Multiply(l, r) => variables(l) ++ variables(r)
    case Divide(l, r) => variables(l) ++ variables(r)
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
