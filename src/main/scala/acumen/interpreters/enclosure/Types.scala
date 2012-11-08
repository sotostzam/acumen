package acumen.interpreters.enclosure

import Box._
import Util._

object Types {

  /* Type synonyms */

  /** Variable name */
  type VarName = String

  /* Hybrid System types */

  /** Mode name */
  type ModeName = String

  /** Type for hybrid automaton nodes */
  case class Mode(name: ModeName) {
    override def toString = name
  }

  /** Type for mode invariants */
  type Domain = Predicate

  def domain(conjuncts: Relation*) = All(conjuncts)

  /** Event name */
  type EventName = String

  /** Type for hybrid automaton edges */
  case class Event(name: EventName, sigma: Mode, tau: Mode) {
    override def toString = name
  }

  object Event {
    /** Convenience constructor */
    def apply(sigma: Mode, tau: Mode): Event = Event(sigma.name + " to " + tau.name, sigma, tau)
  }

  /** Type for hybrid automaton transition guards */
  type Guard = All

  def guard(conjuncts: Relation*) = All(conjuncts)

  /** Type for result of event detection */
  abstract class Outcome {
    def events: Set[Event]
  }

  /** TODO add description */
  case class CertainlyOneOf(events: Set[Event]) extends Outcome

  /** TODO add description */
  case class MaybeOneOf(events: Set[Event]) extends Outcome

  /** Hybrid system state. Used as initial condition when simulating the system. */
  case class UncertainState(mode: Mode, initialCondition: Box)

  object UncertainState {
    /** Convenience constructor */
    def apply(pair: (Mode, Box)): UncertainState =
      pair match { case (k, v) => UncertainState(k, v) }
  }

  /** The L1 norm as a measure of precision for a box. */
  def norm(box: Box)(implicit rnd: Rounding) = box.values.fold(Interval(0))(_ + _.width)

  /**
   * Map a collection of UncertainStates to one with unique modes.
   *
   * Implementation note: defined in Definition 6.5.
   */
  // TODO add better description
  def M(states: Set[UncertainState])(implicit rnd: Rounding): Set[UncertainState] =
    states.groupBy(_.mode).mapValues { ss =>
      ss.tail.foldLeft(ss.head.initialCondition) {
        (res, s) => zipDefault(res, s.initialCondition, Interval(0)).mapValues { case (l, r) => l /\ r }
      }
    }.map(UncertainState(_)).toSet

  /** Computes the union of the uncertain state initial conditions. */
  // TODO add better description
  def endTimeInterval(ss: Set[UncertainState])(implicit rnd: Rounding): Box = {
    require(ss.nonEmpty)
    ss.tail.foldLeft(ss.head.initialCondition) { (res, s) =>
      zipDefault(res, s.initialCondition, Interval(0)).mapValues { case (l, r) => l /\ r }
    }
  }

}
