package acumen.interpreters.enclosure.solver

import acumen.interpreters.enclosure._
import acumen.interpreters.enclosure.Types._
import acumen.interpreters.enclosure.Util._

import SolveVt._
import Solver._

/** TODO add description */
// TODO add tests
abstract class EventSequence {
  val enclosure: AffineEnclosure
  var mayBeLast: Boolean
  def sigma: Mode
  def tau: Set[Mode]
  def prefixes: Seq[EventSequence]
  val size: Int
  val domain: Box
}

/** TODO add description */
// TODO add tests
case class EmptySequence(
  val initialMode: Mode,
  val enclosure: AffineEnclosure,
  var mayBeLast: Boolean) extends EventSequence {
  def sigma = initialMode
  def tau = Set(initialMode)
  def prefixes = Seq()
  val size = 0
  val domain = enclosure.domain
}

/** TODO add description */
// TODO add tests
case class NonemptySequence(
  val lastEvent: Event,
  val enclosure: AffineEnclosure,
  var mayBeLast: Boolean,
  val prefix: EventSequence) extends EventSequence {
  def sigma = prefix.sigma
  def tau = Set(lastEvent.tau)
  def prefixes = prefix +: prefix.prefixes
  val size = prefix.size + 1
  val domain = prefix.domain
}

/** TODO add description */
// TODO add tests
case class EventTree(
  maximalSequences: Set[EventSequence],
  T: Interval,
  H: HybridSystem,
  S: UncertainState,
  delta: Double,
  m: Int,
  n: Int,
  output: String) {

  /** TODO add description */
  // TODO add tests
  def size = maximalSequences.map(_.size).max

  /**
   * helper function for onlyUpdateAffectedVariables
   * determines if variable i is affected by event e, as defined by:
   * (i)  the fields of e.sigma and e.tau affect i in the same way
   * (ii) the reset map of e is the identity on i and any variable that
   *      depends on i within the field of e.tau (note that one needs
   *      to chase this dependence through multiple applications of
   *      the field)
   */
  private def eventDoesNotAffectVariable(e: Event, name: VarName) = {
    val fieldsHaveSameEffect =
      H.fields(e.sigma).components(name) == H.fields(e.tau).components(name)
    val resetIsIdentityOnThisAndAllDependentVariables =
      (Set(name) ++ H.dependentVariables(H.fields(e.tau))(name)).forall(n =>
        H.resets(e).components(n) == Variable(n))
    fieldsHaveSameEffect && resetIsIdentityOnThisAndAllDependentVariables
  }

  /**
   * Uses components from previous enclosure in place
   * of current ones for variables not affected by the
   * event e.
   *
   * Implements the event-variable independence analysis
   * algorithm in 6.3.
   *
   * property: result should be equal at T.hi to previous
   *           components for unaffected variables and to
   *           current for affected ones, as given by
   *           eventDoesNotAffectVariable.
   */
  private def onlyUpdateAffectedComponents(
    e: Event,
    previous: AffineEnclosure,
    current: Map[VarName, Interval])(implicit rnd: Rounding) =
    current.map {
      case (name, x) => {
        name -> {
          if (eventDoesNotAffectVariable(e, name))
            previous(name)(Box("t" -> T.high))
          else x
        }
      }
    }

  /** TODO add description */
  // TODO add tests
  def addLayer(implicit rnd: Rounding): EventTree = {
    def newSequences(v: EventSequence, es: Set[Event]) = {
      es.map { e =>
        {
          val A = H.resets(e)(H.guards(e).support(v.enclosure.range))
          val N = solveVt(H.fields(e.tau), T, A, delta, m, n, output).range
          val lastEvent = e
          val affines = onlyUpdateAffectedComponents(e, v.enclosure, H.domains(e.tau).support(N))
          val enclosure = AffineEnclosure(v.domain, affines)
          val mayBeLast = false
          val prefix = v
          NonemptySequence(lastEvent, enclosure, mayBeLast, prefix).
            asInstanceOf[EventSequence]
        }
      }
    }

    /** TODO add description */
    val newMaximalSequences = maximalSequences.flatMap { v =>
      if (v.prefixes.exists { w =>
        w.tau == v.tau && w.enclosure.contains(v.enclosure)
      }) {
        Set(v)
      } else {
        val mode = v.tau.head
        val affines = v.enclosure.components
        val decision = detectNextEvent(H, T, mode, affines)
        decision match {
          case CertainlyOneOf(es) => newSequences(v, es)
          case MaybeOneOf(es) => {
            if (es.isEmpty) {
              v.mayBeLast = true
              Set(v)
            } else {
              v.mayBeLast = true
              newSequences(v, es)
            }
          }
        }
      }
    }
    EventTree(newMaximalSequences, T, H, S, delta, m, n, output)
  }

  /** TODO add description */
  // TODO add tests
  def endTimeStates(implicit rnd: Rounding): Set[UncertainState] = {
    val mayBeLastSequences =
      maximalSequences.flatMap(v => (v +: v.prefixes).toSet).filter(v => v.mayBeLast)
    val mayBeLastStates = mayBeLastSequences.flatMap { v =>
      {
        val modes = v.tau
        val initialCondition = v.enclosure.components.mapValues(_(Box("t" -> T.high)))
        modes.map(q => UncertainState(q, initialCondition))
      }
    }
    val modewiseMayBeLastStates =
      (mayBeLastStates groupBy (_.mode)).mapValues(_.map(_.initialCondition))
    modewiseMayBeLastStates.mapValues(xs => xs.tail.foldLeft(xs.head) {
      (res, x) =>
        zipDefault(res, x, Interval(0)).mapValues { case (l, r) => l /\ r }
    }).map { case (q, b) => UncertainState(q, H.domains(q).support(b)) }.toSet
  }

  /** TODO add description */
  // TODO add tests
  def enclosureUnion(implicit rnd: Rounding): AffineEnclosure = {
    val sequences = maximalSequences.flatMap(v => (v +: v.prefixes).toSet)
    require(sequences.nonEmpty)
    val affs = sequences.tail.foldLeft(sequences.head.enclosure.components) { (res, v) =>
      zipDefault(res, v.enclosure.components, AffineScalarEnclosure(
        // FIXME for now.. not sure of correct choice of domain!
        sequences.head.enclosure.domain,
        0)).mapValues {
        case (l, r) =>
          // FIXME used to be: l union r
          // This HACK should be replaced by proper use of UnivariateAffineScalarEnclosure!
          AffineScalarEnclosure(l.domain, l.range /\ r.range)
      }
    }
    AffineEnclosure(maximalSequences.head.domain, affs)
  }

  /** TODO add description */
  // TODO add tests
  def enclosures(implicit rnd: Rounding): Seq[AffineEnclosure] = {
    val sequences = maximalSequences.flatMap(v => (v +: v.prefixes).toSet)
    sequences.map(_.enclosure).toSeq
  }
}

object EventTree {

  /** TODO add description */
  // TODO add tests
  def initialTree(
    T: Interval,
    H: HybridSystem,
    S: UncertainState,
    delta: Double,
    m: Int,
    n: Int,
    output: String)(implicit rnd: Rounding) = {
    //    Util.appendFile(output, "segment width " + T.width.hi.round(Rounding(3).up) + " segment " + T + "\n")
    //    println("segment width " + T.width.hi.round(Rounding(3).up) + " segment " + T)
    val mode = S.mode
    val enclosure = solveVt(H.fields(mode), T, S.initialCondition, delta, m, n, output)
    //    println("Yinit = " + enclosure)
    val mayBeLast = false
    val sequences = Set(EmptySequence(mode, enclosure, mayBeLast).asInstanceOf[EventSequence])
    EventTree(sequences, T, H, S, delta, m, n, output)
  }

}

