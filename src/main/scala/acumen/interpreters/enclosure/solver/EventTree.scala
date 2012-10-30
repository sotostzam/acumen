package acumen.interpreters.enclosure.solver

import acumen.interpreters.enclosure._
import acumen.interpreters.enclosure.Types._
import acumen.interpreters.enclosure.Util._

/** TODO add description */
// TODO add tests
abstract class EventSequence {
  val enclosure: UnivariateAffineEnclosure
  val mayBeLast: Boolean
  def sigma: Mode
  def tau: Set[Mode]
  def prefixes: Seq[EventSequence]
  val size: Int
  val domain: Interval
  def setMayBeLastTo(value: Boolean): EventSequence
}

/** TODO add description */
// TODO add tests
case class EmptySequence(
  val initialMode: Mode,
  val enclosure: UnivariateAffineEnclosure,
  val mayBeLast: Boolean) extends EventSequence {
  def sigma = initialMode
  def tau = Set(initialMode)
  def prefixes = Seq()
  val size = 0
  val domain = enclosure.domain
  def setMayBeLastTo(value: Boolean) =
    if (mayBeLast == value) this
    else EmptySequence(initialMode, enclosure, value)
}

/** TODO add description */
// TODO add tests
case class NonemptySequence(
  val lastEvent: Event,
  val enclosure: UnivariateAffineEnclosure,
  val mayBeLast: Boolean,
  val prefix: EventSequence) extends EventSequence {
  def sigma = prefix.sigma
  def tau = Set(lastEvent.tau)
  def prefixes = prefix +: prefix.prefixes
  val size = prefix.size + 1
  val domain = prefix.domain
  def setMayBeLastTo(value: Boolean) =
    if (mayBeLast == value) this
    else NonemptySequence(lastEvent, enclosure, value, prefix)
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
  output: String) extends Solver with SolveVt {

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
  // TODO add tests
  private def onlyUpdateAffectedComponents(
    e: Event,
    previous: UnivariateAffineEnclosure,
    current: Box)(implicit rnd: Rounding) =
    current.map {
      case (name, x) => {
        name -> {
          if (eventDoesNotAffectVariable(e, name))
            previous(name)(T.high)
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
          //          println("Guard:   " + H.guards(e))
          //          println("Box:     " + v.enclosure.range)
          //          println("Support: " + H.guards(e).support(v.enclosure.range))
          //          println("T: " + T)
          /**
           * try-catch HACK, handle this properly by detecting when empty intersections
           * indicate inconsistent model and when impossible transitions!
           */
          //          try {
          println("\naddLayer: range             = " + v.enclosure.range)
          println("addLayer: domain before     = " + H.domains(e.tau))
          println("\naddLayer: contracted range  = " + H.guards(e).support(v.enclosure.range))
          println("addLayer: reset             = " + H.resets(e))
          if (H.resets(e)(H.guards(e).support(v.enclosure.range)) == Set(false)) println("\naddLayer: illegal reset!")
          println("\naddLayer: range after reset  = " + H.resets(e)(H.guards(e).support(v.enclosure.range)))
          println("addLayer: domain after reset = " + H.domains(e.tau))
          if (H.domains(e.tau)(H.resets(e)(H.guards(e).support(v.enclosure.range))) != Set(false))
            println("\naddLayer: consistent value after reset!")
          val A = H.domains(e.tau).support(H.resets(e)(H.guards(e).support(v.enclosure.range)))
          //          val A = H.domains(e.tau).support(H.resets(e)(H.guards(e).support(v.enclosure.range)))
          println("\naddLayer: A         = " + A)
          println("addLayer: field     = " + H.fields(e.tau))
          //            println("addLayer: enclosure = " + solveVt(H.fields(e.tau), T, A, delta, m, n, output))
          val N = solveVt(H.fields(e.tau), T, A, delta, m, n, output).range
          println("addLayer: N         = " + N)
          val lastEvent = e
          //          println("Domain:  " + H.domains(e.tau))
          //          println("Box:     " + N)
          //          println("Support: " + H.domains(e.tau).support(N))
          if (H.domains(e.tau)(N) == Set(false)) println("\naddLayer: illegal enclosure!")
          println("addLayer: contracted N = " + H.domains(e.tau).support(N))
          // onlyUpdateAffectedComponents introduces some errors! FIXME
          val affines = N // onlyUpdateAffectedComponents(e, v.enclosure, H.domains(e.tau).support(N))
          println("addLayer: disregarding unaffectd components N = " + affines)
          //            println("Y(ve): " + affines)
          val enclosure = UnivariateAffineEnclosure(v.domain, affines)
          val mayBeLast = false
          val prefix = v
          NonemptySequence(lastEvent, enclosure, mayBeLast, prefix).asInstanceOf[EventSequence]
          //          } catch {
          //            case _ => v
          //          }
        }
      }
    }

    /** TODO add description */
    val newMaximalSequences = maximalSequences.flatMap { v =>
      if (v.prefixes.exists { w =>
        w.tau == v.tau && w.enclosure.contains(v.enclosure)
      }) {
        println("\naddLayer: containment!\n") // PRINTME
        Set(v)
      } else {
        val mode = v.tau.head
        val enclosure = v.enclosure
        val decision = detectNextEvent(H, T, mode, enclosure)
        //        println("\naddLayer: " + decision) // PRINTME
        decision match {
          case CertainlyOneOf(es) => newSequences(v, es)
          case MaybeOneOf(es) => {
            if (es.isEmpty) {
              Set(v.setMayBeLastTo(true))
            } else {
              newSequences(v.setMayBeLastTo(true), es)
            }
          }
        }
      }
    }

    val res = EventTree(newMaximalSequences, T, H, S, delta, m, n, output)
    println("\naddLayer: " + res)
    res
  }

  /** TODO add description */
  // TODO add tests
  def endTimeStates(implicit rnd: Rounding): Set[UncertainState] = {
    val mayBeLastSequences =
      maximalSequences.flatMap(v => (v +: v.prefixes).toSet).filter(v => v.mayBeLast)
    val mayBeLastStates = mayBeLastSequences.flatMap { v =>
      {
        val modes = v.tau
        // FIXME evaluating at the enclosure's domain.high instead of T.high
        // the latter caused an assertion failure as enclosures were evaluated
        // outside their domain. E.g. and enclosure over [0,1.5] would be evaluated
        // at the point [3,3].
        val initialCondition = v.enclosure.components.mapValues(e => e(e.domain.high))
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
  def enclosureUnion(implicit rnd: Rounding): UnivariateAffineEnclosure = {
    val sequences = maximalSequences.flatMap(v => (v +: v.prefixes).toSet)
    require(sequences.nonEmpty)
    val affs = sequences.tail.foldLeft(sequences.head.enclosure.components) { (res, v) =>
      zipDefault(res, v.enclosure.components, UnivariateAffineScalarEnclosure(
        // FIXME for now.. not sure of correct choice of domain!
        sequences.head.enclosure.domain,
        0)).mapValues { case (l, r) => l union r }
    }
    UnivariateAffineEnclosure(maximalSequences.head.domain, affs)
  }

  /** TODO add description */
  // TODO add tests
  def unprunedEnclosures(implicit rnd: Rounding): Seq[UnivariateAffineEnclosure] = {
    val sequences = maximalSequences.flatMap(v => (v +: v.prefixes).toSet)
    sequences.map(_.enclosure).toSeq
  }

  /**
   * Takes the box-hull of enclosures for non-empty event sequences
   * and intersects with the target mode domain invariant.
   *
   * This has the effect of "shaving off" of the parts of enclosures that e.g.
   * in the bouncing ball example "dip below" the ground.
   */
  def prunedEnclosures(implicit rnd: Rounding): Seq[UnivariateAffineEnclosure] =
    maximalSequences.head match {
      case EmptySequence(_, enclosure, _) => Seq(enclosure)
      case _ =>
        val sequences = maximalSequences.flatMap(v => (v +: v.prefixes).toSet)
        sequences.map(s => s.tau.foldLeft(s.enclosure.range) {
          case (res, mode) => H.domains(mode).support(res)
        }).map(ran => UnivariateAffineEnclosure(T, ran)).toSeq
    }

}

object EventTree extends SolveVt {

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
    val res = EventTree(sequences, T, H, S, delta, m, n, output)
    println("\n############\n")
    println("initialTree: " + res)
    res
  }

}

