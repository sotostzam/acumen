package acumen.interpreters.enclosure.event.tree

import acumen.interpreters.enclosure._
import acumen.interpreters.enclosure.HybridSystem
import acumen.interpreters.enclosure.Types._
import acumen.interpreters.enclosure.Types.Event
import acumen.interpreters.enclosure.Util._
import acumen.interpreters.enclosure.affine.UnivariateAffineEnclosure
import acumen.interpreters.enclosure.affine.UnivariateAffineScalarEnclosure
import acumen.interpreters.enclosure.ivp.IVPSolver
import acumen.interpreters.enclosure.ivp.PicardSolver

/** TODO add description */
// TODO add tests
abstract class EventSequence {
  val enclosure: UnivariateAffineEnclosure
  val mayBeLast: Boolean
  def sigma: Mode
  def tau: Set[Mode]
  def prefixes: Seq[EventSequence]
  def subSequences: Set[EventSequence]
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
  def subSequences = Set(this)
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
  def subSequences = prefix.subSequences + this
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
    degree: Int) {

  def sequences = maximalSequences.flatMap(_.subSequences)

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
    current: Box) =
    current.map {
      case (name, x) => {
        name -> {
          if (eventDoesNotAffectVariable(e, name))
            previous(name)(T.high)
          else x
        }
      }
    }

  /**
   * Adds another event to the event sequences in the tree.
   */
  def addLayer(ivpSolver: IVPSolver): EventTree = {

    def newSequences(v: EventSequence, o: Outcome) = {
      o.events.map { e =>
        {
          if (H.resets(e)(H.guards(e).support(v.enclosure.range)) == Set(false)) println("\naddLayer: illegal reset!")
          require(H.guards(e)(v.enclosure.range) != Set(false), "Rand(Y(v)) \\/ C_e must be nonempty")
          if (o.isInstanceOf[MaybeOneOf] &&
            H.domains(e.tau)(H.resets(e)(H.guards(e).support(v.enclosure.range))) == Set(false)) {
            /**
             * We have detected that the event sequence v cannot be followed
             * by e as the reset maps the previous state outside the support
             * of the domain invariant of the target mode of e.
             */
            v.setMayBeLastTo(true)
          }
          else {
            require(
              H.domains(e.tau)(H.resets(e)(H.guards(e).support(v.enclosure.range))) != Set(false),
              "Reset " + H.resets(e) +
                "\nmust map contracted enclosure " + H.guards(e).support(v.enclosure.range) +
                "\ninto target domain " + H.domains(e.tau))
            val A = H.domains(e.tau).support(H.resets(e)(H.guards(e).support(v.enclosure.range)))
            val N = ivpSolver.solveIVP(H.fields(e.tau), T, A, delta, m, n, degree)._1.range
            val lastEvent = e
            val affines = N
            val enclosure = UnivariateAffineEnclosure(v.domain, affines)
            val mayBeLast = false
            val prefix = v
            NonemptySequence(lastEvent, enclosure, mayBeLast, prefix).asInstanceOf[EventSequence]
          }
        }
      }
    }

    /** TODO add description */
    val newMaximalSequences = {

      /**
       * This is the main event detection algorithm.
       */
      def detectNextEvent(
        H: HybridSystem,
        T: Interval,
        q: Mode,
        Y: UnivariateAffineEnclosure): Outcome = {
        /**
         * This is likely where we get the issues with enclosures exploding from. The
         * events that are deemed possible are determined by evaluating the guard over
         * the range of the enclosure, rather than directly on the enclosure which is
         * a major source of imprecision!
         */
        val events = H.events.filter(e => e.sigma == q && H.guards(e)(Y.range) != Set(false))
        if (events.isEmpty)
          MaybeOneOf(events)
        else if (H.domains(q)(Y(Y.domain.high)) == Set(false))
          CertainlyOneOf(events)
        else
          MaybeOneOf(events)
      }

      maximalSequences.flatMap { v =>
        if (v.prefixes.exists { w =>
          w.tau == v.tau && w.enclosure.contains(v.enclosure)
        }) {
          Set(v)
        }
        else {
          val mode = v.tau.head
          val enclosure = v.enclosure
          val decision = detectNextEvent(H, T, mode, enclosure)

          if (decision.events isEmpty) Set(v.setMayBeLastTo(true))
          else decision match {
            case CertainlyOneOf(es) => newSequences(v, decision)
            case MaybeOneOf(es)     => newSequences(v.setMayBeLastTo(true), decision)
          }
        }
      }
    }

    EventTree(newMaximalSequences, T, H, S, delta, m, n, degree)
  }

  /** TODO add description */
  // TODO add tests
  def endTimeStates: Set[UncertainState] = {
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
        zipDefault(res, x, Interval.zero).mapValues { case (l, r) => l /\ r }
    }).map {
      case (q, b) => UncertainState(q, H.domains(q).support(b))
    }.toSet
  }

  /** TODO add description */
  // TODO add tests
  def enclosureUnion: UnivariateAffineEnclosure = {
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
  def unprunedEnclosures: Seq[UnivariateAffineEnclosure] = {
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
  def prunedEnclosures: Seq[UnivariateAffineEnclosure] =
    maximalSequences.head match {
      case EmptySequence(_, enclosure, _) => Seq(enclosure)
      case _ =>
        val sequences = maximalSequences.flatMap(v => (v +: v.prefixes).toSet)
        sequences.map(s => s.tau.foldLeft(s.enclosure.range) {
          case (res, mode) => H.domains(mode).support(res)
        }).map(ran => UnivariateAffineEnclosure(T, ran)).toSeq
    }

  // StateEnclosure extraction methods

  /** TODO add description */
  def stateEnclosure: StateEnclosure =
    StateEnclosure.union(sequences.map(s => new StateEnclosure(Map(
      s.tau.head -> Some(H.domains(s.tau.head).support(s.enclosure.range))))))

  /** TODO add description */
  def endTimeStateEnclosure =
    StateEnclosure.union(endTimeStates.map(s =>
      new StateEnclosure(Map(s.mode -> Some(s.initialCondition)))))

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
    degree: Int,
    ivpSolver: IVPSolver) = {
    val mode = S.mode
    val (enclosure, _) = ivpSolver.solveIVP(H.fields(mode), T, S.initialCondition, delta, m, n, degree)
    val mayBeLast = false
    val sequences = Set(EmptySequence(mode, enclosure, mayBeLast).asInstanceOf[EventSequence])
    EventTree(sequences, T, H, S, delta, m, n, degree)
  }

}

