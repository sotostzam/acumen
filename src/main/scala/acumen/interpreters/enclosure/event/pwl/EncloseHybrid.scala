package acumen.interpreters.enclosure.event.pwl

import acumen.interpreters.enclosure.Box
import acumen.interpreters.enclosure.Interval
import acumen.interpreters.enclosure.Parameters
import acumen.interpreters.enclosure.Rounding
import acumen.interpreters.enclosure.event.tree.HybridSystem
import acumen.interpreters.enclosure.Types._
import acumen.interpreters.enclosure.affine.UnivariateAffineEnclosure
import acumen.interpreters.enclosure.EnclosureInterpreterCallbacks
import acumen.interpreters.enclosure.Predicate
import acumen.interpreters.enclosure.All
import acumen.interpreters.enclosure.BinaryRelation
import acumen.interpreters.enclosure.BinaryRelationName._
import acumen.interpreters.enclosure.Field

trait EncloseHybrid extends EncloseEvents {

  def encloseHybrid(ps: Parameters, h: HybridSystem, t: Interval, sInit: StateEnclosure, cb: EnclosureInterpreterCallbacks)(implicit rnd: Rounding): Seq[UnivariateAffineEnclosure] = {

    // call event localising ODE solver for each possible mode:
    val lfes: Map[Mode, LFE] = for ((mode, obox) <- sInit if obox.isDefined) yield mode -> encloseFlowUntilEventDetected(ps, h, t, mode, obox.get, cb)

    val teL = (for ((_, mae, _) <- lfes.values if !mae.isEmpty) yield domain(mae).low).foldLeft(t.high)(Interval.min)

    //    val teR = (for ((_, mae, compl) <- lfes.values if compl) yield domain(mae).high).foldLeft(t.high)(Interval.min)
    val teR = (for ((_, mae, _) <- lfes.values if !mae.isEmpty) yield domain(mae).high).foldLeft(t.high)(Interval.min)

    val noe = unionOfEnclosureListsUntil(teL, lfes.values.toSeq.map { case (noe, _, _) => noe })
    if (teL equalTo t.high) { // proved that there no event at all on T
      cb.sendResult(noe)
      noe
    } else {
      val seInit: StateEnclosure = for ((mode, _) <- sInit) yield {
        lfes.get(mode) match {
          case None => mode -> None
          case Some(lfe) => mode -> Some(evalAt(lfe, teL))
        }
      }
      val te = teL /\ teR
      val (se, seFinal) = handleEvents(ps, h, te, seInit)
      if (teR equalTo t.high) {
        val ret = noe ++ enclosures(te, se)
        cb.sendResult(ret)
        ret
      } else {
        val done = noe ++ enclosures(te, se)
        cb.sendResult(done)
        val tf = teR /\ t.high
        val rest = encloseHybrid(ps, h, tf, seFinal, cb)
        done ++ rest
      }
    }
  }

  def unionOfEnclosureListsUntil(t: Interval, enclosureLists: Seq[Seq[UnivariateAffineEnclosure]])(implicit rnd: Rounding): Seq[UnivariateAffineEnclosure] = {
    require(!enclosureLists.isEmpty)
    val lists = enclosureLists.filter(!_.isEmpty)
    if (lists.isEmpty) Seq()
    else {
      val (heads, tails) = lists.tail.
        foldLeft((Seq(lists.head.head), Seq(lists.head.tail))) {
          case ((hs, ts), es) =>
            (es.head +: hs, es.tail +: ts)
        }
      val shortestHead = heads.sortWith(_.domain lessThanOrEqualTo _.domain).head
      val start = shortestHead.domain.low
      if (start equalTo t) Seq() // may have to use almostEqualTo
      else {
        val tH = shortestHead.domain.high
        val isLastSegment = t lessThanOrEqualTo tH
        val tHT = if (isLastSegment) t else tH
        def chopAt(here: Interval)(e: UnivariateAffineEnclosure) =
          if (e.domain properlyContains here)
            e.restrictTo(e.domain.low /\ here)
          else e
        val headsChopped = heads.map(chopAt(tHT))
        val nextEnclosure = headsChopped.tail.fold(headsChopped.head)(_ union _)
        if (isLastSegment) Seq(nextEnclosure)
        else {
          nextEnclosure +: unionOfEnclosureListsUntil(t, tails)
        }
      }
    }
  }

  def encloseFlowUntilEventDetected(ps: Parameters, h: HybridSystem, t: Interval, m: Mode, init: Box, cb: EnclosureInterpreterCallbacks)(implicit rnd: Rounding): LFE = {

    def computeLFE(t: Interval, init: Box): LFE =
      if (t.width greaterThan ps.maxTimeStep) splitAndRepeatComputeLFE(t, init)
      else try {
        val (e, _) = ivpSolver.solveIVP(h.fields(m), t, init, ps.initialPicardPadding, ps.picardImprovements, ps.maxPicardIterations, ps.splittingDegree)
        if (t.width lessThan ps.minSolverStep * 2)
          computeLFEnoODE(e)
        else {
          val (eL, eR) = splitAndSolveVt(t, init)
          if (significantImprovement(e, eR, t.high, ps.minComputationImprovement))
            try { splitAndRepeatComputeLFE(t, init) }
            catch { case _ => computeLFEnoODE(e) } // FIXME use ComputeLFEFailure solver specific exception
          else computeLFEnoODE(e)
        }
      } catch {
        case _ => // FIXME do this properly using specialized exceptions re-throwing messages...
          if (t.width lessThan ps.minSolverStep * 2)
            sys.error("solveVt: terminated at " + t + " after " + ps.maxPicardIterations + " Picard iterations")
          else splitAndRepeatComputeLFE(t, init)
      }

    def splitAndSolveVt(t: Interval, init: Box) = {
      val (tL, tR) = t.split
      val (eL, initR) = ivpSolver.solveIVP(h.fields(m), tL, init, ps.initialPicardPadding, ps.picardImprovements, ps.maxPicardIterations, ps.splittingDegree)
      //      val initR = eL(tL.high)
      val vs = h.domains(m).support(initR)
      val (eR, _) = ivpSolver.solveIVP(h.fields(m), tR, vs, ps.initialPicardPadding, ps.picardImprovements, ps.maxPicardIterations, ps.splittingDegree)
      // val eR = solveVt(h.fields(m), tR, initR, ps.initialPicardPadding, ps.picardImprovements, ps.maxPicardIterations, ps.splittingDegree)
      (eL, eR)
    }

    def splitAndRepeatComputeLFE(t: Interval, init: Box) = {
      val (tL, tR) = t.split
      val lfeL @ (_, _, lfeLcompl) = computeLFE(tL, init)
      if (lfeLcompl) lfeL
      else {
        val initR = evalAt(lfeL, tL.high)
        val vm = h.domains(m).support(initR)
        val lfeR = computeLFE(tR, vm)
        // val lfeR = computeLFE(tR, initR)
        concatenateLFEs(lfeL, lfeR)
      }
    }

    def computeLFEnoODE(e: UnivariateAffineEnclosure)(implicit rnd: Rounding): LFE = {
      val eHasNoLocalizationInfo = enclosureHasNoEventInfo(ps, h, m, e)
      val (eLFE, _) = enclosureToLFE(h, m, e)
      val (_, wm, _) = eLFE
      val eLFEdoesNotRuleOutEvent = wm.nonEmpty
      val shouldSplit =
        (e.domain.width greaterThanOrEqualTo ps.minLocalizationStep * 2) &&
          !eHasNoLocalizationInfo &&
          eLFEdoesNotRuleOutEvent
      if (!shouldSplit) eLFE
      else {
        val (domL, domR) = e.domain.split // TODO factor out as bisectAE
        val eL = e.restrictTo(domL) // TODO factor out as bisectAE
        val eLlfeRecur @ (_, _, locatedLeft) = computeLFEnoODE(eL)
        if (locatedLeft) eLlfeRecur
        else {
          val eR = e.restrictTo(domR) // TODO factor out as bisectAE 
          val eRlfeRecur = computeLFEnoODE(eR)
          val eLRlfe = concatenateLFEs(eLlfeRecur, eRlfeRecur)
          eLRlfe
        }
      }
    }

    val res = computeLFE(t, init)
    res
  }

  def repeatEncloseUntilEventDetected(ps: Parameters, h: HybridSystem, t: Interval, m: Mode, init: Box, cb: EnclosureInterpreterCallbacks)(implicit rnd: Rounding): LFE = {
    val (tL, tR) = t.split
    val lfeL = encloseFlowUntilEventDetected(ps, h, tL, m, init, cb)
    if (!(domain(lfeL) equalTo tL)) lfeL
    else {
      val initR = evalAtRightEndpoint(lfeL)
      val lfeR = encloseFlowUntilEventDetected(ps, h, tR, m, initR, cb)
      concatenateLFEs(lfeL, lfeR)
    }
  }

  def enclosureToLFE(h: HybridSystem, m: Mode, e: UnivariateAffineEnclosure)(implicit rnd: Rounding): (LFE, Boolean) = {
    val eRange = e.range
    val res @ (lfe, lfeIsBad) =
      if (!h.events.filter(_.sigma == m).exists(h.guards(_)(eRange).contains(true))) ((Seq(e), Seq(), false), false) // TODO LaTeX specification says narrow to 0 here
      else if (eventCertain(h, e, m)) ((Seq(), Seq(e), true), false)
      else ((Seq(), Seq(e), false), true)
    res
  }

  def eventCertain(h: HybridSystem, e: UnivariateAffineEnclosure, m: Mode)(implicit rnd: Rounding): Boolean = {
    val eAtRightEndpoint = e(e.domain.high)
    (try { h.domains(m).support(eAtRightEndpoint); false } catch { case _ => true }) ||
      // (h.domains(m)(eAtRightEndpoint) == Set(false)) || // TODO LaTeX specification only narrows to empty box here
      h.events.filter(_.sigma == m).exists(h.guards(_)(eAtRightEndpoint) == Set(true))
  }

  def enclosureHasNoEventInfo(ps: Parameters, h: HybridSystem, m: Mode, e: UnivariateAffineEnclosure)(implicit rnd: Rounding): Boolean = {
    val guards = h.events.filter(_.sigma == m).map(h.guards(_))
    val inv = h.domains(m)
    conditionNowhereFalsifiableOnEnclosure(e, inv) &&
      (guards.exists(conditionNowhereFalsifiableOnEnclosure(e, _))) &&
      (guards.forall(conditionNowhereProvableOnEnclosure(e, _)))
  }

  def conditionNowhereFalsifiableOnEnclosure(e: UnivariateAffineEnclosure, cond: Predicate)(implicit rnd: Rounding): Boolean = {
    val condAtLoRan = cond(e.low.range)
    val condAtHiRan = cond(e.high.range)
    val oneTrueOneFalse =
      (condAtLoRan == Set(true) && condAtHiRan == Set(false)) ||
        (condAtLoRan == Set(false) && condAtHiRan == Set(true))
    val equalityNowhereFalsified = cond match {
      case All(Seq(BinaryRelation(Eq, expr1, expr2))) =>
        val expre = (expr1 - expr2)(rnd)(e)
        (expre.low.range lessThanOrEqualTo 0) && (expre.high.range greaterThanOrEqualTo 0)
      case _ => false
    }
    val conjunctionSplit = cond match {
      case All(Seq(c1, c2)) if c1(e.range) == Set(true) =>
        conditionNowhereFalsifiableOnEnclosure(e, All(Seq(c2)))
      case All(Seq(c1, c2)) if c2(e.range) == Set(true) =>
        conditionNowhereFalsifiableOnEnclosure(e, All(Seq(c1)))
      case All(Seq(c1, c2)) =>
        conditionNowhereFalsifiableOnEnclosure(e, All(Seq(c1))) &&
          conditionNowhereFalsifiableOnEnclosure(e, All(Seq(c2)))
      case _ => false
    }
    oneTrueOneFalse || equalityNowhereFalsified || conjunctionSplit
  }

  def conditionNowhereProvableOnEnclosure(e: UnivariateAffineEnclosure, cond: Predicate)(implicit rnd: Rounding): Boolean = {
    val condAtLoRan = cond(e.low.range)
    val condAtHiRan = cond(e.high.range)
    val oneTrueOneFalse =
      (condAtLoRan == Set(true) && condAtHiRan == Set(false)) ||
        (condAtLoRan == Set(false) && condAtHiRan == Set(true))
    val isNonThinEquality = cond match {
      case All(Seq(BinaryRelation(Eq, lhs, rhs))) =>
        // test that the equality has at least one non-thin enclosure
        !(lhs(e) - lhs(e)).range.isZero || !(rhs(e) - rhs(e)).range.isZero
      case _ => false
    }
    val conjunctionSplit = cond match {
      case All(Seq(c1, c2)) if c1(e) == Set(false) =>
        conditionNowhereProvableOnEnclosure(e, All(Seq(c2)))
      case All(Seq(c1, c2)) if c2(e) == Set(false) =>
        conditionNowhereProvableOnEnclosure(e, All(Seq(c1)))
      case All(Seq(c1, c2)) =>
        conditionNowhereProvableOnEnclosure(e, All(Seq(c1))) &&
          conditionNowhereProvableOnEnclosure(e, All(Seq(c2)))
      case _ => false
    }
    oneTrueOneFalse || isNonThinEquality || conjunctionSplit
  }

  // a type for representing enclosure lists with first event 

  type LFE = (Seq[UnivariateAffineEnclosure], Seq[UnivariateAffineEnclosure], Boolean)

  // operations on LFEs

  def isBetterLFEThan(left: LFE, right: LFE): Boolean = (left, right) match {
    case ((_, maeL, _), _) if maeL.isEmpty => false
    case (_, (_, maeR, _)) if maeR.isEmpty => true
    case ((_, maeL, complL), (_, maeR, complR)) =>
      val tL = domain(maeL)
      val tR = domain(maeR)
      (tR lessThan tL) ||
        // ((tL almostEqualTo tR) && !complR && complL) || // TODO this is included in the LaTeX specification
        (tR properlyContains tL)
  }

  // get the first component of `lfe` truncated at `time`
  // TODO utilize that the enclosures in `lfe` are ordered to optimize this inefficient version
  def getNoeUntil(x: Interval, lfe: LFE)(implicit rnd: Rounding): Seq[UnivariateAffineEnclosure] = lfe match {
    case (noe, _, _) =>
      val unchanged = noe.filter(_.domain.high lessThanOrEqualTo x)
      val toBeRestricted = noe.filter(e => (e.domain.low lessThanOrEqualTo x) && (x lessThan e.domain.high))
      val restricted = toBeRestricted.map(e => e.restrictTo(e.domain.low /\ x))
      unchanged ++ restricted
  }

  // assumes that `x` is in the domain of some enclosure in `lfe`
  // assumes that `lfe` is nonempty
  def evalAt(lfe: LFE, x: Interval)(implicit rnd: Rounding): Box = lfe match {
    case (noe, mae, _) =>
      val es = noe ++ mae
      require(es.exists(_.domain.contains(x)))
      val bs = for (e <- es if e.domain.contains(x)) yield e(x)
      bs.tail.fold(bs.head)(_ hull _)
  }

  def evalAtRightEndpoint(lfe: LFE): Box = {
    val e = lfe match { case (noe, mae, _) => (noe ++ mae).last }
    e(e.domain.high)
  }

  def domain(lfe: LFE): Interval =
    lfe match { case (noe, mae, _) => val es = noe ++ mae; es.first.domain /\ es.last.domain }

  def concatenateLFEs(lfe1: LFE, lfe2: LFE): LFE =
    (lfe1, lfe2) match {
      case ((_, _, compl1), _) if compl1 => lfe1
      case ((noe1, mae1, _), (noe2, mae2, compl2)) if mae1.isEmpty => (noe1 ++ noe2, mae2, compl2)
      case ((noe1, mae1, _), (noe2, _, _)) if !noe2.isEmpty => (noe1, mae1, true)
      case ((noe1, mae1, _), (noe2, mae2, compl2)) if noe2.isEmpty => (noe1, mae1 ++ mae2, compl2)
    }

  // auxiliary operations 
  // TODO move most of them out of this section

  // the union of the intervals in `es` that transitively have nonempty intersection 
  // with the one(s) with least left end-point, i.e. the leftmost component of `es`
  def leftmostComponentOfUnion(is: Seq[Interval]): Interval = {
    require(!is.isEmpty)
    val sorted = is.sortWith(_.low lessThanOrEqualTo _.low)
    // assumes `is` is sorted by low end-point
    def leftmostComponentOfUnionHelper(is: Seq[Interval]): Seq[Interval] =
      if (is.isEmpty) Seq()
      else {
        val (intersecting, rest) = is.tail.span(!_.disjointFrom(is.head))
        intersecting.fold(is.head)(_ /\ _) +: leftmostComponentOfUnionHelper(rest)
      }
    leftmostComponentOfUnionHelper(sorted).head
  }

  // refines the enclosures so that their domains only intersect at the end-points
  // assumes that the domains of consecutive elements are connected
  def unionOfEnclosureLists(es: Seq[UnivariateAffineEnclosure]): Seq[UnivariateAffineEnclosure] =
    es.groupBy(_.domain).values.toList.flatMap(UnivariateAffineEnclosure.unionThem(_)).
      sortWith { _.domain.low lessThanOrEqualTo _.domain.low }

  // assumes that the domains of consecutive elements are connected
  def domain(es: Seq[UnivariateAffineEnclosure]) = {
    require(es.nonEmpty)
    es.first.domain /\ es.last.domain
  }

}
