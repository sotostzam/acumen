package acumen

case object HypothesisResultFilter extends Enumeration {
  type HypothesisResultFilter = Value
  val Ignore, Comprehensive, IgnoreInitialOnly, MostSignificant = Value
}
import HypothesisResultFilter._

/** Used to store information about the Store. */
abstract class Metadata(f: Option[HypothesisResultFilter] /* Optional filtering parameter */
  ) {
  def combine(that: Metadata): Metadata
  final def reportAsString = StringHypothesisOutcomeSummary.makeReport(this)
}
case object NoMetadata extends Metadata(None) {
  def combine(that: Metadata): Metadata = that
}
case class NoMetadata(f: Option[HypothesisResultFilter]) extends Metadata(f) {
  /* As this class contains no data, 
   * only the filtering should be preserved */
  def combine(that: Metadata): Metadata = that match {
    case NoMetadata      => this
    case n: NoMetadata   => n.copy(f = if (f isDefined) f else n.f)
    case s: SomeMetadata => s.copy(f = if (f isDefined) f else s.f)
  }
}
case class SomeMetadata 
  /* hyp associates (object, class, name) with outcomes (initial, momentary, severe) 
   * The enclosure interpreter produces None as initial and momentary hypothesis outcomes */  
  ( hyp: Map[ (CId, ClassName, Option[String]), 
              (Option[HypothesisOutcome], Option[HypothesisOutcome], HypothesisOutcome) ] 
  , timeDomainLo: Double
  , timeDomainHi: Double
  , rigorous: Boolean /* Does this describe output of a rigorous interpreter? */
  , f: Option[HypothesisResultFilter] = None
  ) extends Metadata(f) {
  /** Combines two Metadata, giving priority to the first */
  def combine(that: Metadata): Metadata = {
    that match {
      case NoMetadata => this
      case NoMetadata(ff) => this.copy(f = if (f isDefined) f else ff)
      case SomeMetadata(th,ttLo,ttHi,rr,ff) =>
        require( timeDomainLo <= ttHi && ttLo <= timeDomainHi  
               , "Can not combine SomeMetadata with non-overlapping time domains.")
        SomeMetadata(
          (this.hyp.keySet union th.keySet).map(k => k -> {
             /* (!) The new Metadata is not from a Continuous state, 
              * thus former failure at the very same time is 
              * not severe but only momentary */
            def thisIsMomentary(ho: HypothesisOutcome, thHi : Double, ttHi : Double) = (thHi == ttHi, ho) match {
              case (true, TestFailure(t,e)) => t == thHi
              case _                        => false }
            ((this.hyp get k, th get k): @unchecked) match {
              case (Some(o @ (i, m, s)), None) =>
                if (thisIsMomentary(s, timeDomainHi, ttHi) && !rr)
                  (i, Some(s pick m), TestSuccess) // (!)
                else
                  o
              case (None, Some(o)) => o
              case (Some((Some(li), Some(lm), ls))
                   ,Some((Some(ri), Some(rm), rs))) =>
                if (thisIsMomentary(ls, timeDomainHi, ttHi) && !rr && !rigorous) 
                  (Some(li pick ri), Some(lm pick ls pick rm), rs) // (!)
                                               // ^^ this has priority over the new element rm so it gets picked first
                else
                  (Some(li pick ri), Some(lm pick rm), ls pick rs)
          }}).toMap
        , Math.min(timeDomainLo, ttLo), Math.max(timeDomainHi, ttHi)
        , rr && rigorous, if (f isDefined) f else ff)
    } 
  }
}

/** The result of evaluating a hypothesis.*/
trait HypothesisOutcome {
  /**
   * Returns either this or that outcome, depending on which is more significant. 
   * A failure is more significant than a success, and an earlier failure more
   * significant than a later one.
   */
  def pick(that: HypothesisOutcome): HypothesisOutcome
  def pick(that: Option[HypothesisOutcome]): HypothesisOutcome = that match {
    case Some(ho) => this pick ho
    case _ => this
  }
}
abstract class Success extends HypothesisOutcome { def pick(that: HypothesisOutcome) = that }
abstract class Failure(counterExample: Set[(Dot,GValue)]) extends HypothesisOutcome
/** Result of non-rigorous hypothesis evaluation (reference interpreter). */
case object TestSuccess extends Success
case class InitialTestFailure(counterExample: Set[(Dot,GValue)]) extends Failure(counterExample: Set[(Dot,GValue)]) {
  def pick(that: HypothesisOutcome) = this
}
case class TestFailure(earliestTime: Double, counterExample: Set[(Dot,GValue)]) extends Failure(counterExample: Set[(Dot,GValue)]) {
  def pick(that: HypothesisOutcome) = that match {
    case TestSuccess    => this
    case f: TestFailure => if (this.earliestTime <= f.earliestTime) this else that
  }
}
/** Result of rigorous hypothesis evaluation (enclosure interpreter). */
case object CertainSuccess extends Success
abstract class RigorousFailure(
  earliestTimeLo: Double, earliestTimeHi: Double, 
  counterExample: Set[(Dot,GValue)]) extends Failure(counterExample: Set[(Dot,GValue)]) 
case class CertainFailure(
  earliestTimeLo: Double, earliestTimeHi: Double, 
  counterExample: Set[(Dot,GValue)]) extends RigorousFailure(earliestTimeLo, earliestTimeHi, counterExample) {
  def pick(that: HypothesisOutcome) = that match {
    case CertainSuccess      => this
    case _: UncertainFailure => this
    case f: CertainFailure   => if (this.earliestTimeLo <= f.earliestTimeLo) this else that
  } 
}  
case class UncertainFailure(
  earliestTimeLo: Double, earliestTimeHi: Double, 
  counterExample: Set[(Dot,GValue)]) extends RigorousFailure(earliestTimeLo, earliestTimeHi, counterExample) {
  def pick(that: HypothesisOutcome): HypothesisOutcome = that match {
    case CertainSuccess      => this
    case f: UncertainFailure => if (this.earliestTimeLo <= f.earliestTimeLo) this else that
    case f: CertainFailure   => f
  } 
}
trait HypothesisOutcomeSummary {

  import HypothesisResultFilter._
  
  // Implement these methods to customize the formatting in makeReport
  def colorSuccess(m: String): String
  def colorUncertain(m: String): String
  def colorFailure(m: String): String
  def nbsp: String
  def br: String
  def formatReport(header: String, summary: String, report: String) : String

  final def makeReport(md0: Metadata) : String = md0 match {
    case md:SomeMetadata => 
      val (mLenCId, mLenCN, mLenHN) = md.hyp.foldLeft((0, 0, 0)) {
        case ((rid, rcn, rhn), ((id,cn,hn), od)) =>
          val sid = id.cid.toString
          ( Math.max(rid, sid.length)
          , Math.max(rcn, cn.x.length)
          , Math.max(rhn, hn.map(_.length).getOrElse(0)))
      }
      val (report, successes, uncertains, failures) = md.hyp.toList.reverse.foldLeft(("", 0, 0, 0)) {
        case ((resReport, resS, resU, resF), ((id, cn, hn), ho)) =>
          /* Hypothesis outcome is a triple: ho = (iRes, mRes, aRes)
           * Results about tests for initial, momentary falsifications 
           * and almost always satisfaction */
          
          val sid = s"(#${id.cid.toString}:${cn.x})".padTo(mLenCId + mLenCN + 2, nbsp).mkString
          val shn = hn.map("'" + _ + "'").getOrElse("").padTo(mLenHN + 2, nbsp).mkString
          
          /* Creating failure messages */
          def fail(prefix: String, t: Any, e: Set[(Dot,GValue)]) = 
            prefix + (if (t == "") "" else s" $t") + (if (e isEmpty) "" else ", where " + e.map { case (d, v) => 
            val lhs = Pretty pprint (if (d.obj == Var(util.Canonical.self)) Var(d.field) else (d:Expr)) 
            s"$lhs = ${Pretty pprint v}"
            }.mkString(", "))  
          
          /* Filtering the hypothesis outcomes */
          val ho1 = md.f match {
            
            case Some(MostSignificant) => ho._3 match {
              /* Traditional interpreter outcomes */  
              case TestFailure(_,_) => (None, None, ho._3)  
              case TestSuccess => ho._2 match {
                case Some(TestFailure(_,_)) => (None, ho._2, ho._3)
                case _ => ho }
              /* Rigorous interpreter outcomes */
              case _ => ho }

            case Some(IgnoreInitialOnly) => (None, ho._2, ho._3)
              
            case _ => ho
          }

          /* Reports from initial and momentary tests*/
          lazy val subReports =
            (ho1._2 match { 
               case Some(TestFailure(tm, em)) => List( None -> fail("Falsified momentarily at", tm, em) ) 
               case _ => Nil }) ++ 
            (ho1._1 match { 
               case Some(InitialTestFailure(ei)) => List( None -> fail("Falsified initially", "", ei) ) 
               case _ => Nil }) 
          
          /* (successes, uncertains, failures, report lines) */
          val (s, u, f, hoLines) = (ho1 : @unchecked) match {

            /* Traditional interpreter outcomes */
            case (Some(TestSuccess) | None, Some(TestSuccess) | None, TestSuccess) => 
              (1, 0, 0, List( Some("+") -> "Tested" ))
            case (_, _, TestSuccess) => 
              (0, 1, 0, (if (md.f == Some(MostSignificant)) Nil else List( Some(colorUncertain("~")) -> "Tested almost everywhere" )) ++ subReports ) 
            case (_, _, TestFailure(t,e)) =>
              (0, 0, 1, List( Some(colorFailure("-")) -> fail("Falsified at", t, e) ) ++ subReports )

            /* Rigorous interpreter outcomes */
            case (_, _, CertainSuccess) => 
              (1, 0, 0, List( Some("+") -> "Proved" ))
            case (_, _, UncertainFailure(tLo, tHi, e)) => 
              (0, 1, 0, List( Some(colorUncertain("?")) -> fail("Inconclusive over", s"[$tLo..$tHi]", e) ))
            case (_, _, CertainFailure(tLo, tHi, e)) => 
              (0, 0, 1, List( Some(colorFailure("-")) -> fail("Disproved over", s"[$tLo..$tHi]", e) ))
          }
          ( hoLines.map{ case (symbol, outcome) =>
              if (symbol.nonEmpty) symbol.get + s" $sid $shn " + outcome
              else nbsp * (mLenCId + mLenCN + mLenHN + 10) + outcome
            }.mkString(br) + br + resReport
          , resS + s, resU + u, resF + f)
      }
      val domain = s" OVER [${md.timeDomainLo}..${md.timeDomainHi}]" 
      val header = (successes, uncertains, failures) match {
        case (_, _, f) if f > 0 =>
          colorFailure ("SOME HYPOTHESES " + (if (md.rigorous) "DISPROVED" else "FALSIFIED") + domain)
        case (_, u, 0) if u > 0 =>
          colorUncertain("SOME HYPOTHESES INCONCLUSIVE" + domain)
        case _  =>
          colorSuccess((if (md.rigorous) "ALL HYPOTHESES PROVED" else "NO HYPOTHESES FALSIFIED") + domain)
      }
      val summary = s"$successes TRUE, $failures FALSE, $uncertains INCONCLUSIVE"
      
      /* Filtering results */ 
      md.f match {
        case Some(Ignore) => ""
        case _ => formatReport(header, summary, report)
      }
    case _ => "" // NoMetadata does not contribute to report
  }
}
object StringHypothesisOutcomeSummary extends HypothesisOutcomeSummary {
  def colorSuccess(m: String) = m
  def colorUncertain(m: String) = m
  def colorFailure(m: String) = m
  def nbsp = " "
  def br = "\n"
  def formatReport(header: String, summary: String, report: String) : String = 
    s"$header\n$summary\n$report\n"
}
