package acumen

case object HypothesisResultFilter extends Enumeration {
  val All, Ignore, Comprehensive, IgnoreInitialOnly, MostSignificant = Value
}

/** Used to store information about the Store. */
abstract class Metadata {
  def combine(that: Metadata): Metadata
  final def reportAsString = new SummarizeHypothesisOutcomes().makeReport(this)
}
case object NoMetadata extends Metadata {
  def combine(that: Metadata): Metadata = that
}
case class NoMetadata
  ( f: Option[HypothesisResultFilter.Value] /* Optional filtering parameter */
  ) extends Metadata {
  /* As this class contains no data, 
   * only the filtering should be preserved */
  def combine(that: Metadata): Metadata = that match {
    case NoMetadata => this
    case NoMetadata(ff) => NoMetadata(if (f == None) ff else f)
    case SomeMetadata(th,tt,rr,ff) => SomeMetadata(th,tt,rr,if (f == None) ff else f)
  }
}
case class SomeMetadata 
  /* hyp associates (object, class, name) with outcomes (initial, discrete, continuous) 
   * The enclosure interpreter produces None as initial and discrete hypothesis outcomes */  
  ( hyp: Map[ (CId, ClassName, Option[String]), 
              (Option[HypothesisOutcome], Option[HypothesisOutcome], HypothesisOutcome) ] 
  , timeDomain: (Double, Double)
  , rigorous: Boolean /* Does this describe output of a rigorous interpreter? */
  , f: Option[HypothesisResultFilter.Value] /* Optional filtering parameter */
  ) extends Metadata {
  def combine(that: Metadata): Metadata = {
    that match {
      case NoMetadata => this
      case NoMetadata(ff) => SomeMetadata(this.hyp, this.timeDomain, this.rigorous, 
                              if (f == None) ff else f)
      case SomeMetadata(th,tt,rr,ff) =>
        require( this.timeDomain._2 >= tt._1 || tt._2 >= this.timeDomain._1 
               , "Can not combine SomeMetadata with non-overlapping time domains.")
        SomeMetadata(
          (this.hyp.keySet union th.keySet).map(k => k -> {
            (this.hyp.get(k), th.get(k)) match {
              case (Some(o), None) => o
              case (None, Some(o)) => o
              case (Some((Some(la), Some(ld), lc))
                   ,Some((Some(ra), Some(rd), rc))) =>
                (Some(la pick ra), Some(ld pick rd), lc pick rc)
          }}).toMap
        , (Math.min(this.timeDomain._1, tt._1), Math.max(this.timeDomain._2, tt._2))
        , rr && rigorous, if (f == None) ff else f)
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
abstract class RigorousFailure(earliestTime: (Double,Double), counterExample: Set[(Dot,GValue)]) extends Failure(counterExample: Set[(Dot,GValue)]) 
case class CertainFailure(earliestTime: (Double,Double), counterExample: Set[(Dot,GValue)]) extends RigorousFailure(earliestTime, counterExample) {
  def pick(that: HypothesisOutcome) = that match {
    case CertainSuccess      => this
    case _: UncertainFailure => this
    case f: CertainFailure   => if (this.earliestTime._1 <= f.earliestTime._1) this else that
  } 
}  
case class UncertainFailure(earliestTime: (Double,Double), counterExample: Set[(Dot,GValue)]) extends RigorousFailure(earliestTime, counterExample) {
  def pick(that: HypothesisOutcome): HypothesisOutcome = that match {
    case CertainSuccess      => this
    case f: UncertainFailure => if (this.earliestTime._1 <= f.earliestTime._1) this else that
    case f: CertainFailure   => f
  } 
}
class SummarizeHypothesisOutcomes {

  import HypothesisResultFilter._
  
  // override these methods to customize the formatting in makeReport
  def colorSuccess(m: String) = m
  def colorUncertain(m: String) = m
  def colorFailure(m: String) = m
  def nbsp = " "
  def br = "\n"
  def formatReport(header: String, summary: String, report: String) : String = 
    s"$header\n$summary\n$report\n"

  final def makeReport(md0: Metadata) : String = md0 match {
    case NoMetadata | NoMetadata(_) => ""
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
           *  Results about tests for initial, momentary falsifications 
           * and almost always satisfaction */
          
          val sid = s"(#${id.cid.toString}:${cn.x})".padTo(mLenCId + mLenCN + 2, nbsp).mkString
          val shn = hn.map("'" + _ + "'").getOrElse("").padTo(mLenHN + 2, nbsp).mkString
          
          /* Creating failure messages */
          def fail(prefix: String, t: Any, e: Set[(Dot,GValue)]) = 
            prefix + (if (t == "") "" else s" $t") + (if (e isEmpty) "" else ", where " + e.map { case (d, v) => 
            val lhs = Pretty pprint (if (d.obj == Var(util.Canonical.self)) Var(d.field) else (d:Expr)) 
            s"$lhs = ${Pretty pprint v}"
            }.mkString(", "))  
     
          
          val ho1 = (ho._2, ho._3) match {
            
            case (Some(TestFailure(tm, em)), TestFailure(t, e)) => 
              if ((tm > t) ||                                 /* Removing superflous momentary falsification */
                  (md.f != Some(All) && tm == t && em == e))  /* Removing repeated momentary falsification */
                (ho._1, None, ho._3)
              else
                 ho
            case _ => ho
          }            
          
          /* Filtering the hypothesis outcomes */
          val ho2 = md.f match {
            
            case Some(MostSignificant) => ho1._3 match {
              /* Traditional interpreter outcomes */  
              case TestFailure(_,_) => (None, None, ho1._3)  
              case TestSuccess => ho1._2 match {
                case Some(TestFailure(_,_)) => (None, ho1._2, ho1._3)
                case _ => ho1 }
              /* Rigorous interpreter outcomes */
              case _ => ho1 }

            case Some(IgnoreInitialOnly) => (None, ho1._2, ho1._3)
              
            case _ => ho1
          }

          /* Reports from initial and momentary tests*/
          lazy val subReports =
            (ho2._2 match { 
               case Some(TestFailure(tm, em)) => List( (colorFailure("-"), fail("Falsified momentarily at", tm, em)) ) 
               case _ => List()}) ++ 
            (ho2._1 match { 
               case Some(InitialTestFailure(ei)) => List( (colorFailure("-"), fail("Falsified initially", "", ei)) ) 
               case _ => List()}) 
          
          /* (successes, uncertains, failures, report lines) */
          val (s, u, f, hoLines) = (ho2 : @unchecked) match {

            /* Traditional interpreter outcomes */
            case (Some(TestSuccess) | None, Some(TestSuccess) | None, TestSuccess) => 
              (1, 0, 0, List( ("+", "Tested") ))
            case (_, _, TestSuccess) => 
              (0, 1, 0, (if (md.f != Some(MostSignificant)) List( ("+", "Tested almost everywhere") ) else List()) ++ subReports ) 
            case (_, _, TestFailure(t,e)) =>
              (0, 0, 1, List( (colorFailure("-"), fail("Falsified at", t, e)) ) ++ subReports )

            /* Rigorous interpreter outcomes */
            case (None, None, CertainSuccess) => 
              (1, 0, 0, List( ("+", "Proved") ))
            case (None, None, UncertainFailure(t, e)) => 
              (0, 1, 0, List( (colorUncertain("?"), fail("Inconclusive over", s"[${t._1}..${t._2}]", e)) ))
            case (None, None, CertainFailure(t, e)) => 
              (0, 0, 1, List( (colorFailure("-"), fail("Disproved over", s"[${t._1}..${t._2}]", e)) ))
          }
          ( hoLines.map{ case (symbol, sho) => s"$symbol $sid $shn $sho" }.mkString(br) + br + resReport
          , resS + s, resU + u, resF + f)
      }
      val domain = s" OVER [${md.timeDomain._1}..${md.timeDomain._2}]" 
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
      
  }
}