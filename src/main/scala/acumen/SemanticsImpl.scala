package acumen

import Errors._
import interpreters._

case class Semantics(id: Option[String], 
                     // id is None if the semantics being implement
                     // does not have a name, for example the
                     // optimized interpreter with non-standard
                     // parameters
                     requiredPasses: Seq[String],
                     defaultPasses: Seq[String]) 

object Semantics {
  val S2012 = Semantics(Some("2012"), Seq("desugar-local"), Seq("SD"));
  val S2013 = Semantics(Some("2013"), Seq("desugar-toplevel"), Seq("SD"));
  val S2014 = Semantics(Some("2014"), Seq("desugar-local-inline"), Seq("SD"));
}


abstract class SemanticsImpl[+I <: Interpreter]
{
  val id : Seq[String] 
  val semantics: Semantics
  def interpreter() : I
  def parse(s: java.io.Reader) : Prog = Parser.run(Parser.prog, s)
  def parse(s: String) : Prog = Parser.run(Parser.prog, s)
  def applyPasses(p: Prog, extraPasses: Seq[String]) : Prog =
    PassManager.applyPasses(p, semantics.requiredPasses, semantics.defaultPasses, extraPasses)
  // withArgs returns when given an invalid argument, calling function
  // is expected to throw an error
  def applyRequiredPasses(p: Prog) : Prog = 
    PassManager.applyPasses(p, semantics.requiredPasses, Nil, Nil)
  def withArgs(args: List[String]) = args match {case Nil => this; case _ => null}
}

object SemanticsImpl {
  import Semantics._

  type CStore = SemanticsImpl[CStoreInterpreter]

  case class Reference(val semantics: Semantics) extends CStore {
    val i = semantics match {
      case S2012 => reference2012.Interpreter
      case S2013 => reference2013.Interpreter
      case S2014 => reference2014.Interpreter
    }
    val id : Seq[String] = i.id
    def interpreter() = i
  }
  object Imperative2012 extends CStore {
    val i = imperative2012.ImperativeInterpreter
    val semantics = S2012
    val id : Seq[String] = i.id
    def interpreter() = i
  }
  case class Parallel2012(numThreads: Int = -1, scheduler: String = "static") extends CStore {
    val semantics = S2012
    val id = Seq("parallel2012")
    def interpreter() = (numThreads, scheduler) match {
      case (-1, "static") => imperative2012.ParallelInterpreter.static
      case (_, "static") => imperative2012.ParallelInterpreter.static(numThreads)
      case (-1, "sharing") => imperative2012.ParallelInterpreter.sharing
      case (_, "sharing") => imperative2012.ParallelInterpreter.sharing(numThreads)
    }
    override def withArgs(args: List[String]) : Parallel2012 = args match {
      case ("static"|"sharing") :: tail => Parallel2012(numThreads, args(0)).withArgs(tail)
      case head :: tail if head.matches("\\d+") => Parallel2012(Integer.parseInt(head), scheduler).withArgs(tail)
      case Nil => this
      case _ => null
    }
  }
  case class Optimized(parDiscr: Boolean = true, parCont: Boolean = false , contWithDiscr: Boolean = false) extends CStore {
    val i = new optimized.Interpreter(parDiscr,parCont,contWithDiscr)
    val semantics = if (parDiscr == true && parCont == false && contWithDiscr == false) S2013
                    else S2013.copy(id = None)
    val id : Seq[String] = i.id
    def interpreter() = i
    override def withArgs(args: List[String]) : Optimized = args match {
      case "parDiscr" :: tail => Optimized(true, parCont, contWithDiscr).withArgs(tail)
      case "seqDiscr" :: tail => Optimized(false, parCont, contWithDiscr).withArgs(tail)
      case "parCont" :: tail => Optimized(parDiscr, true, contWithDiscr).withArgs(tail)
      case "seqCont" :: tail => Optimized(parDiscr, false, contWithDiscr).withArgs(tail)
      case "contWithDiscr" :: tail => Optimized(parDiscr, parCont, true).withArgs(tail)
      case "contWithCont" :: tail => Optimized(parDiscr, parCont, false).withArgs(tail)
      case Nil => this
      case _ => null
    }
  }
  case class Enclosure(i: RecursiveInterpreter = interpreters.enclosure.Interpreter.asPWL) extends SemanticsImpl[RecursiveInterpreter] {
    val semantics = Semantics(None, Seq("desugar-local"), Nil)
    val id : Seq[String] = i.id
    val desc = i.id
    def interpreter() = i
    override def withArgs(args: List[String]) : Enclosure = args match {
      case "pwl" :: tail => Enclosure(interpreters.enclosure.Interpreter.asPWL).withArgs(tail)
      case "evt" :: tail => Enclosure(interpreters.enclosure.Interpreter.asEVT).withArgs(tail)
      case Nil => this
      case _ => null
    }
  }

  // constants, for common choices, safe to compare against for
  // equality
  lazy val Ref2012 = Reference(S2012)
  lazy val Ref2013 = Reference(S2013)
  lazy val Ref2014 = Reference(S2014)
  lazy val Ref = Ref2014
  lazy val Opt2012 = Imperative2012
  lazy val Opt2013 = Optimized()
  lazy val EnclosurePWL = apply("enclosure-pwl")
  lazy val EnclosureEVT = apply("enclosure-evt")

  def apply(args0: String*) : SemanticsImpl[Interpreter] = {
    val args = args0.flatMap(_.split('-')).toList
    val res = args match {
      case "reference2012" :: Nil => Ref2012
      case "reference2013" :: Nil => Ref2013
      case ("" | "reference" | "reference2014") :: Nil => Ref2014
      case "parallel2012" :: tail => Parallel2012().withArgs(tail)
      case "imperative2012" :: Nil => Imperative2012
      case "optimized2012" :: Nil => Opt2012
      case "optimized2013" :: Nil => Opt2013
      case "optimized" :: tail => Optimized().withArgs(tail)
      case "enclosure" :: tail => Enclosure().withArgs(tail)
      case _ => null
    }
    if (res == null) 
      throw UnrecognizedInterpreterString(args.mkString("-"))
    res
  }

}
