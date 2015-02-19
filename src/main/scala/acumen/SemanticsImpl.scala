package acumen

import Errors._
import interpreters._
import interpreters.optimized.ContMode
import java.io.{File,InputStreamReader,FileInputStream}
import scala.util.parsing.input.{Position}
import scala.collection.mutable.{HashMap => MutHashMap}

case class Semantics(id: Option[String], 
                     // id is None if the semantics being implement
                     // does not have a name, for example the
                     // optimized interpreter with non-standard
                     // parameters
                     requiredPasses: Seq[String],
                     defaultPasses: Seq[String]) 

object Semantics {
  val S2012 = Semantics(Some("2012"), Seq("desugar-local"), Seq("SD"))
  val S2013 = Semantics(Some("2013"), Seq("desugar-toplevel"), Seq("SD"))
  val S2014 = Semantics(Some("2014"), Seq("desugar-local-inline"), Seq("SD"))
  val S2015 = Semantics(Some("2015"), Seq("desugar-local-inline"), Seq("SD"))
}

abstract class SemanticsSel
{
  def withArgs(args: List[String]) : SemanticsImpl[Interpreter]
  def argsHelpString : String
  val isOldSemantics = true // override and set to false if the
                          // semantics should be enabled
}

abstract class SemanticsImpl[+I <: Interpreter] extends SemanticsSel
{
  final def descr : String = SemanticsImpl.lookup(this) match {
    case Some(s) => s.display
    case None    => id.mkString("-")
  }
  def id = Seq(toString()) // override this to use something better
  val semantics: Semantics
  def interpreter() : I
  def parse(s: java.io.Reader) : Prog = Parser.run(Parser.prog, s, None)
  def parse(s: String) : Prog = Parser.run(Parser.prog, s)

  // Parse a program with includes
  // "dir" is the directory 
  def parse(s: java.io.Reader, dir: File, fn: Option[String]) : Prog = Prog(parseHelper(s,dir,fn,Nil,MutHashMap.empty))
  def parse(s: String, dir: File, fn: Option[String]) : Prog = parse(new java.io.StringReader(s),dir,fn)
  private def parseHelper(s: java.io.Reader, dir: File, fn: Option[String], includedFrom: List[Position],
                          seen: MutHashMap[ClassName, List[Position]]) : List[ClassDef] = {
    val file = fn map {f => new File(dir, f)}
    val (incl, defs) = Parser.run(Parser.fullProg, s, file)
    defs.foreach{case defn@ClassDef(cn,_,_,_) => 
      if (seen.contains(cn)) {
        val err = ClassIncludedTwice(cn, defn.pos :: includedFrom, seen(cn))
        if (includedFrom.nonEmpty) err.setPos(includedFrom.head)
        throw err
      } else {
        seen.put(cn, defn.pos :: includedFrom)
      }
    }
    incl.flatMap{case incl@Include(fn) => 
      val in = new InputStreamReader(new FileInputStream(new File(dir,fn)))
      parseHelper(in, dir, Some(fn), incl.pos :: includedFrom, seen)
    } ++ defs
  }

  def applyPasses(p: Prog, extraPasses: Seq[String]) : Prog =
    PassManager.applyPasses(p, semantics.requiredPasses, semantics.defaultPasses, extraPasses)
  // withArgs returns when given an invalid argument, calling function
  // is expected to throw an error
  def applyRequiredPasses(p: Prog) : Prog = 
    PassManager.applyPasses(p, semantics.requiredPasses, Nil, Nil)
  override def withArgs(args: List[String]) = args match {case Nil => this; case _ => null}
  override def argsHelpString : String = ""
}

object SemanticsImpl {
  import Semantics._

  type SI = SemanticsImpl[Interpreter]
  type CStore = SemanticsImpl[CStoreInterpreter]

  case class Reference(val semantics: Semantics) extends CStore {
    val i = semantics match {
      case S2012 => reference2012.Interpreter
      case S2013 => reference2013.Interpreter
      case S2014 => reference2014.Interpreter
      case S2015 => reference2015.Interpreter
    }
    override val isOldSemantics = !(semantics == S2015)
    def interpreter() = i
  }
  case class Enclosure2014(contraction: Boolean) extends CStore {
    override val isOldSemantics  = false
    val i = enclosure2014.Interpreter(contraction)
    val semantics = Semantics(None, Seq("desugar-local-inline"), Seq("SD"))
    def interpreter() = i
  }
  object Imperative2012 extends CStore {
    val i = imperative2012.ImperativeInterpreter
    val semantics = S2012
    def interpreter() = i
  }
  case class Parallel2012(numThreads: Int = -1, scheduler: String = "static") extends CStore {
    val semantics = S2012

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
    override def argsHelpString = "[-<num threads>]"
    // "static|sharing" options is considered experimental and should not be documented
  }
  case class Optimized(parDiscr: Boolean = true, contMode: ContMode = ContMode.Seq, 
                       contWithDiscr: Boolean = false) extends CStore
  {
    val i = new optimized.Interpreter(parDiscr,contMode,contWithDiscr)
    val semantics = if (parDiscr == true && contMode == ContMode.Seq && contWithDiscr == false) S2013
                    else if (parDiscr == true && contMode == ContMode.IVP && contWithDiscr == false) S2014
                    else S2014.copy(id = None)
    override val isOldSemantics = semantics != S2014
    def interpreter() = i
    override def withArgs(args: List[String]) : Optimized = args match {
      case "parDiscr" :: tail => copy(parDiscr = true).withArgs(tail)
      case "seqDiscr" :: tail => copy(parDiscr = false).withArgs(tail)
      case "parCont" :: tail => copy(contMode = ContMode.Par).withArgs(tail)
      case "seqCont" :: tail => copy(contMode = ContMode.Seq).withArgs(tail)
      case "IVP" :: tail => copy(contMode = ContMode.IVP).withArgs(tail)
      case "contWithDiscr" :: tail => copy(contWithDiscr = true).withArgs(tail)
      case "contWithCont" :: tail => copy(contWithDiscr = false).withArgs(tail)
      case Nil => this
      case _ => null
    }
    override def id = Array("optimized", 
                            if (parDiscr) "parDiscr" else "seqDiscr",
                            contMode match {case ContMode.Seq => "seqCont"; 
                                            case ContMode.Par => "parCont"; 
                                            case ContMode.IVP => "IVP"},
                            if (contWithDiscr) "contWithDiscr" else "contWithCont")
  }
  case class Optimized2015() extends CStore
  {
    val i = new optimized2015.Interpreter
    val semantics = S2015
    override val isOldSemantics = false
    def interpreter() = i
    override def withArgs(args: List[String]) : Optimized2015 = args match {
      /** Template for handling of arguments
       * case "SomeArgument" :: tail => copy(SomeFlag = true).withArgs(tail)  */
      case Nil => this
      case _ => null
    }
    /** Template for handling of arguments
      * override def id = Array("optimized2015", 
      *                      if (SomeFlag) "SomeName" else "") */
  }
  // Use this as a base for selecting the generic optimized semantics
  // that not trying to match a particular semantics
  object Optimized extends SemanticsSel {
    override def argsHelpString = 
        "[-parDiscr|-seqDiscr][-parCont|-seqCont|-IVP][-contWithDiscr|contWithCont]"
    override def withArgs(args: List[String]) = Optimized().withArgs(args)
  }
  sealed abstract class EventHandler
  case object PWL extends EventHandler
  case object EVT extends EventHandler
  case class Enclosure(eventHandler: EventHandler, contraction: Boolean = false) extends SemanticsImpl[RecursiveInterpreter] {
    val semantics = Semantics(None, Seq("desugar-local"), Nil)
    def interpreter() = (eventHandler match {
      case PWL => enclosure.Interpreter.PWL
      case EVT => enclosure.Interpreter.EVT
    }).withContraction(contraction)
  }

  // constants, for common choices, safe to compare against for
  // equality
  lazy val Ref2012 = Reference(S2012)
  lazy val Ref2013 = Reference(S2013)
  lazy val Ref2014 = Reference(S2014)
  lazy val Ref2015 = Reference(S2015)
  lazy val Ref = Ref2014
  lazy val Opt2012 = Imperative2012
  lazy val Opt2013 = Optimized()
  lazy val Opt2014 = Optimized(contMode = ContMode.IVP)
  lazy val Opt2015 = Optimized2015()

  case class Sel(si: SemanticsSel, 
                 // First id is the display name
                 // Second id is internal name
                 // Everything else are aliases
                 hidden: Boolean,
                 ids: String*) {def display = ids(0); def internal = ids(1)}
  def sel(si: SemanticsSel, ids: String*) = Sel(si,false,ids:_*)
  def exp(si: SemanticsSel, ids: String*) = Sel(si,true,ids:_*)
  val selections = 
    List(sel(Ref2015, "2015 Reference", "reference2015", "reference"),
         sel(Opt2015, "2015 Optimized", "optimized2015", ""),
         sel(Ref2014, "2014 Reference", "reference2014"),
         sel(Opt2014, "2014 Optimized", "optimized2014"),
         sel(Ref2013, "2013 Reference", "reference2013"),
         sel(Opt2013, "2013 Optimized", "optimized2013"),
         sel(Ref2012, "2012 Reference", "reference2012"),
         sel(Opt2012, "2012 Optimized", "optimized2012", "imperative2012"),
         sel(Parallel2012(), "2012 Parallel", "parallel2012"),
         sel(Enclosure(PWL), "2013 PWL", "enclosure-pwl"),
         sel(Enclosure(EVT), "2013 EVT", "enclosure-evt"),
         sel(Enclosure(PWL,true), "2013 PWL (Contraction)", "enclosure-pwl-contraction"),
         sel(Enclosure(EVT,true), "2013 EVT (Contraction)", "enclosure-evt-contraction"),
         sel(Enclosure2014(false), "2014 Enclosure", "enclosure2014"),
         sel(Enclosure2014(true), "2014 Enclosure (Contraction)", "enclosure2014-contraction"),
         exp(Optimized, "Optimized", "optimized"))

  def lookup(si: SemanticsSel) : Option[Sel] = 
    selections.find{case Sel(si0, _, _*) => si == si0}

  def apply(toFind: String) : SI = {
    val candidates : Seq[(SemanticsSel, String)] = selections.flatMap {
      case Sel(si, _, ids@_*) => ids.collect {
        case id if toFind.startsWith(id) => (si, id)}}
    if (candidates.isEmpty) throw UnrecognizedSemanticsString(toFind)
    val (si, id) = candidates.maxBy{case (_, id) => id.length}
    if (si.isOldSemantics & !Main.enableOldSemantics)
      throw DisabledSemantics(toFind)
    val rest = toFind.substring(id.length)
    if (rest.isEmpty) return si.withArgs(Nil)
    if (rest(0) != '-') throw UnrecognizedSemanticsString(toFind)
    val args = rest.substring(1).split('-').toList
    val res = si.withArgs(args)
    if (res == null) throw UnrecognizedSemanticsString(toFind)
    return res
  }

  def helpText(full: Boolean) : String = {
    val sels = if (full) selections else selections.filter{case Sel(_,hidden,_*) => !hidden}
    sels.map{case Sel(si, _, display, id, alias@_*) => 
                     "  %-23s %s\n".format(id + si.argsHelpString,
                                           (display::alias.toList).map{"\"" + _ + "\""}.mkString(" "))}.mkString("")
  }

  def apply(spec: SemanticsSpec) : SI = {
    try {
      apply(spec.s)
    } catch {
      case e:PositionalAcumenError => e.setPos(spec.pos); throw e
    }
  }

}
