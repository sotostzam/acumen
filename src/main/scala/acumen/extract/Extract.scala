// Extract a Hybrid automaton

// Mostly follows algo.txt; however, the data structures used here are
// slightly different and the order the steps is not exactly the same.
package acumen
package extract

import scala.collection.mutable.{ArrayBuffer,ListBuffer,Map=>MutMap}
import scala.text._
import scala.text.Document.nest

import Util._
import CondImplicits._
import Pretty._

object Extract {
  // Constant
  val MODE = Name("$mode", 0)
  val MODE_VAR = Dot(Var(Name("self", 0)), MODE)

  case class Mode(val label: String,
                  var claims: Cond = Nil,
                  var actions: List[ContinuousAction] = Nil, 
                  var resets: List[Reset] = Nil,
                  var preConds: Cond = Cond.True,
                  var trans: Boolean = false) 
  {
    def cont = !trans
    def dump = ("mode " + label + (if (trans) " # trans \n" else "\n") +
                "  precond " + preConds + "\n" +
                "  claim " + claims + "\n" +
                "  " + pprint(nest(2,pretty(resets.map{_.toAST:Action} ++ actions.map{Continuously(_):Action}))) + "\nend\n")
    def markDead() {claims = Nil; actions = Nil; resets = Nil; preConds = Cond.False; trans = false}
  }
  // ^^ note: "preConds" are the conditions that must always hold when
  // in this mode, both after a discr. and cont. step.  Thus it must
  // not contain any cont. var changed in this mode.

  case class Reset(var conds: Cond,
                   var actions: ListBuffer[Assign] = ListBuffer.empty) 
  {
    def toAST: IfThenElse =
      IfThenElse(conds.toExpr,
                 actions.map(Discretely(_)).toList, Nil)
    def mode : Option[String] = actions.collectFirst{case Assign(lhs, Lit(GStr(l))) if getName(lhs).orNull == MODE => l}
    def mode_=(label: Option[String]) = {
      actions = actions.filter{a => getName(a.lhs).orNull != MODE}
      label.foreach{l => actions.prepend(Assign(MODE_VAR, Lit(GStr(l))))}
    }
  }
}

class Extract(val prog: Prog, private val debugMode: Boolean = false) 
{
  import Extract._
  import ExtractPasses._

  //
  //  Initialization
  //

  val origDef = {
    if (prog.defs.size > 1) 
      throw OtherUnsupported("Multiple objects not supported.")
    if (prog.defs(0).name != ClassName("Main"))
      throw OtherUnsupported("Could not find Main class.")
    prog.defs(0)
  }
  var init = origDef.priv.collect{case Init(v,ExprRhs(e)) => (v,e); case init => throw UnhandledSyntax(init)}
  var body : IfTree.Node = IfTree.create(origDef.body)
  val simulatorName = origDef.fields(0)
  var simulatorAssigns = body.extractSimulatorAssigns(simulatorName)
  var modes : ListBuffer[Mode] = ListBuffer.empty
  var resets : List[Reset] = null
  var modeVars : Map[Name,Set[GroundValue]] = null

  //
  // Entry point
  //

  // unsafe assumptions, currently unused
  //var assumeNoResetLoops = true // this includes loops that are really a fixed point
  //var assumeNoContEqMeansTrans = false

  var pif = "handle";
  var convertLevel = 2;
  var cleanupLevel = 2;

  def run() = {
    dumpPhase("EXTRACTED")
    pif match {
      case "ignore" =>
      case "reject" => rejectParallelIfs(body)
      case "handle" => handleParallelIfs(body); dumpPhase("HANDLE PARALLEL IF")
      case _ => throw Errors.ShouldNeverHappen()
    }
    counter = 100; dumpPhase("PRE CONVERT")
    convertLevel match {
      case _ => convert()
      //case _ => throw Errors.ShouldNeverHappen()
    }
    counter = 200; dumpPhase("PRE CLEANUP")
    cleanupLevel match {
      case 0 => 
      case 1 => cleanUpSimple()
      case 2 => cleanUp()
      case _ => throw Errors.ShouldNeverHappen()
    }
    counter = 300; dumpPhase("FINAL")
    toAST
  }

  def dump(label: String = "", fileName: String = null) = {
    val out : java.io.PrintStream = if (fileName == null) System.out else new java.io.PrintStream(new java.io.File(fileName + ".out"))
    out.println("*** " + label + " ***\n")
    out.println(pprint(DocGroup(DocNest(2, 
          "private" :/: breakWith(";", initPart map pretty[Init])) :/: "end")) + "\n")
    //if (simulatorAssigns.nonEmpty)
    //  println(pprint(DocGroup(DocNest(2, breakWith(";", simulatorAssigns.map{a => pretty[Action](Discretely(a))}.toList) + "\n"))))
    if (simulatorAssigns.nonEmpty)
      out.println(simulatorAssigns.map{a => pprint[Action](Discretely(a))}.mkString(";\n") + "\n")
    if (body != null)
      out.println(IfTree.dumpLeafs(Nil, body) + "\n")
    modes.foreach{m => out.println(m.dump)}
    out.println("---\n")
    if (fileName != null)
      out.close
  }

  val res = run() // FIXME: Eventually remove

  //
  // Various ways to convert from an IfTree to a set of Modes with
  // Resets by populating "modes".  When done IfTree (i.e. body) will
  // no longer have any actions.
  //
 
  // The simplest most straightforward way.  
  def convert() : Unit = {
    modeVars = getModeVars(body)
    val (m,r) = extractAll(body, modeVars.keySet); dumpPhase("EXTRACT")
    modes = m;
    resets = r.toList;
    addInit()
    addResets()
  }

  // The minimal amout of clean up to be able eliminate unneeded state
  // variables and hense allow the model to run with the enclosure
  // interpreter
  def cleanUpSimple() : Unit = {
    pruneDeadModes()
    pruneResetConds()

    killDeadVars()
  }

  def cleanUp() : Unit = {
    pruneDeadModes()
    pruneResetConds()

    markTransModes()

    splitModes()

    resolveModes()

    // now the loop
    eliminateTrueOnlyModes()
    mergeDupModes()
    cleanUpTransModes()
    resolveModes()

    eliminateTrueOnlyModes()
    mergeDupModes()
    cleanUpTransModes()
    resolveModes()

    // eliminateTrueOnlyModes()
    // mergeDupModes()
    // cleanUpTransModes()
    // resolveModes()
    // done, fixme: this should be a fixed point loop

    killDeadVars()

    cleanUpAssigns()
  }
  
  var counter = 0;
  def dumpPhase(name: String) = {
    if (debugMode)
      dump("", "%03d-%s".format(counter, name.toLowerCase.replace(' ', '_')))
    counter += 1;
  }

  //
  // Convert the result back to an AST
  //

  def toAST = {
    // Converts the modes into a big switch and return a new prog
    // The CONVERT TO SWITCH and FIX UP steps
    val initMode = modes.head
    assert(initMode.label == "Init")
    val (simAssigns, otherAssigns) = getSimulatorAssigns(simulatorName, initMode.resets.head.actions)
    val initP = otherAssigns.toList.map{case Assign(Dot(Var(Name("self",0)), f), e) => Init(f,ExprRhs(e))}

    val theSwitch = Switch(MODE_VAR,
                           modes.tail.map{m => Clause(GStr(m.label),
                                                      m.claims.toExpr,
                                                      m.resets.map(_.toAST) ++
                                                      m.actions.map(Continuously(_)).toList)}.toList)
    val newMain = ClassDef(origDef.name,
                           origDef.fields,
                           initP, 
                           List(theSwitch) ++ simAssigns.map{Discretely(_)})

    new Prog(List(newMain))
  }

  def initPart = 
    Init(MODE,ExprRhs(Lit(GStr("Init")))) :: init.toList.map{case (v,e) =>  Init(v,ExprRhs(e))}


  //
  // Passes
  //

  def ep = ExtractPasses

  def addInit() {
    val initAsAssign = init.toSeq.map{case (k,v) => Assign(Dot(Var(Name("self", 0)), k),v)}
    val initActions = ListBuffer.empty ++ initAsAssign ++ simulatorAssigns :+ Assign(MODE_VAR, Lit(GStr("D0")))
    val eqInit = Cond.eq(MODE,GStr("Init"))
    val notEqInit = Cond.not(eqInit)
    resets.foreach {r => r.conds = Cond.and(notEqInit, r.conds)}
    resets = Reset(eqInit,initActions) :: resets
    modes.prepend(Mode("D0", trans=true,
                       preConds = discrConds(Util.postConds(eqInit, initActions),modeVars.keySet)))
    modes.prepend(Mode("Init", trans=true,
                       preConds = Cond.eq(MODE, GStr("Init"))))
    init = Nil
    simulatorAssigns = Nil
    dumpPhase("INIT MODE")
  }

  def addResets() {ep.addResets(resets, modes); dumpPhase("ADD RESETS")}

  def pruneDeadModes() {ep.pruneDeadModes(modes); dumpPhase("PRUNE DEAD MODES")}

  def pruneResetConds() {ep.pruneResetConds(modes); dumpPhase("PRUNE RESET CONDS")}

  def markTransModes() {ep.markTransModes(modes); dumpPhase("MARK TRANS MODES")}

  def eliminateTrueOnlyModes() {ep.eliminateTrueOnlyModes(modes); dumpPhase("ELIMINATE TRUE ONLY")}

  def splitModes() = {ep.splitModes(modes,modeVars); dumpPhase("SPLIT MODES");}

  def mergeDupModes() {ep.mergeDupModes(modes); dumpPhase("MERGE DUP MODES")}

  def cleanUpTransModes() {ep.cleanUpTransModes(modes); dumpPhase("CLEAN UP TRANS MODE")}

  def resolveModes() {ep.resolveModes(modes); dumpPhase("RESOLVE MODES")}
  // ^^ find mode with true precond based on reset post, if more than
  // one error split mode to be able to enhance precond
  
  def cleanUpAssigns() {ep.cleanUpAssigns(modes); dumpPhase("CLEAN UP ASSIGNS")}
  
  def killDeadVars() {ep.killDeadVars(modeVars.keys, modes); dumpPhase("KILL DEAD VARS")}

  def extractInitialConds() {
    
  }
}

