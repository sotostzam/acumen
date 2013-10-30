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

class Extract(val prog: Prog, private val debugMode: Boolean = false) 
{
  import Extract._

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
  var init = MutMap(origDef.priv.collect{case Init(v,ExprRhs(e)) => (v,e); case init => throw UnhandledSyntax(init)}:_*)
  var body : IfTree.Node = IfTree.create(origDef.body)
  val simulatorName = origDef.fields(0)
  var simulatorAssigns = body.extractSimulatorAssigns(simulatorName)
  var modes = ListBuffer.empty[Mode]
  var initMode = "D0"

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
      case 1 => convertSimple()
      case 2 => convertWithPreConds()
      case _ => throw Errors.ShouldNeverHappen()
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
  def convertSimple() : Unit = {
    modes = extractModes(body.contOnly); dumpPhase("EXTRACT MODES")
    val resets = extractResets(body.discrOnly); dumpPhase("EXTRACT RESETS")
    sanity(body); body = null; dumpPhase("SANITY")

    modes.prepend(Mode("D0", trans=true)); dumpPhase("INIT MODE")

    addResets(resets, modes); dumpPhase("ADD RESETS")
  }

  def convertWithPreConds() : Unit = {
    modes = extractModes(body.contOnly); dumpPhase("EXTRACT MODES")
    val resets = extractResets(body.discrOnly); dumpPhase("EXTRACT RESETS")
    sanity(body); body = null; dumpPhase("SANITY")

    val env = Env(modeVars = getModeVars(resets, modes))

    enhanceModePreCond(resets, modes, env); dumpPhase("ENHANCE MODE PRECONDS")
    modes.prepend(Mode("D0", trans=true, preConds = discrConds(initPostCond(init.toList),env.modeVars))); dumpPhase("INIT MODE")

    addResets(resets, modes); dumpPhase("ADD RESETS")
  }

  // The minimal amout of clean up to be able eliminate unneeded state
  // variables and hense allow the model to run with the enclosure
  // interpreter
  def cleanUpSimple() : Unit = {
    pruneDeadModes(modes); dumpPhase("PRUNE DEAD MODES") 
    pruneResetConds(modes); dumpPhase("PRUNE RESET CONDS")

    killDeadVars(init, modes); dumpPhase("KILL DEAD VARS")
  }

  def cleanUp() : Unit = {
    pruneDeadModes(modes); dumpPhase("PRUNE DEAD MODES") 

    pruneResetConds(modes); dumpPhase("PRUNE RESET CONDS")

    killDeadResets(modes); dumpPhase("KILL DEAD RESETS")

    // do this early so the initial mode is not a special case
    cleanUpInitMode(); dumpPhase("CLEAN UP INIT")

    markTransModes(modes); dumpPhase("MARK TRANS MODES")

    eliminateTrueOnlyModes(); dumpPhase("ELIMINATE TRUE ONLY")

    mergeDupModes(modes); dumpPhase("MERGE DUP MODES")
    eliminateTrueOnlyModes(); dumpPhase("ELIMINATE TRUE ONLY")

    cleanUpTransModes(modes); dumpPhase("CLEAN UP TRANS MODE")
    eliminateTrueOnlyModes(); dumpPhase("ELIMINATE TRUE ONLY")
    
    cleanUpAssigns(modes); dumpPhase("CLEAN UP ASSIGNS")
    
    killDeadVars(init, modes); dumpPhase("KILL DEAD VARS")
  }

  var counter = 0;
  def dumpPhase(name: String) = {
    if (debugMode)
      dump("", "%03d-%s".format(counter, name.toLowerCase.replace(' ', '_')))
    counter += 1;
  }

  // Attempt to remove the init mode, this passes modifies most of the
  // internal state so its easiest to define it here rather than in
  // the companion object like most of the other operations
  def cleanUpInitMode() : Unit = {
    val mode = modes.find{_.label == initMode}.get
    mode.resets.find{_.conds == Cond.True} match {
      case None => /* nothing */
      case Some(r) => 
        val (sim, non) = getSimulatorAssigns(simulatorName, r.actions)
        simulatorAssigns ++= sim
        val whatsLeft = non.flatMap{ a => a match {
          case Assign(n,Lit(v)) => getName(n) match {
            case Some(n) => if (n != MODE) init(n) = Lit(v); None
            case None => Some(a)
          }
          case _ => Some(a)
        }}
        if (whatsLeft.isEmpty) {
          modes -= mode
          initMode = r.mode.get
        } else {
          r.actions = whatsLeft
        }
    }
  }

  // Wrapper around Extract.eliminateTrueOnlyModes to hande the
  // initialization case
  def eliminateTrueOnlyModes() {
    modes.prepend(Mode("Init", preConds = Cond.False, resets = placeHolderReset(initMode)))
    Extract.eliminateTrueOnlyModes(modes)
    assert(modes.head.label == "Init")
    initMode = modes.head.resets.head.mode.get
    modes = modes.tail
  }

  //
  // Convert the result back to an AST
  //

  def toAST = {
    // Converts the modes into a big switch and return a new prog
    // The CONVERT TO SWITCH and FIX UP steps
    val theSwitch = Switch(MODE_VAR,
                           modes.map{m => Clause(GStr(m.label),
                                                 m.claims.toExpr,
                                                 m.resets.map(_.toAST) ++
                                                 m.actions.map(Continuously(_)).toList)}.toList)
    val newMain = ClassDef(origDef.name,
                           origDef.fields,
                           initPart, 
                           List(theSwitch) ++ simulatorAssigns.map{Discretely(_)})

    new Prog(List(newMain))
  }

  def initPart = 
    Init(MODE,ExprRhs(Lit(GStr(initMode)))) :: init.toList.map{case (v,e) =>  Init(v,ExprRhs(e))}

  //
  // Additional utility functions
  //

}

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
    def dump = (
                "mode " + label + (if (trans) " # trans \n" else "\n") +
                "  precond " + pprintOneLine(pretty(preConds.toExpr)) + "\n" +
                "  claim " + pprintOneLine(pretty(claims.toExpr)) + "\n" +
                "  " + pprint(nest(2,pretty(resets.map{_.toAST:Action} ++ actions.map{Continuously(_):Action}))) + "\nend\n")
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

  case class Env(modeVars: Seq[Name] = Nil)

  def rejectParallelIfs(body: IfTree[_], msg: String = "Parallel conditionals unsupported.") {
    if (body.children.map{_.megId}.distinct.length > 1)
      throw OtherUnsupported(msg)
    body.children.foreach{rejectParallelIfs(_)}
  }
  def handleParallelIfs(body: IfTree[_]) {
    val megIds = body.children.groupBy{_.megId}
    if (megIds.size > 1) {
      val node = body.node
      val newMegId = megIds.keys.min
      node.children = node.children.filter{v => !megIds.get(v.megId).nonEmpty}
      def allPossib(chosen: List[IfTree.Node], toChoose: List[Seq[IfTree.Node]]) {
        if (toChoose.nonEmpty) {
          val (head :: tail) = toChoose
          head.foreach{choice => allPossib(chosen :+ choice, tail)}
        } else {
          node.children += IfTree.mergeNodes(chosen,newMegId)
        }
      }
      allPossib(Nil, megIds.values.map{_.map{_.node}}.toList)
    }
    body.children.foreach{handleParallelIfs(_)}
  }

  // Extract the mode and remove continuous assignments from the tree
  // In also adds the necessary "magic" so that we are guaranteed to
  // always know what mode to go into after a reset
  def extractModes(root: IfTree.ContView) : ListBuffer[Mode] = {
    val modes = new ListBuffer[Mode]
    var idx = 1 
    def doit(parentActions: List[ContinuousAction], body: IfTree.ContView) : Unit = {
      val actions = parentActions ++ body.actions
      body.actions.clear()
      if (body.children.nonEmpty) {
        body.children.foreach{doit(actions, _)}
      } else {
        val label = "C" + idx
        modes += Mode(label, claims = body.claims, actions = actions)
        body.node.discrAssigns += Assign(MODE_VAR, Lit(GStr(label)))
        idx += 1
      }
    }
    doit(Nil, root)
    modes
  }
  // Extract resets and (optionally) remove the discrete assignments
  // from the if tree
  def extractResets(root: IfTree.DiscrView, clearExtracted: Boolean = true) : List[Reset] = {
    val resets = new ListBuffer[Reset]
    def doit(parentActions: List[Assign], body: IfTree.DiscrView) : Unit = {
      val actions = parentActions ++ body.actions
      if (clearExtracted)
        body.actions.clear()
      if (body.children.nonEmpty) {
        body.children.foreach{doit(actions, _)}
      } else {
        resets += Reset(body.conds, ListBuffer(actions:_*))
      }
    }
    doit(Nil, root)
    resets.toList
  }
  // Check that no actions are left in the if tree.  If there is
  // something left it means we have syntax we can not support
  def sanity(root: IfTree.Node) : Unit = {
    root.foreach{node => 
      if (node.actions.nonEmpty) throw UnhandledSyntax(node.actions.head)
    }
  }

  // If we are not already in a mode after a reset go into the special
  // catch all mode
  def ensureModeCatchAll(resets: Seq[Reset], catchAllMode: String) : Unit = {
    resets.filter(_.mode.isEmpty).foreach { r => 
      r.mode = Some(catchAllMode)
    }
  }

  def enhanceModePreCond(resets: Seq[Reset], modes: ListBuffer[Mode], env: Env) {
    assert({val modes = resets.map{_.mode}; !modes.contains(None) && modes.distinct.size == modes.size})
    resets.foreach{r =>
      val postConds = Util.postConds(r.conds, r.actions)
      val modePreConds = discrConds(postConds,env.modeVars)
      val modeLabel = r.mode.get
      val mode = modes.find{_.label == modeLabel}.get
      mode.preConds ++= modePreConds
    }
  }

  def addResets(resets: List[Reset], modes: Seq[Mode]) {
    modes.foreach { m =>
      m.resets = resets.map{_.copy()}
    }
  }

  def pruneDeadModes(modes: ListBuffer[Mode]) {
    modes --= modes.filter{m => m.preConds == Cond.False}
  }

  // Remove redundant conds from the resets based on the modes precond
  def pruneResetConds(modes: Seq[Mode]) : Unit = {
    modes.foreach{m => 
      m.resets.foreach{r => 
        r.conds = r.conds.eval(m.preConds)
      }
    }
  }

  def killDeadResets(modes: Seq[Mode]) : Unit = {
    modes.foreach{m => 
      m.resets = m.resets.filter{_.conds != Cond.False}
    }
  }

  def cleanUpAssigns(modes: Seq[Mode]) : Unit = {
  // for each mode, kill all unnecessary discr. assignemnts in the
  // resets, if the reset than has no actions other than to go back
  // into the same mode, kill it.
    modes.foreach{m => 
      m.resets.foreach{r=>
        val preConds = Cond.and(m.preConds,r.conds)
        r.actions = r.actions.filter{case a@Assign(lhs,rhs) => !((getName(lhs), rhs) match {
          case (Some(name), Lit(value)) => 
            preConds.exists(_ == Cond.Eq(name,value))
          case _ => false
        })}
      }
      m.resets = m.resets.filter{r => 
        !(r.actions.length == 1 && r.mode == Some(m.label))
      }
    }
  }

  def killDeadVars(init: MutMap[Name,Expr], modes: Seq[Mode]) = {
    val kill = getDeadVars(init.keys, modes)

    modes.foreach{m =>
      m.resets.foreach{r => 
        r.actions = r.actions.filter{case Assign(rhs,_) => getName(rhs) match {
          case Some(name) => !kill.exists(_ == name)
          case _ => true
        }}
      }
      m.resets = m.resets.filter(_.actions.nonEmpty)
    }

    init--= kill
  }

  def getDeadVars(allVars: Iterable[Name], modes: Seq[Mode]) = {
    val contDeps = modes.flatMap(_.actions.flatMap{a => 
      extractRHSDeps(a) ++ extractLHSDeps(a)}).distinct
    val discrDeps = modes.flatMap(_.resets.flatMap{r =>
      r.conds.flatMap(_.deps).toList ++ r.actions.flatMap{case Assign(_,rhs) => extractDeps(rhs)}
    }).distinct
    val allDeps = (contDeps ++ discrDeps).distinct :+ MODE // $mode is special and needs to be kept
    allVars.filter{case Name(x,_) => !allDeps.exists{case Name(y,_) => x == y}}
  }

  def mergeDupModes(modes: Seq[Mode]) {
    val dups = modes.groupBy{m => (m.preConds, m.claims, m.actions, m.trans)}.filter{case (_, v) => v.length > 1}
    dups.values.foreach{ms =>
      val target :: toKill = ms.sortWith{(a,b) => a.label < b.label}.toList
      toKill.foreach{m =>
        m.resets = placeHolderReset(target.label)
        m.actions = Nil
        m.trans = true
      }
    }
  }

  // This pass needs to be run before dead resets are eliminated There
  // should be a single reset that trans into each mode and at least
  // one reset should fire (possible more).  If the reset that goes
  // back into this mode is dead (i.e. conds == false) than it follows
  // that there is no way to end up in this mode, thus the mode is
  // effectively trans.
  def markTransModesPreCleanup(modes: Seq[Mode]) {
    modes.filter{!_.trans}.foreach{m =>
      val rs = m.resets.filter{r => r.mode.orNull == m.label}
      assert(rs.size == 1)
      if (rs.head.conds == Cond.False) {
        m.actions = Nil
        m.trans = true
      }
    }
  }
  // More robust version that can be run before or after dead resets
  // are eliminated
  def markTransModes(modes: Seq[Mode]) {
    modes.filter{!_.trans}.foreach{m =>
      val rs = m.resets.filter{r => r.mode.orNull == m.label}
      assert(rs.size <= 1)
      if (rs.isEmpty || rs.head.conds == Cond.False) {
        m.actions = Nil
        m.trans = true
      }
    }
  }

  // See if any trans. mode can be eliminated.  A trans. mode can be
  // elimited if there is another mode with the same set of resets.
  // Such a mode will (by the way the mode/resets is constructed) will
  // have the same preconds sans the special $mode variable (which
  // does does affect any of the resets).
  def cleanUpTransModes(modes: Seq[Mode]) {
    modes.filter{_.trans}.foreach{m =>
      var candidates = modes.filter{m2 => !m2.trans && m.preConds == m2.preConds}
      if (candidates.size != 1) {
        candidates = modes.filter{m2 => m.preConds == m2.preConds}
        if (!candidates.isEmpty && candidates.head.label != m.label)
          candidates = List(candidates.head)
        else
          candidates = Nil
      }
      if (candidates.size == 1) {
        val target = candidates.head
        m.resets = placeHolderReset(target.label)
      } 
    }
  }
  def placeHolderReset(label: String) = 
      List(Reset(Cond.True, ListBuffer(Assign(MODE_VAR, Lit(GStr(label))))))

  def eliminateTrueOnlyModes(modes: ListBuffer[Mode]) = {
    val toRemove = ListBuffer.empty[Mode]
    modes.filter{m => m.trans && m.resets.length == 1 && m.resets.head.conds == Cond.True}.foreach{m =>
      val actions = m.resets.head.actions
      val rs = getResetsWithMode(m.label, modes)
      val canFold = rs.forall{r => canFoldActions(r.actions, actions)}
      if (canFold) {
        rs.foreach{r => foldActions(r.actions, actions)}
        toRemove += m
      }
    }
    modes --= toRemove
  }
  def getResetsWithMode(mode: String, modes: Seq[Mode]) : Seq[Reset] = 
    modes.flatMap{m => m.resets.filter{r => r.mode.orNull == mode}}
  def canFoldActions(first: Seq[Assign], second: Seq[Assign]) : Boolean = 
      // FIXME: Give some intuition on why these tests are needed.
      // (I think it has something to so with the new semantics and the
      // fact that repeated assignments to the same var are not allowed)
      getDeps(first, _.rhs).intersect(getDeps(second, _.lhs)).isEmpty &&
      getDeps(first, _.lhs).intersect(getDeps(second, _.rhs)).isEmpty
  def getDeps(actions: Seq[Assign], f: Assign => Expr) =
    actions.flatMap(a => extractDeps(f(a))).distinct
  def foldActions(first: ListBuffer[Assign], second: Seq[Assign]) : Unit =
    second.foreach { action =>
      val toFind = getName(action.lhs)
      val idx = if (toFind == None) -1 else first.indexWhere(a => getName(a.lhs) == toFind)
      if (idx != -1)
        first(idx) = action
      else
        first += action
    }
  
  //
  // Additional utility
  // 

  def getModeVars(resets: Seq[Reset], modes: Seq[Mode]) : Seq[Name] = { 
    val contVars = modes.flatMap{m => m.actions.flatMap{a => extractLHSDeps(a)}}.toSet
    resets.flatMap{_.conds.collect{case Cond.Eq(n,_) if !contVars.contains(n) => n}}.distinct
  }
}