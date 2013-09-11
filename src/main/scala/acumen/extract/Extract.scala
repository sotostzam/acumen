// Extract a Hybrid automaton

// Mostly follows algo.txt; however, the data structures used here are
// slightly different and the order the steps is not exactly the same.

package acumen
package extract

import scala.collection.mutable.{ArrayBuffer,ListBuffer,Map=>MutMap}

import Util._
import CondAsSeq._

object Extract {
  // Constant
  val MODE = Name("$mode", 0)
  val MODE_VAR = Dot(Var(Name("self", 0)), MODE)

  case class Mode(val label: String,
                  //var claims: List[Cond] = Nil,
                  var actions: List[ContinuousAction] = Nil, 
                  var resets: List[Reset] = Nil,
                  var preConds: Cond = null) 
  {
    if (preConds == null) preConds = Cond.Eq(MODE, GStr(label))
    def claims = preConds // this is for debugging, need to eventually
                          // be more careful with claims
    def cont = actions.nonEmpty
    def trans = actions.isEmpty
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
        modes += Mode(label, actions = actions)
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

  // Find a cont. mode we can go into based on its preconds, if that
  // fails create a new transient mode
  def ensureMode(resets: Seq[Reset], modes: ListBuffer[Mode]) {
    val modeVars = resets.flatMap{_.conds.flatMap{case Cond.Eq(n,_) => Some(n); case _ => None}}.distinct
    var counter = 1
    resets.foreach { r => 
      var postConds = Util.postConds(r.conds, r.actions)
      var candidates = modes.filter{m => m.cont && m.preConds.eval(postConds) == Cond.True}.toList
      val filteredPostConds = discrConds(postConds, modeVars)
      //println("pc = " + postConds)
      if (candidates.size != 1) {
        // see if there is already a trainsint mode with the same preConds
        candidates = modes.filter{m => m.trans && m.preConds == filteredPostConds}.toList
      }
      if (candidates.size != 1) {
        val m = Mode("D" + counter, preConds = filteredPostConds)
        counter += 1
        modes += m
        candidates = List(m)
      }
      assert(candidates.size == 1)
      r.mode = Some(candidates.head.label)
    }
  }

  def enhanceModePreCond(resets: Seq[Reset], modes: ListBuffer[Mode]) {
    val modeVars = resets.flatMap{_.conds.flatMap{case Cond.Eq(n,_) => Some(n); case _ => None}}.distinct
    assert({val modes = resets.map{_.mode}; !modes.contains(None) && modes.distinct.size == modes.size})
    resets.foreach{r =>
      val postConds = Util.postConds(r.conds, r.actions)
      val modePreConds = discrConds(postConds,modeVars)
      val modeLabel = r.mode.get
      val mode = modes.find{_.label == modeLabel}.get
      //println("BB: " + mode.preConds)
      mode.preConds ++= modePreConds
      //println("AA: " + mode.preConds)
    }
  }

  def addResets(resets: List[Reset], modes: Seq[Mode]) {
    modes.foreach { m =>
      m.resets = resets.map{_.copy()}
    }
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
  // for each mode, kill all unnecessary discr. assignemnts in
  // the resets, if the reset than has no actions, kill it.
    modes.foreach{m => 
      m.resets.foreach{r=>
        val preConds = Cond.and(m.preConds,r.conds)
        // keep the mode assignment operation even if the mode does not
        // change as it is needed by the enclosure interpreter
        var modeAssign : Assign = null
        r.actions = r.actions.filter{case a@Assign(lhs,rhs) => !((getName(lhs), rhs) match {
          case (Some(name), Lit(value)) => 
            val res = preConds.exists(_ == Cond.Eq(name,value))
            if (res && name == MODE) modeAssign = a
            res
          case _ => false
        })}
        if (r.actions.nonEmpty && modeAssign != null) 
          r.actions += modeAssign
      }
      m.resets = m.resets.filter(_.actions.nonEmpty)
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

  def eliminateTrueOnlyModes(modes: ListBuffer[Mode], initMode: String) = {
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
}

class Extract(val prog: Prog) 
{
  import Extract._

  // Initialization
  val origDef = {
    if (prog.defs.size > 1) 
      throw OtherUnsupported("Multiple objects not supported.")
    if (prog.defs(0).name != ClassName("Main"))
      throw OtherUnsupported("Could not find Main class.")
    prog.defs(0)
  }
  var init = MutMap(origDef.priv.collect{case Init(v,ExprRhs(e)) => (v,e); case init => throw UnhandledSyntax(init)}:_*)
  val body : IfTree.Node = IfTree.create(origDef.body)
  val simulatorName = origDef.fields(0)
  var simulatorAssigns = body.extractSimulatorAssigns(simulatorName)
  var modes = ListBuffer.empty[Mode]
  var initMode = "D0"

  //
  // Various ways to convert from an IfTree to a set of Modes with
  // Resets by populating "modes".  When done IfTree (i.e. body) will
  // no longer have any actions.
  //

  // The simplest most straightforward way.  
  def convertSimple() = {
    modes = extractModes(body.contOnly)
    val resets = extractResets(body.discrOnly)
    sanity(body)

    modes.prepend(Mode("D0"))

    addResets(resets, modes)
  }

  def convertMinimalModes() = {
    modes = extractModes(body.contOnly.pruned)
    val resets = extractResets(body.discrOnly)
    sanity(body)

    modes.prepend(Mode("D0"))
    ensureModeCatchAll(resets, "D0")

    addResets(resets, modes)
  }

  def convertSimplePreCond() = {
    modes = extractModes(body.contOnly.pruned)
    val resets = extractResets(body.discrOnly)
    sanity(body)

    modes.prepend(Mode("D0", preConds = initPostCond(init.toList)))
    ensureMode(resets, modes)

    addResets(resets, modes)
  }

  def convertAdvancedPreCond() = {
    val contIfPruneSet = body.contOnly.pruned.p.prune
    modes = extractModes(body.contOnly.withPruneSet(contIfPruneSet))
    val resetsSpecial = extractResets(body.discrOnly.withPruneSet(contIfPruneSet), clearExtracted = false)
    val resets = extractResets(body.discrOnly)
    sanity(body)

    modes.prepend(Mode("D0", preConds = initPostCond(init.toList)))

    enhanceModePreCond(resetsSpecial, modes)
    ensureMode(resets, modes)

    addResets(resets, modes)
  }

  // The minimal amout of clean up to be able eliminate unneeded state
  // variables and hense allow the model to run with the enclosure
  // interpreter
  def cleanUpSimple() = {
    pruneResetConds(modes)
    killDeadVars(init, modes)
  }

  // Full clean up pass, get ride of as much crap as possible
  def cleanUp() = {
    pruneResetConds(modes)
    killDeadResets(modes)
    cleanUpAssigns(modes)
    killDeadVars(init, modes)
  }

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
                           Init(MODE,ExprRhs(Lit(GStr(initMode)))) :: init.toList.map{case (v,e) =>  Init(v,ExprRhs(e))}, 
                           List(theSwitch) ++ simulatorAssigns.map{Discretely(_)})

    new Prog(List(newMain))
  }

  var pif = "handle";
  var convertLevel = 4;
  var cleanupLevel = 3;

  def run() = {
    pif match {
      case "ignore" =>
      case "reject" => rejectParallelIfs(body)
      case "handle" => handleParallelIfs(body)
      case _ => throw Errors.ShouldNeverHappen()
    }
    convertLevel match {
      case 1 => convertSimple()
      case 2 => convertMinimalModes()
      case 3 => convertSimplePreCond()
      case 4 => convertAdvancedPreCond()
      case _ => throw Errors.ShouldNeverHappen()
    }
    cleanupLevel match {
      case 0 => 
      case 1 => cleanUpSimple()
      case 2 => cleanUp()
      case 3 => cleanUp()
                cleanUpInitMode()
                eliminateTrueOnlyModes(modes, initMode)
      case _ => throw Errors.ShouldNeverHappen()
    }
    toAST
  }

  val res = run() // FIXME: Eventually remove

  //
  // Additional utility functions
  //

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
}

