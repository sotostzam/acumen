// Extract a Hybrid automaton

// Mostly follows algo.txt; however, the data structures used here are
// slightly different and the order the steps is not exactly the same.

package acumen
package extract

import scala.collection.mutable.{ArrayBuffer,ListBuffer,Map=>MutMap}

import Util.getName

object Extract {
  // Constant
  val MODE = Name("$mode", 0)
  val MODE_VAR = Dot(Var(Name("self", 0)), MODE)

  case class Mode(val label: String, 
                  var claims: List[Cond], 
                  var actions: List[ContinuousAction], 
                  var resets: List[Reset] = Nil)
  
  case class Reset(var conds: List[Cond],
                   var actions: ListBuffer[Assign] = ListBuffer.empty) 
  {
    def toAST: IfThenElse =
      IfThenElse(Cond.toExpr(conds),
                 actions.map(Discretely(_)).toList, Nil)
  }
  def rejectParallelIfs(body: IfTree[_], msg: String = "Parallel conditionals unsupported.") {
    if (body.children.map{_.megId}.distinct.length > 1)
      throw OtherUnsupported(msg)
    body.children.foreach{rejectParallelIfs(_)}
  }
  // Extract the mode and remove continuous assignments from the tree
  // In also adds the necessary "magic" so that we are guaranteed to
  // always know what mode to go into after a reset
  def extractModes(root: IfTree.ContView) : List[Mode] = {
    val modes = new ListBuffer[Mode]
    var idx = 1 
    def doit(parentActions: List[ContinuousAction], body: IfTree.ContView) : Unit = {
      val actions = parentActions ++ body.actions
      body.actions.clear()
      if (body.children.nonEmpty) {
        body.children.foreach{doit(actions, _)}
      } else {
        val label = "C" + idx
        modes += Mode(label, Nil, actions)
        body.node.discrAssigns += Assign(MODE_VAR, Lit(GStr(label)))
        idx += 1
      }
    }
    doit(Nil, root)
    modes.toList
  }
  // Extract resets and remove the discrete assignments from the if tree
  def extractResets(root: IfTree.DiscrView) : List[Reset] = {
    val resets = new ListBuffer[Reset]
    def doit(parentActions: List[Assign], body: IfTree.DiscrView) : Unit = {
      val actions = parentActions ++ body.actions
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
  // D0 mode
  def ensureMode(resets: Seq[Reset]) : Unit =
    resets.foreach { dIf => 
      if (!dIf.actions.exists(assign => getName(assign.lhs).orNull == MODE)) {
        dIf.actions += Assign(MODE_VAR, Lit(GStr("D0")))
      }
    }
  // For every mode indiscriminately add all possible resets
  def addResetsSimple(resets: Seq[Reset], modes: Seq[Mode]) {
    modes.foreach { m =>
      m.resets = resets.toList
    }
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
  var init = origDef.priv
  val body : IfTree.Node = IfTree.create(origDef.body)
  val simulatorName = origDef.fields(0)
  var simulatorAssigns = body.extractSimulatorAssigns(simulatorName)
  var modes : List[Mode] = Nil
  var initMode = Init(MODE,ExprRhs(Lit(GStr("D0"))))

  // Convert from an IfTree to a set of Modes with Resets by
  // populating "modes".  When done IfTree (i.e. body) will no longer
  // have any actions.
  def convertSimple(superSimple: Boolean = false) = {
    modes = extractModes(if (superSimple) body.contOnly else body.contOnly.pruned)
    val resets = extractResets(body.discrOnly)
    sanity(body)

    modes = Mode("D0", Nil, Nil) +: modes
    if (!superSimple) ensureMode(resets)

    addResetsSimple(resets, modes)
  }
  
  def toAST = {
    // Converts the modes into a big switch and return a new prog
    // The CONVERT TO SWITCH and FIX UP steps
    val theSwitch = Switch(MODE_VAR,
                           modes.map{m => Clause(GStr(m.label),
                                                 Cond.toExpr(m.claims),
                                                 m.resets.map(_.toAST) ++
                                                 m.actions.map(Continuously(_)).toList)}.toList)
    val newMain = ClassDef(origDef.name,
                           origDef.fields,
                           init :+ initMode, 
                           List(theSwitch) ++ simulatorAssigns.map{Discretely(_)})

    new Prog(List(newMain))
  }

  def run() = {
    rejectParallelIfs(body)
    convertSimple(true)
    toAST
  }

  val res = run() // FIXME: Eventually remove
}

