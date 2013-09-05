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
  var modes = new ListBuffer[Mode]

  def convertSimple() = {
    // Now extract the mode and remove continuous assignments from the tree
    var idx = 1 

    def extractModes(parentContActions: List[ContinuousAction], body: IfTree.ContView) : Unit = {
      val actions = parentContActions ++ body.actions
      body.actions.clear()
      if (body.children.nonEmpty) {
        body.children.foreach{extractModes(actions, _)}
      } else {
        val label = "C" + idx
        modes += Mode(label, Nil, actions)
        body.node.discrAssigns += Assign(MODE_VAR, Lit(GStr(label)))
        idx += 1
      }
    }
    extractModes(Nil, body.contOnly.pruned)

    // Now extract resets and remove the discrete assignments from the if tree
    var resets = new ListBuffer[Reset]
    def extractResets(parentActions: List[Assign], body: IfTree.DiscrView) : Unit = {
      val actions = parentActions ++ body.actions
      body.actions.clear()
      if (body.children.nonEmpty) {
        body.children.foreach{extractResets(actions, _)}
      } else {
        resets += Reset(body.conds, ListBuffer(actions:_*))
      }
    }
    extractResets(Nil, body.discrOnly)

    // Check that no actions are left in the if tree.  If there is
    // something left it means we have syntax we can not support
    body.foreach{node => 
      if (node.actions.nonEmpty) throw UnhandledSyntax(node.actions.head)
    }

    // The next three steps perform the ADD RESETS AND SPECIAL MODES
    // step

    // Add the specal DO mode
    modes.prepend(Mode("D0", Nil, Nil))

    // If we are not already in a mode after a reset go into the special
    // D0 mode
    resets.foreach { dIf => 
      if (!dIf.actions.exists(assign => getName(assign.lhs).orNull == MODE)) {
        dIf.actions += Assign(MODE_VAR, Lit(GStr("D0")))
      }
    }

    // For every mode indiscriminately add all the discrIfs as possible
    // events
    modes.foreach { m =>
      m.resets = resets.toList
    }
  }
  
  def toAST = {
    // Converts the modes into a big switch and create a new mode
    // The CONVERT TO SWITCH and FIX UP steps
    val theSwitch = Switch(MODE_VAR,
                           modes.map{m => Clause(GStr(m.label),
                                                 Cond.toExpr(m.claims),
                                                 m.resets.map(_.toAST) ++
                                                 m.actions.map(Continuously(_)).toList)}.toList)
    val newMain = ClassDef(origDef.name,
                           origDef.fields,
                           init :+ Init(MODE,ExprRhs(Lit(GStr("D0")))), 
                           List(theSwitch) ++ simulatorAssigns.map{Discretely(_)})

    new Prog(List(newMain))
  }

  def run() = {
    rejectParallelIfs(body)
    convertSimple()
    toAST
  }

  val res = run() // FIXME: Eventually remove
}

