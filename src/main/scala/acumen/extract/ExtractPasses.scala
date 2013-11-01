package acumen
package extract

import scala.collection.immutable.{Set}
import scala.collection.mutable.{ArrayBuffer,ListBuffer,
                                 Map=>MutMap,Set=>MutSet,MultiMap=>MutMultiMap,
                                 HashSet,HashMap}
import scala.text.Document.nest

import Util._
import CondImplicits._
import Pretty._

object ExtractPasses {
  import Extract._

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
  // Extract resets and (optionally) remove the discrete assignments
  // from the if tree
  // Check that no actions are left in the if tree.  If there is
  // something left it means we have syntax we can not support
  def extractAll(root: IfTree.Node) : (ListBuffer[Mode],ListBuffer[Reset]) = {
    val modes = new ListBuffer[Mode]
    val resets = new ListBuffer[Reset]
    var idx = 1 
    def doit(parentContActions: List[ContinuousAction], 
             parentDiscrAssigns: List[Assign], 
             body: IfTree.Node) : Unit = {
      val contActions  = parentContActions  ++ body.contActions
      val discrAssigns = parentDiscrAssigns ++ body.discrAssigns
      if (body.otherActions.nonEmpty) throw UnhandledSyntax(body.otherActions.head)
      if (body.children.nonEmpty) {
        body.children.foreach{doit(contActions, discrAssigns, _)}
      } else {
        val label = "C" + idx
        modes += Mode(label, claims = body.claims, actions = contActions)
        resets += Reset(body.conds, ListBuffer(Assign(MODE_VAR, Lit(GStr(label))) :: discrAssigns :_*))
        idx += 1
      }
    }
    doit(Nil, Nil, root)
    (modes, resets)
  }

  def enhanceModePreCond(resets: Seq[Reset], modes: ListBuffer[Mode], modeVars: Set[Name]) {
    assert({val modes = resets.map{_.mode}; !modes.contains(None) && modes.distinct.size == modes.size})
    resets.foreach{r =>
      val postConds = Util.postConds(r.conds, r.actions)
      val modePreConds = discrConds(postConds,modeVars)
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
  // Then remove the reset if its guard is false.
  def pruneResetConds(modes: Seq[Mode]) : Unit = {
    modes.foreach{m => 
      m.resets.foreach{r => r.conds = r.conds.eval(m.preConds)}
      m.resets = m.resets.filter{_.conds != Cond.False}
    }
  }

  def cleanUpAssigns(modes: Seq[Mode]) : Unit = {
  // for each mode, kill all unnecessary discr. assignemnts in the
  // resets, if the reset than has no actions other than to go back
  // into the same mode, kill it.
    modes.foreach{m => 
      // FIXME: Broken, needs to take into account mode variable when reenabled
      //   also not sure if its really needed
      //m.resets.foreach{r=>
      //  val preConds = Cond.and(m.preConds,r.conds)
      //  r.actions = r.actions.filter{case a@Assign(lhs,rhs) => !((getName(lhs), rhs) match {
      //    case (Some(name), Lit(value)) => 
      //      preConds.exists(_ == Cond.MemberOf(name,Set(value)))
      //    case _ => false
      //  })}
      //}
      m.resets = m.resets.filter{r => 
        !(r.actions.length == 1 && r.mode == Some(m.label))
      }
    }
  }

  def killDeadVars(allVars: Iterable[Name], modes: Seq[Mode]) = {
    val kill = getDeadVars(allVars, modes)

    modes.foreach{m =>
      m.resets.foreach{r => 
        r.actions = r.actions.filter{case Assign(rhs,_) => getName(rhs) match {
          case Some(name) => !kill.exists(_ == name)
          case _ => true
        }}
      }
      m.resets = m.resets.filter(_.actions.nonEmpty)
    }

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
    // Untested propriety, if the resets were reapplied after merging
    // the resets after filing will be identical, even if some
    // preconds were eliminated during the merge
    val dups = modes.groupBy{m => (m.claims, m.resets, m.actions, m.trans)}.filter{case (_, v) => v.length > 1}
    dups.values.foreach{ms =>
      val target :: toKill = ms.sortWith{(a,b) => a.label < b.label}.toList
      target.preConds = mergePreConds(target.preConds :: toKill.map{_.preConds})
      toKill.foreach{_.markDead()}
    }
  }

  def markTransModes(modes: Seq[Mode]) {
    modes.filter{!_.trans}.foreach{m =>
      val rs = m.resets.filter{r => r.mode.orNull == m.label}
      assert(rs.size <= 1)
      if (rs.isEmpty) {
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
    // Untested properity: see mergeDupModes
    modes.filter{_.trans}.foreach{m =>
      var candidates = modes.filter{m2 => !m2.trans && m.resets == m2.resets}
      // Possible fixme: Is this check really necessary, does it
      // really mater what mode we merge into if there are multiple
      // candidates, can't we just chose one randomly
      if (candidates.size != 1) {
        candidates = modes.filter{m2 => m.resets == m2.resets}
        if (!candidates.isEmpty && candidates.head.label != m.label)
          candidates = List(candidates.head)
        else
          candidates = Nil
      }
      if (candidates.size == 1) {
        val target = candidates.head
        target.preConds = mergePreConds(m.preConds :: target.preConds :: Nil)
        m.markDead()
      } 
    }
  }
  def placeHolderReset(label: String) = 
      List(Reset(Cond.True, ListBuffer(Assign(MODE_VAR, Lit(GStr(label))))))

  // FIXME: Is this wrapper still needed, is it doing the correct thing?
  // Attemt to eliminate modes with only a single reset with a true guard
  def eliminateTrueOnlyModes(modes: Seq[Mode]) = {
    modes.filter{m => m.label != "Init" && m.trans && m.resets.length == 1 && m.resets.head.conds == Cond.True}.foreach{m =>
      val actions = m.resets.head.actions
      val rs = getResetsWithMode(m.label, modes)
      val canFold = rs.forall{r => canFoldActions(r.actions, actions)}
      if (canFold) {
        rs.foreach{r => foldActions(r.actions, actions)}
        m.markDead
      }
    }
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

  def resolveModes(modes: ListBuffer[Mode]) : Unit = {
    modes.foreach{m => m.resets.foreach {r =>
      val postConds = Util.postConds(Cond.and(m.preConds,r.conds), r.actions)
      val candidates = modes.map{m => (m, m.preConds.eval(postConds))}.filter{case (_,res) => res != Cond.False}
      if (candidates.length == 1) {
        val (target, res) = candidates.head
        assert(res == Cond.True)
        r.mode = Some(target.label)
      } else { 
        // none of 
        /* FIXME: Split! */
      }
    }}
  }


  //
  // Additional utility
  // 

  def getModeVars(root: IfTree.Node) : Map[Name,Set[GroundValue]] = { 
    // A mode var is a variable that is only assigned to literal
    // values in discrete assignments (and consequently never assigned
    // to continously) and one that is only tested for equality.
    // Possible values for a mode variable are simply the values that
    // its gets assigned.
    val res = new HashMap[Name, MutSet[GroundValue]] with MutMultiMap[Name, GroundValue]
    val kill = new HashSet[Name]
    root.foreach{_.node.discrAssigns.foreach{
      case Assign(Dot(Var(Name("self",0)), f), e) => e match {
        case Lit(v) => res.addBinding(f, v)
        case _      => kill += f
      }
      case _ => /* do nothing, something other than local assignment */
    }}
    root.foreach{_.node.contActions.foreach{a => 
      kill ++= extractLHSDeps(a)
    }}
    def traverse(cond: Cond) : Unit = cond match {
      case Cond.True | Cond.False => /* no deps */
      case Cond.MemberOf(_,_) => /* do nothing */
      case Cond.And(conds) => conds.foreach{traverse(_)}
      case Cond.Not(cond) => traverse(cond)
      case Cond.Other(_, deps) => kill ++= cond.deps
    }
    root.foreach{n => traverse(n.localConds)}
    res --= kill
    Map(res.toSeq.map{case (k, v) => (k, Set(v.toSeq : _*))} :+ (MODE,Set.empty[GroundValue]) : _*)
  }

  def mergePreConds(preConds: List[Cond]) : Cond = {
    if (preConds.isEmpty) Cond.True
    else if (preConds.length == 1) preConds.head
    else {
      val first = preConds.head
      val second = preConds.tail.head
      val rest = preConds.tail.tail
      val combinedModes = getModesPC(first) ++ getModesPC(second)
      val combined = Cond.and(Cond.MemberOf(MODE, combinedModes), first.toSet intersect second.toSet)
      mergePreConds(combined :: rest)
    }
  }
  def getModesPC(preConds: Cond) : Set[GroundValue] = {
    val res = preConds.collect{case Cond.MemberOf(MODE, vals) => vals}
    assert(res.size == 1)
    res.head
  }

}
