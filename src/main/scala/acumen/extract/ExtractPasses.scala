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
  def extractAll(root: IfTree.Node, modeVars: Set[Name]) : (ListBuffer[Mode],ListBuffer[Reset]) = {
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
        idx += 1

        try {
          val assigns = Assign(MODE_VAR, Lit(GStr(label))) :: discrAssigns

          val conds = body.conds
          val postConds = Util.postConds(conds, assigns)
          val modePreConds = discrConds(postConds,modeVars)
          
          modes += Mode(label, claims = body.claims, actions = contActions, preConds = modePreConds)
          resets += Reset(body.conds, ListBuffer(assigns :_*))
        } catch {
          case DuplicateAssignments =>
            resets += Reset(body.conds, 
                            ListBuffer(Assign(MODE_VAR, Lit(GStr("ERROR")))))
        }
      }
    }
    doit(Nil, Nil, root)
    (modes, resets)
  }

  // splitModes = if user-mode variables can't be eliminated, split
  // the mode by adding all possible values of user-mode variables 

  // resolve if a given mode can't be resolved split that mode as
  // nessary and repeat the split/merge step basically when there are
  // multiple candidates eval each caniate precond, if it doesn't
  // become True or False the leftover predicates are a basis for
  // additional precond for split
  def splitModes(modes: ListBuffer[Mode], modeVars: Map[Name,Set[GroundValue]]) {
    val newModes = new ListBuffer[Mode];
    modes.foreach{m =>
      val leftover = m.resets.flatMap{r => r.conds.deps.filter{d => modeVars.contains(d)}}.toSet
      val extraConds = leftover.map{d => modeVars(d).map{v => Cond.eq(d, v)}}
      val theList = cross(extraConds)
      splitMode(theList, m, newModes)
    }
    pruneResetConds(newModes)
    modes ++= newModes
    pruneDeadModes(modes)
  }

  def cross(todo: Set[Set[Cond]], done: Cond = Cond.True) : List[Cond] = {
    if (todo.isEmpty) {if (done == Cond.True) Nil
                       else List(done)}
    else todo.head.toList.flatMap{cond => cross(todo.tail, Cond.and(done, cond))}
  }

  def splitMode(addConds: Seq[Cond], mode: Mode, res: ListBuffer[Mode]) : Unit = {
    //println("split mode: " + mode.label + " " + addConds)
    if (addConds.isEmpty) return
    var keep = false
    var idx = 1;
    val (memberOf,rest) = mode.preConds.partition{case Cond.MemberOf(MODE,_) => true; case _ => false}
    assert(memberOf.size == 1)
    val Cond.MemberOf(_,vls) = memberOf.head
    addConds.foreach{cond =>
      val newPreCond = Cond.and(rest,cond)
      if (newPreCond != rest) {
        val newLabel = mode.label + "." + idx
        idx += 1
        res += mode.copy(label = newLabel, preConds = Cond.and(Cond.MemberOf(MODE, vls + GStr(newLabel)), newPreCond), resets = mode.resets.map{_.copy()})
      } else {
        keep = true;
      }
    }
    val newLabel = mode.label + ".0"
    if (keep) res += mode.copy(label = newLabel, preConds = Cond.and(Cond.MemberOf(MODE, vls + GStr(newLabel)), rest), resets = mode.resets.map{_.copy()})
    mode.markDead()
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

  // precond: no dead modes
  def mergeDupModes(modes: ListBuffer[Mode]) {
    // Untested propriety, if the resets were reapplied after merging
    // the resets after filing will be identical, even if some
    // preconds were eliminated during the merge
    val dups = modes.groupBy{m => (m.claims, m.resets, m.actions, m.trans)}.filter{case (_, v) => v.length > 1}
    dups.values.foreach{ms =>
      //println("MERGING: " + ms.map{_.label})
      val target :: toKill = ms.sortWith{(a,b) => a.label < b.label}.toList
      target.preConds = mergePreConds(target.preConds :: toKill.map{_.preConds})
      toKill.foreach{_.markDead()}
    }
    pruneDeadModes(modes)
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
  def cleanUpTransModes(modes: ListBuffer[Mode]) {
    // Untested properity: see mergeDupModes
    modes.filter{_.trans}.foreach{m =>
      var candidates = modes.filter{m2 => !m2.trans && m.resets == m2.resets}
      if (candidates.size > 1) {
        // See if there is a mode with no (cont) actions, its
        // slightly more correct to merge into this mode than one
        // with actions, although it really doesn't affect the result
        val filtered = candidates.filter{m2 => m2.actions.isEmpty}
        if (filtered.nonEmpty)
          candidates = filtered
      }
      // If there is more than one merging into any one of them is correct
      // so "merge" into them all and sort it out later in resolveModes()
      if (candidates.nonEmpty) {
        candidates.foreach{target => 
          target.preConds = mergePreConds(m.preConds :: target.preConds :: Nil)
        }
        m.markDead()
      }
    }
    pruneDeadModes(modes)
  }
  def placeHolderReset(label: String) = 
    List(Reset(Cond.True, ListBuffer(Assign(MODE_VAR, Lit(GStr(label))))))

  // FIXME: Is this wrapper still needed, is it doing the correct thing?
  // Attemt to eliminate modes with only a single reset with a true guard
  def eliminateTrueOnlyModes(modes: ListBuffer[Mode]) = {
    modes.filter{m => m.label != "Init" && m.trans && m.resets.length == 1 && m.resets.head.conds == Cond.True}.foreach{m =>
      val actions = m.resets.head.actions
      val rs = getResetsWithMode(m.label, modes)
      val canFold = rs.forall{r => canFoldActions(r.actions, actions)}
      if (canFold) {
        rs.foreach{r => foldActions(r.actions, actions)}
        m.markDead()
      }
    }
    pruneDeadModes(modes)
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
    if (modes.isEmpty) return
    val toSplit = new HashMap[String,(Mode, HashSet[Cond])];
    val newModes = new ListBuffer[Mode];
    modes.foreach{m => m.resets.filter{_.mode != Some("ERROR")}.foreach {r =>
      try {
        val postConds = Util.postConds(Cond.and(m.preConds,r.conds), r.actions)
        val evaled = modes.map{m => (m, m.preConds.eval(postConds))}
        var candidates = evaled.filter{case (_,res) => res == Cond.True}
        if (candidates.length > 1) {
          // Sort the list so the "best" candidates is first.  In truth
          // it doesn't mater, but this way we at least get determinist
          // behavior
          import scala.math.Ordering.Implicits._
          def sortKey(m: Mode) = (m.preConds.toSeq.length, m.label)
          candidates = candidates.sortWith{(a,b) => sortKey(a._1) < sortKey(b._1)}
        }
        if (candidates.nonEmpty) {
          val (target, res) = candidates.head
          assert(res == Cond.True)
          r.mode = Some(target.label)
        } else {
          //println("can't resolve! " + m.label + " -- " + r.mode)
          val neededConds = evaled.collect{case (_,res) if res != Cond.False => res}
          toSplit.getOrElseUpdate(m.label, (m, new HashSet[Cond]))._2 ++= neededConds
        }
      } catch {
        case Util.DuplicateAssignments => r.mode = Some("ERROR " + r.mode.get)
      }
    }}
    toSplit.foreach{case (l,(m,neededConds)) =>         
      splitMode(neededConds.toSeq, m, newModes)
    }
    if (newModes.nonEmpty) {
      //println("resolve new modes: " + newModes.map{_.label})
      pruneResetConds(newModes)
      modes ++= newModes
      pruneDeadModes(modes)
      resolveModes(modes)
    }
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
    //println("res == " + res)
    assert(res.size == 1)
    res.head
  }

}
