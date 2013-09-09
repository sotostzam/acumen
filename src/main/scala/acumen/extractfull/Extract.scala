// Extract a Hybrid automaton

// Mostly follows algo-orig.txt
//  The SPLIT, TRANSFORM and FLATTEN are all done at once during the
//  extraction into a normal form in which all other transformation
//  are done.

package acumen
package extractfull

import scala.collection.mutable.{ ListMap => MutListMap, ArrayBuffer }
import scala.util.control.Breaks.{ break, breakable }

import Pretty._

/***************************************************************************
 * The algorithm
 ***************************************************************************/

class Extract(prog: Prog, 
              val allowSeqIfs: Boolean = false,
              val unsafe: Boolean = true) extends MainClass(prog)
{
  // Additional Paramaters (mainly useful for debugging)
  val forceGuards = false 

  // Constant
  val MODE = Name("$mode", 0)
  val MODE_VAR = Dot(Var(Name("self", 0)), MODE)
  val MODE0 = Name("$mode0", 0)

  ////
  //// CHECK, SPLIT, TRANSFORM & FLATTEN (and part of ADD MODE VAR)
  ////

  // Builds the initial data structures
  extract(allowSeqIfs)

  // Removes simulator assigns at the top level.  Basically the CHECK
  //  step.  Needs to be done before uniquify is called.  (note that
  //  foreach is applied to an Option not a collection; (the
  //  overloading of names in Scala like this is rather unfortunate
  //  and confusing on by view -kevina))
  discrIfs.data.get(Nil).foreach(extractSimulatorAssigns(_))

  // Finish the building of the data structure.  Parts of the
  // TRANSFORM step
  contIfs.uniquify // FIXME: Better name, maybe pushIntoLeaves
  discrIfs.uniquify

  ////
  //// Note: At this point the first 4 steps are now done
  ////

  // Used later
  val contLHSDeps = contIfs.data.values.flatMap(_.actions.flatMap(Cond.extractLHSDeps(_))).toList.distinct
  val contRHSDeps = contIfs.data.values.flatMap(_.actions.flatMap(Cond.extractRHSDeps(_))).toList.distinct
  val contDeps = (contLHSDeps ++ contRHSDeps).distinct

  // Copy part of cont. if conds into "claim", remove that parts that
  // only involve discr. stuff (ie no dependencies contained in
  // "contDeps").  Done now before the conds are modified by the algo.
  contIfs.data.values.foreach{if0 =>
    if0.claims = if0.claims ++ if0.conds.filter(Cond.getDeps(_).intersect(contDeps).nonEmpty).toList
  }

  def getDeps(if0: DiscrIf, f: Assign => Expr) =
    if0.actions.flatMap(a => Cond.extractDeps(f(a))).distinct

  ////
  //// FOLD DISCR STATE
  ////
  //// (An optimization)

  def foldDiscrIf =
    discrIfs.data.values.foreach { if0 =>
      breakable { while (true) {
        // Determine possible candidates to match
        val matches = discrIfs.data.values.filter(_.matchConds(if0.postConds()) == CondTrue).toList
        // We can only fold if there is a single ??? that matches
        if (matches.size != 1) break
        val if1 = matches(0)
        // It is possible the ??? is ourselfs, in that case don't fold
        // as we will get into an infinite loop
        if (if0 == if1) break
        // Determine if its safe to merge the two sets of actions.
        // FIXME: Give some intuition on why these tests are needed.
        // (I think it has something to so with the new semantics)
        if (getDeps(if0, _.rhs).intersect(getDeps(if1, _.lhs)).nonEmpty) break
        if (getDeps(if0, _.lhs).intersect(getDeps(if1, _.rhs)).nonEmpty) break
        // Now fold them
        matches(0).actions.foreach { action =>
          val toFind = Cond.getName(action.lhs)
          val idx = if (toFind == None) -1 else if0.actions.indexWhere(a => Cond.getName(a.lhs) == toFind)
          if (idx != -1)
            if0.actions(idx) = action
          else
            if0.actions += action
        }
      }}
    }
  foldDiscrIf

  ////
  //// DETERMINE CONT STATE
  //// 
  //// (An optimization, we could just force guards...)

  def matchUp = // fixme: better name
    discrIfs.data.values.foreach { if0 =>
      val postConds = if0.postConds()
      if0.matches = contIfs.data.values.filter(_.matchConds(postConds) == CondTrue).toList
      if0.maybe = contIfs.data.values.filter { if1 =>
        if1.matchConds(postConds) match {
          case CantTell(_) =>
            if1.needGuard = true; true
          case _ => false
        }
      }.toList
    }
  matchUp

  if (forceGuards)
    contIfs.data.values.foreach(_.needGuard = true)

  var needGuards = contIfs.data.values.filter { _.needGuard }.toList

  ////
  //// KILL CONT GUARDS
  ////
  //// (An unsafe optimization)

  // WARNING: This makes assumtions that are only true 99.9% of the time
  def killGuards =
    discrIfs.data.values.foreach { if0 =>
      if0.maybe.foreach { if1 =>
        if1.matchConds(if0.postConds()) match {
          case CantTell(unmatched) =>
            if1.origConds = if1.conds
            if1.conds = if1.conds.diff(unmatched)
          case _ =>
          /* already cleaned up */
        }
      }
    }
  if (unsafe && needGuards.nonEmpty) {
    killGuards
    contIfs.data.values.foreach { if0 => if0.needGuard = false }
    matchUp
    if (discrIfs.data.values.exists(_.matches.size > 1)) {
      // we screwed up and removed too much, undo
      //println("******** UNDO ************")
      contIfs.data.values.foreach { if0 =>
        if0.needGuard = false
        if0.conds = if0.origConds
      }
      matchUp
    }
    needGuards = contIfs.data.values.filter { _.needGuard }.toList
  }

  ////
  //// Do something about the guards we can't kill
  ////

  if (needGuards.nonEmpty) {
    discrIfs.data ++= discrIfs.emptyIfs.map { if0 => (if0.conds, if0) }
    contIfs.data.values.filter { _.needGuard }.foreach { if0 =>
      //println("Need guards on " + if0.label)
      val modeVal = GStr(if0.label)
      discrIfs.addwEmpties(if0.conds).actions += Assign(Var(MODE0), Lit(modeVal))
      if0.conds = if0.conds :+ Cond.Eq(MODE0, modeVal)
    }
    contIfs.reset
    discrIfs.reset
    discrIfs.uniquify
    foldDiscrIf
    matchUp
  }

  ////
  //// ADD MODE VAR (Rest of)
  //// ADD RESETS AND SPECIAL MODES
  ////

  // Determe reachable discrete states from the continuous states and
  // mark them
  contIfs.data.values.foreach { if0 =>
    val preCond = if0.discrConds(contLHSDeps)
    discrIfs.data.values.foreach { if1 =>
      if (if1.matchConds(preCond) != CondFalse)
        if1.reachable = true
    }
  }
  // Dermine what discr. states are reachable from other marked discr. state
  def calcReachableFixedPoint : Unit = {
    var somethingChanged = false
    discrIfs.data.values.filter(_.reachable).foreach { if0 =>
      val postConds = if0.postConds()
      discrIfs.data.values.foreach { if1 =>
        if (if1.matchConds(postConds) != CondFalse && !if1.reachable) {
          if1.reachable = true
          somethingChanged = true
        }
      }
    }
    if (somethingChanged) calcReachableFixedPoint
  }

  // Now see the initial state can be determined and if possibe try to
  // eliminate the need for it
  def getInitConds = init.flatMap {
    _ match {
      case Init(name, ExprRhs(Lit(value))) => List(Cond.Eq(name, value))
      case _ => Nil
    }
  }
  var initConds = getInitConds
  def followInitialState: Boolean = { // return true if the initial
                                      // state was eliminated
    val matches = discrIfs.data.values.filter(_.matchConds(initConds) == CondTrue).toList
    if (matches.size != 1) return false
    def initState = matches(0)
    extractSimulatorAssigns(initState)
    // If the initial may be reachable from a continuous state than
    // there is no point in eliminating it
    if (initState.reachable) return false
    if (getDeps(initState, _.rhs).nonEmpty) return false
    val initInputDeps = init.flatMap {
      _ match {
        case Init(name, ExprRhs(expr)) => Cond.extractDeps(expr)
        case _ => Nil
      }
    }.distinct
    if (initInputDeps.intersect(getDeps(initState, _.lhs)).nonEmpty) return false
    init = init.map {
      _ match {
        case a0 @ Init(name, _) =>
          initState.actions.find { case Assign(name0, _) => name == Cond.getName(name0).orNull } match {
            case Some(a1) => Init(name, ExprRhs(a1.rhs))
            case _ => a0
          }
      }
    }
    discrIfs.data.remove(initState.conds)
    initConds = getInitConds
    return true
  }
  var initStateZapped = followInitialState
  val initialModes = 
    contIfs.data.values.filter(_.matchConds(initConds) == CondTrue)
  var possibleInitialStates : List[DiscrIf] = Nil
  var initMode = "ERROR"
  if (initialModes.size == 1) {
    initMode = initialModes.head.label
  } else {
    possibleInitialStates = 
      discrIfs.data.values.filter(_.matchConds(initConds) != CondFalse).toList
    if (possibleInitialStates.nonEmpty)
      initMode = "Init"
  }
  //println("INIT MODE: " + initMode)
  init = init :+ Init(MODE,ExprRhs(Lit(GStr(initMode))))

  // Mark the possible initial stats as reachable and any from those
  if (possibleInitialStates.nonEmpty) {
    possibleInitialStates.foreach(_.reachable = true)
    calcReachableFixedPoint
  }
  discrIfs.data.retain((_,if0) => if0.reachable)


  // fixme: make sure there are no assigments to simulator parameters
  // left (any ones in valid location should have already been
  // extracted)
  
  // Determine continous mode or go into D0 mode if, if it can't be
  // determined, collect resets that need to be in D0 mode.
  // The D0 mode is a catch all mode
  var d0ModeNeeded = false
  discrIfs.data.values.foreach{if0 =>
    if (if0.matches.size == 1) {
      if0.actions += Assign(MODE_VAR, Lit(GStr(if0.matches.head.label)))
    } else {
      d0ModeNeeded = true
      if0.actions += Assign(MODE_VAR, Lit(GStr("D0")))
      discrIfs.data.values.filter(_.matchConds(if0.conds) != CondFalse).foreach(_.d0Mode = true)
    }
  }

  def calcD0ModeFixedPoint : Unit = {
    var somethingChanged = false
    discrIfs.data.values.filter(_.d0Mode).foreach { if0 =>
      val postConds = if0.postConds()
      discrIfs.data.values.foreach { if1 =>
        if (if1.matchConds(postConds) != CondFalse && !if1.d0Mode) {
          if1.d0Mode = true
          somethingChanged = true
        }
      }
    }
    if (somethingChanged) calcD0ModeFixedPoint
  }
  if (d0ModeNeeded) calcD0ModeFixedPoint

  def getResets(preCond: Seq[Cond]) = 
    discrIfs.data.values.flatMap { if1 =>
      if1.matchConds(preCond) match {
        case CondTrue => List(if1.dup(Nil))
        case CantTell(unmatched) => List(if1.dup(unmatched))
        case CondFalse => Nil
      }
    }.toList

  contIfs.data.values.foreach { if0 =>
    val preCond = if0.discrConds(contLHSDeps)
    if0.resets = getResets(preCond)
  }

  var modes = contIfs.data.values.toList

  if (d0ModeNeeded) {
    val resets = discrIfs.data.values.filter(_.d0Mode).toList
    val commonConds = resets.map(_.conds).reduce{(a, b) => a.intersect(b)}
    val d0Mode = new ContIf(commonConds, "D0")
    d0Mode.resets = resets.map{if0 => if0.dup(if0.conds.diff(commonConds))}
    modes = d0Mode +: modes
  }

  if (possibleInitialStates.nonEmpty) {
    val initMode = new ContIf(initConds, "Init")
    initMode.resets = getResets(initConds)
    modes = initMode +: modes
  }

  modes.foreach{if0 =>
    if0.conds = Cond.Eq(MODE, GStr(if0.label)) +: if0.conds
  }

  ////
  //// CLEAN UP
  ////
  //// (An Optimization) 

  // for each mode, kill all unnecessary discr. assignemnts in
  // the resets, if the reset than has no actions, kill it.
  modes.foreach{if0 => 
    if0.resets.foreach{if1=>
      val preConds = if0.conds ++ if1.conds
      // keep the mode assignment operation even if the mode does not
      // change as it is needed by the enclosure interpreter
      var modeAssign : Assign = null
      if1.actions = if1.actions.filter{case a@Assign(lhs,rhs) => !((Cond.getName(lhs), rhs) match {
        case (Some(name), Lit(value)) => 
          val res = preConds.exists(_ == Cond.Eq(name,value))
          if (res && name == MODE) modeAssign = a
          res
        case _ => false
      })}
      if (if1.actions.nonEmpty && modeAssign != null) 
        if1.actions += modeAssign
    }
    if0.resets = if0.resets.filter(_.actions.nonEmpty)
  }

   // 
  modes.foreach(_.conds = Nil)

  val discrDeps = modes.flatMap(_.resets.flatMap{if0 =>
    if0.conds.flatMap(Cond.getDeps(_)) ++ if0.actions.flatMap{case Assign(_,rhs) => Cond.extractDeps(rhs)}
  }).distinct
  val allDeps = (contDeps ++ discrDeps).distinct :+ MODE // $mode is special and needs to be kept
  val allVars = init.map{case Init(n,_) => n} :+ MODE0 // $mode0 should be killed if not there
                                                                   // is a likely problem
  val kill = allVars.filter{case Name(x,_) => !allDeps.exists{case Name(y,_) => x == y}}
  //println("KILL: " + kill)
  
  modes.foreach{if0 =>
    if0.resets.foreach{if1 => 
      if1.actions = if1.actions.filter{case Assign(rhs,_) => Cond.getName(rhs) match {
        case Some(name) => !kill.exists(_ == name)
        case _ => true
      }}
    }
    if0.resets = if0.resets.filter(_.actions.nonEmpty)
  }
  init = init.filter{case Init(n,_) => !kill.exists(_ == n)}

  //// 
  //// CONVERT TO SWITCH and FIXUP
  //// i.e. putting it all together to get a new AST
  ////

  val theSwitch = Switch(MODE_VAR,
                         modes.map{if0 => Clause(GStr(if0.label),
                                                 Cond.toExpr(if0.claims),
                                                 if0.resets.map(_.toAST) ++
                                                 if0.actions.map(Continuously(_)).toList)})
  val newMain = ClassDef(origDef.name,
                         origDef.fields,
                         init, List(theSwitch) ++ simulatorAssigns.map(Discretely(_)))

  val res = new Prog(List(newMain))
} 
