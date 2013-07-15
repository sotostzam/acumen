// Extract a Hybrid automaton

// Mostly follows algo.txt in https://bitbucket.org/effective/paper-esaes
//  (commit e9a85f9779f26f8719d43d41599074b5725a2332)
//  The SPLIT, TRANSFORM and FLATTEN are all done at once during the
//  extraction into a normal form in which all other transformation
//  are done.

package acumen
import scala.collection.mutable.{ ListMap => MutListMap, ArrayBuffer }
import scala.util.control.Breaks.{ break, breakable }

sealed abstract class Cond { def toExpr: Expr }
case class Eq(name: Name, value: GroundValue) extends Cond {
  def toExpr = Op(Name("==", 0), List(Var(name), Lit(value)))
}
case class Not(cond: Cond) extends Cond {
  def toExpr = Op(Name("not", 0), List(cond.toExpr))
}
case class OtherCond(expr: Expr, deps: Seq[Name]) extends Cond {
  def toExpr = expr
}
object Cond {
  def getName(x: Expr): Option[Name] = x match {
    case Var(name) => Some(name)
    case Dot(Var(Name("self", 0)), name) => Some(name)
    case _ => None
  }
  def getDeps(cond: Cond): Seq[Name] = cond match {
    case null => Nil
    case Eq(name, _) => List(name)
    case Not(cond) => getDeps(cond)
    case OtherCond(_, deps) => deps
  }

  def apply(e: Expr): Cond = e match {
    case Op(Name("==", 0), List(x, Lit(value))) => eq(x, value)
    case Op(Name("not", 0), List(x)) => not(Cond(x))
    case _ => OtherCond(e, extractDeps(e))
  }
  def eq(x: Expr, y: Expr): Cond = (x, y) match {
    case (_, Lit(value)) => eq(x, value)
    case _ => OtherCond(Op(Name("==", 0), List(x, (y))), extractDeps(x) ++ extractDeps(y))
  }
  def eq(x: Expr, y: GroundValue): Cond = (getName(x), y) match {
    case (Some(name), _) => Eq(name, y)
    case _ => OtherCond(Op(Name("==", 0), List(x, Lit(y))), extractDeps(x))
  }
  def not(cond: Cond): Cond = cond match {
    case Not(cond) => cond
    case _ => Not(cond)
  }
  def not(x: Expr): Cond = not(Cond(x))
  def extractDeps(x: Expr): Seq[Name] =
    x match {
      case Op(_, lst) => lst.flatMap(extractDeps(_)).distinct
      case Var(name) => List(name)
      case Dot(Var(Name("self", 0)), name) => List(name)
      // fixme: Handle other cases with embeded expr
      case _ => Nil
    }
  def extractLHSDeps(a: ContinuousAction): Seq[Name] = a match {
    case Equation(lhs, rhs) => extractDeps(lhs)
    case EquationI(lhs, rhs) => extractDeps(lhs)
    case EquationT(lhs, rhs) => extractDeps(lhs)
  }
  def extractRHSDeps(a: ContinuousAction): Seq[Name] = a match {
    case Equation(lhs, rhs) => extractDeps(rhs)
    case EquationI(lhs, rhs) => extractDeps(rhs)
    case EquationT(lhs, rhs) => extractDeps(rhs)
  }
  def toExpr(conds: Seq[Cond]): Expr = 
    if (conds.nonEmpty) 
      conds.tail.foldLeft(conds.head.toExpr) { (a, b) => Op(Name("&&", 0), List(a, b.toExpr)) }
    else
      Lit(GBool(true))
}

sealed abstract class MatchRes
case object CondTrue extends MatchRes;
case object CondFalse extends MatchRes;
case class CantTell(unmatched: Seq[Cond]) extends MatchRes;

abstract class If[ActionT](var conds: Seq[Cond], val label: String) {
  def toAST: IfThenElse
  var actions = new ArrayBuffer[ActionT];
  def dump: String;
  def reset: Unit;
  def matchConds(have: Seq[Cond]): MatchRes = {
    // filter out predicates that exists in both
    val unmatched = conds.filter { a => !have.exists { b => a == b } }
    // if no unmatched predicates return true
    if (unmatched.isEmpty) return CondTrue
    // if A is in unmatched and ~A is known return false
    if (unmatched.exists { a => have.exists { b => Cond.not(a) == b } }) return CondFalse
    // if SYM == SOMETHING exists and SYM == ANYTHING is known return false
    // (the case when SYM == SOMETHING is known was already eliminated)
    if (unmatched.exists {
      _ match {
        case Eq(a, _) => have.exists {
          _ match {
            case Eq(b, _) => a == b;
            case _ => false
          }
        }
        case _ => false
      }
    }) return CondFalse
    // there are predicates that can't be shown to be either true or
    // false so return maybe
    return CantTell(unmatched)
  }
  var reachable = false
  var d0Mode = false;
}
abstract class MkIf[IfT] {
  def apply(conds: Seq[Cond]): IfT
}

class ContIf(conds0: Seq[Cond], label: String = ContIf.newLabel) extends If[ContinuousAction](conds0, label) {
  def toAST: IfThenElse =
    IfThenElse(Cond.toExpr(conds),
      resets.map(_.toAST) ++
        actions.map(Continuously(_)).toList, Nil)
  def dump = label + ": " + Pretty.pprint[Action](toAST) + "\n"
  var claims: List[Cond] = Nil; // i.e. "claims", currently unused
  var needGuard = false;
  var resets: List[DiscrIf] = Nil;
  var origConds: Seq[Cond] = null;
  def reset() { needGuard = false; resets = null; }
  def discrConds(deps: Seq[Name]): Seq[Cond] =
    conds.filter(Cond.getDeps(_).intersect(deps).isEmpty)
}
object ContIf extends MkIf[ContIf] {
  def apply(conds: Seq[Cond]) = new ContIf(conds)
  var counter = 0
  def newLabel = { counter += 1; "C" + counter; }
}

class DiscrIf(conds0: Seq[Cond]) extends If[Assign](conds0, DiscrIf.newLabel) {
  def toAST: IfThenElse =
    IfThenElse(Cond.toExpr(conds),
      actions.map(Discretely(_)).toList, Nil)
  def dump = {
    val post = postConds()
    (label + ": "
      + Pretty.pprint[Action](toAST)
      + "\nPost Conds: "
      + Pretty.pprint[Expr](Cond.toExpr(post))
      + "\n")
  }
  def postConds(conds0: Seq[Cond] = null) : Seq[Cond] = {
    var res = ArrayBuffer((if (conds0 != null) conds0 else conds): _*)
    actions.foreach {
      case a @ Assign(lhs, rhs) => (Cond.getName(lhs), rhs) match {
        case (Some(name), Lit(value)) =>
          invalidate(List(name))
          res += Eq(name, value)
        case (Some(name), expr) =>
          invalidate(Cond.extractDeps(expr))
        case _ =>
        /* unhandled case, FIXME, Warning or Error */
      }
    }
    def invalidate(deps: Seq[Name]) = res.indices.foreach { i =>
      val reqDeps = Cond.getDeps(res(i))
      if (!deps.intersect(reqDeps).isEmpty)
        res(i) = null
    }
    res.filter(_ != null)
  }
  var matches: Seq[ContIf] = null;
  var maybe: Seq[ContIf] = null;
  def reset = { matches = null; maybe = null; }
  def dup(conds: Seq[Cond]) = {
    var res = new DiscrIf(conds)
    res.actions ++= actions
    res
  }
}
object DiscrIf extends MkIf[DiscrIf] {
  def apply(conds: Seq[Cond]) = new DiscrIf(conds)
  var counter = 0
  def newLabel = { counter += 1; "d" + counter; }
}

class Ifs[ActionT, IfT <: If[ActionT]](mkIf: MkIf[IfT]) {
  //def findAll(Seq[Conds]) : Seq[If[ActionT]]
  //def find(Seq[Conds]) : Maybe[If[ActionT]]
  val data = new MutListMap[Seq[Cond], IfT]
  var emptyIfs: List[IfT] = null;
  def find(conds: Seq[Cond]): Option[IfT] = data.get(conds)
  def add(conds: Seq[Cond]): IfT = data.getOrElseUpdate(conds, mkIf(conds))
  def uniquify() {
    var i = 0
    breakable { while (true) {
      val bla = withCondsOfLength(i)
      if (bla.isEmpty) break
      bla.foreach { if1 =>
        val ifs2 = withPrefix(if1.conds)
        ifs2.foreach { if2 =>
          if2.actions ++= if1.actions
        }
        if (!ifs2.isEmpty) if1.actions.clear
      }
      i += 1;
    }}
    emptyIfs = data.values.filter(_.actions.isEmpty).toList
    data.retain { (_, if0) => !if0.actions.isEmpty }
  }
  def withCondsOfLength(length: Int): Iterable[IfT] =
    data.filterKeys { _.size == length }.values
  def withPrefix(toFind: Seq[Cond]): Iterable[IfT] =
    data.filterKeys { conds => conds.size > toFind.size && conds.startsWith(toFind) }.values
  def reset() { data.values.foreach(_.reset); }
}

class Extraction(actions0: List[Action]) {
  var contIfs = new Ifs[ContinuousAction, ContIf](ContIf)
  var discrIfs = new Ifs[Assign, DiscrIf](DiscrIf)
  // notConds is here to simplify other operations
  def extract(conds: Seq[Cond], claims: List[Cond], notConds: Seq[Cond], action: Action) {
    action match {
      case IfThenElse(cond, ifTrue, ifFalse) =>
        val ifTrueConds = conds :+ Cond(cond)
        val ifFalseConds = conds :+ Cond.not(cond)
        extract(ifTrueConds, claims, ifFalseConds, ifTrue)
        extract(ifFalseConds, claims, ifTrueConds, ifFalse)
      case Switch(subject, clauses) =>
        val switchNot = conds ++ clauses.map { case Clause(lhs, _, _) => Not(Cond.eq(subject, lhs)) }
        clauses.foreach {
          case Clause(lhs, newClaim, actions) =>
            val newClaims = if (newClaim != Lit(GBool(true))) claims :+ Cond(newClaim) else claims
            extract(conds :+ Cond.eq(subject, lhs), newClaims, switchNot, actions)
        }
      case Continuously(action) =>
        val if0 = contIfs.add(conds)
        if0.claims = claims
        if0.actions += action
        contIfs.add(notConds)
      case Discretely(action: Assign) =>
        discrIfs.add(conds).actions += action
        discrIfs.add(notConds)
      case _ => /* error, unsupported */
    }
  }
  def extract(conds: Seq[Cond], claims: List[Cond], notConds: Seq[Cond], actions: List[Action]) {
    actions.foreach { action => extract(conds, claims, notConds, action) }
  }
  extract(Nil, Nil, Nil, actions0)
}

class Extract(prog: Prog) extends Extraction(prog.defs.find { cd => cd.name == ClassName("Main") }.orNull.body) {

  val forceGuards = false
  val unsafe = true

  val mainClass = prog.defs.find { cd => cd.name == ClassName("Main") }.orNull
  var init = mainClass.priv
  val simulatorName = mainClass.fields(0)
  var simulatorAssigns: Seq[Assign] = Nil

  val MODE = Name("$mode", 0)
  val MODE_VAR = Dot(Var(Name("self", 0)), MODE)
  val MODE0 = Name("$mode0", 0)


  def extractSimulatorAssigns(if0: DiscrIf) = {
    var (sim, non) = if0.actions.partition {
      _ match {
        case Assign(Dot(Var(s), _), Lit(_)) if s == simulatorName => true
        case Assign(Dot(Dot(Var(Name("self", 0)), s), _), Lit(_)) if s == simulatorName => true
        case a @ _ => false
      }
    }
    simulatorAssigns ++= sim
    if0.actions = non
  }
  // remove simulator assigns at the top level
  discrIfs.data.get(Nil).foreach(extractSimulatorAssigns(_))

  contIfs.uniquify
  discrIfs.uniquify

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

  def foldDiscrIf =
    discrIfs.data.values.foreach { if0 =>
      breakable { while (true) {
        val matches = discrIfs.data.values.filter(_.matchConds(if0.postConds()) == CondTrue).toList
        if (matches.size != 1) break
        val if1 = matches(0)
        if (if0 == if1) break
        if (getDeps(if0, _.rhs).intersect(getDeps(if1, _.lhs)).nonEmpty) break
        if (getDeps(if0, _.lhs).intersect(getDeps(if1, _.rhs)).nonEmpty) break
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

  def matchUp =
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

  // WARNING: This step makes assumtions that are not only true 99.9%
  // of the time
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

  if (needGuards.nonEmpty) {
    discrIfs.data ++= discrIfs.emptyIfs.map { if0 => (if0.conds, if0) }
    contIfs.data.values.filter { _.needGuard }.foreach { if0 =>
      //println("Need guards on " + if0.label)
      def ensureEmpties(cond: Seq[Cond]): Unit = if (!cond.isEmpty) {
        val init = cond.init
        val last = cond.last
        discrIfs.add(init :+ last)
        discrIfs.add(init :+ Cond.not(last))
        ensureEmpties(init)
      }
      ensureEmpties(if0.conds)
      val modeVal = GStr(if0.label)
      discrIfs.add(if0.conds).actions += Assign(Var(MODE0), Lit(modeVal))
      if0.conds = if0.conds :+ Eq(MODE0, modeVal)
    }
    contIfs.reset
    discrIfs.reset
    discrIfs.uniquify
    foldDiscrIf
    matchUp
  }

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
      case Init(name, ExprRhs(Lit(value))) => List(Eq(name, value))
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
  // determined, collect resets that need to be in D0 mode
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
    if0.conds = Eq(MODE, GStr(if0.label)) +: if0.conds
  }

  //
  // Cleanup
  // 

  // for each mode, kill all uncessary discr. assignemnts in
  // the resets, if the reset than has no actions, kill it.
  modes.foreach{if0 => 
    if0.resets.foreach{if1=>
      val preConds = if0.conds ++ if1.conds
      // keep the mode assignment operation even if the mode does not
      // change as it is needed by the enclosure interpreter
      var modeAssign : Assign = null
      if1.actions = if1.actions.filter{case a@Assign(lhs,rhs) => !((Cond.getName(lhs), rhs) match {
        case (Some(name), Lit(value)) => 
          val res = preConds.exists(_ == Eq(name,value))
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
  val allDeps = (contRHSDeps ++ discrDeps).distinct :+ MODE // $mode is special and needs to be kept
  val allVars = init.map{case Init(n,_) => n} :+ MODE0 // $mode0 should be killed if not there
                                                                   // is a likely problem
  val kill = allVars.diff(allDeps)
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

  val theSwitch = Switch(MODE_VAR,
                         modes.map{if0 => Clause(GStr(if0.label),
                                                 Cond.toExpr(if0.claims),
                                                 if0.resets.map(_.toAST) ++
                                                 if0.actions.map(Continuously(_)).toList)})
  val newMain = ClassDef(mainClass.name,
                         mainClass.fields,
                         init, List(theSwitch) ++ simulatorAssigns.map(Discretely(_)))

  val res = new Prog(List(newMain))

  // Now put it all together
} 
