package acumen
package interpreters
package enclosure

trait Checker {

  // checks that the class embeds a hybrid automaton

  def embedsAutomaton(d: ClassDef) =
    containsPreciselyOneSwitch(d.body) &&
      switchesOnUnprimedVariable(d.body) &&
      switchIsAutomaton(d.body) &&
      allNonSwitchActionsAssignToSimulatorParameter(d.body)
  def containsPreciselyOneSwitch(as: List[Action]) =
    as.filter(_.isInstanceOf[Switch]).size == 1
  def switchesOnUnprimedVariable(as: List[Action]) = as.forall {
    case Switch(Dot(Var(Name(self, 0)), _), _) => true
    case Switch(_, _) => false
    case _ => true
  }
  def switchIsAutomaton(as: List[Action]) = as.filter(_.isInstanceOf[Switch]).forall {
    case Switch(Dot(Var(Name(self, 0)), modeName), cs) => cs.forall(isMode(modeName, _))
    case _ => sys.error("should never happen")
  }
  // TODO add checking that actual parameter names are used
  def allNonSwitchActionsAssignToSimulatorParameter(as: List[Action]) =
    as.filterNot(_.isInstanceOf[Switch]).forall {
      case Discretely(Assign(
        Dot(Dot(Var(Name(self, 0)), Name(simulator, 0)), Name(_, 0)),
        Lit(GInt(_) | GDouble(_)))) => true
      case _ => false
    }

  // checks that the clause `c` defines a mode

  def isMode(modeVariable: Name, c: Clause) = c match {
    case Clause(_, _, as) =>
      as.filter(_.isInstanceOf[IfThenElse]).forall(isEvent(modeVariable, _)) &&
        isField(as.filterNot(_.isInstanceOf[IfThenElse]))
    case _ => false
  }

  // checks that the action `a` defines an event 

  def isEvent(modeName: Name, a: Action) = a match {
    case IfThenElse(cond, t, e) if e.isEmpty =>
      assignsModePreciselyOnce(modeName, t) &&
        containsOnlyAssignments(t) &&
        containsNoReassignments(t)
    case _ => false
  }
  def assignsModePreciselyOnce(modeName: Name, as: List[Action]) = as.filter {
    case Discretely(Assign(Dot(Var(Name(self, 0)), x), _)) => x == modeName
    case _ => false
  }.size == 1
  def containsOnlyAssignments(as: List[Action]) = as.forall {
    case Discretely(Assign(_, _)) => true
    case _ => false
  }
  def containsNoReassignments(as: List[Action]) = as.groupBy {
    case Discretely(Assign(Dot(Var(Name(self, 0)), x), _)) => x
    case _ => sys.error("should never happen")
  }.values.forall(_.size == 1)

  // checks that the actions `as` define a field

  def isField(as: List[Action]) = {
    println(as)
    containsOnlyContinuousAssignments(as) &&
      atMostOneContinuousAssignmentPerVariable(as) &&
      onlyContinuousAssignmentsToPrimedVariables(as)
  }

  // note: assumes being called on desugared program
  def containsOnlyContinuousAssignments(as: List[Action]) =
    as.forall(_.isInstanceOf[Continuously])
  def atMostOneContinuousAssignmentPerVariable(as: List[Action]) =
    as.map(_.asInstanceOf[Continuously].a).
      filter(_.isInstanceOf[EquationT]).groupBy {
        case EquationT(Dot(Var(Name(self, 0)), Name(x, _)), _) => x
        case _ => sys.error("should never happen")
      }.values.forall(_.size == 1)
  def onlyContinuousAssignmentsToPrimedVariables(as: List[Action]) =
    as.map(_.asInstanceOf[Continuously].a).
      filter(_.isInstanceOf[EquationT]).forall {
        case EquationT(Dot(Var(Name(self, 0)), Name(_, primes)), _) => primes > 0
        case _ => sys.error("should never happen")
      }

}
