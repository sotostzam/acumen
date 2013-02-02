package acumen
package interpreters
package enclosure
import java.text.ParseException
import acumen.Pretty.pprint

trait Checker {

  type ErrorMessage = String
  
  // checks that the class embeds a hybrid automaton

  def checkValidAutomatonEmbedding(d: ClassDef) {
    containsPreciselyOneSwitch(d.body)
    switchesOnUnprimedVariable(d.body)
    switchIsAutomaton(d.body)
    allNonSwitchActionsAssignToSimulatorParameter(d.body)
  }
    
  def containsPreciselyOneSwitch(as: List[Action]) {
	if (as.filter(_.isInstanceOf[Switch]).size != 1) sys.error("The model must contain precisely one switch statement.")
  }
    
  def switchesOnUnprimedVariable(as: List[Action]) = as.map {
    case Switch(Dot(Var(Name(self, 0)), Name(x, 0)), _) => 
    case Switch(_, _) => sys.error("The switch statement must switch on an unprimed variable.")
    case _ =>
  }
  
  def switchIsAutomaton(as: List[Action]) = as.filter(_.isInstanceOf[Switch]).map {
    case Switch(Dot(Var(Name(self, 0)), modeName), cs) => cs.map(isMode(modeName, _))
    case _ => sys.error("should never happen")
  }
  
  // TODO add checking that actual parameter names are used
  def allNonSwitchActionsAssignToSimulatorParameter(as: List[Action]) {
    as.filterNot(_.isInstanceOf[Switch]).map{
      case Discretely(Assign(
        Dot(Dot(Var(Name(self, 0)), Name(simulator, 0)), Name(_, 0)),
        Lit(GInt(_) | GDouble(_)))) => true
      case a => sys.error(
        "All top-level non-switch statements must be assignments to a simulator parameter.\n" +
      	"Not allowed in this position:\n" + pprint(a))
    }
  }

  // checks that the clause `c` defines a mode

  def isMode(modeVariable: Name, c: Clause) = c match {
    case Clause(_, _, as) =>
      as.filter(_.isInstanceOf[IfThenElse]).map(isEvent(modeVariable, _))
      isField(as.filterNot(_.isInstanceOf[IfThenElse]))
    case _ => sys.error("Invalid case in switch statement:\n" + pprint(c))
  }

  // checks that the action `a` defines an event 

  def isEvent(modeName: Name, a: Action) = a match {
    case IfThenElse(cond, t, e) if e.isEmpty =>
      assignsModePreciselyOnce(modeName, t)
      containsOnlyAssignments(t)
      containsNoReassignments(t)
    case _ => sys.error("If statements may not contain an else branch.")
  }
  
  def assignsModePreciselyOnce(modeName: Name, as: List[Action]) = if (as.filter {
    case Discretely(Assign(Dot(Var(Name(self, 0)), x), _)) => x == modeName
    case _ => false
  }.size != 1) sys.error(
      "Precisely one assignment to switched variable \"" + pprint(modeName) + "\" in each if statement is required.")
  
  def containsOnlyAssignments(as: List[Action]) = as.map {
    case Discretely(Assign(_, _)) => 
    case a => sys.error(
      "The only statements allowed in an if statement are discrete assignments.\n" +
      "Not allowed in this position: " + pprint(a))
  }   
  
  def containsNoReassignments(as: List[Action]) = as.groupBy {
    case Discretely(Assign(Dot(Var(Name(self, 0)), x), _)) => x
    case _ => sys.error("should never happen")
  }.map { case (x,a) => if (a.size != 1) sys.error("Reassignment to varaible \"" + pprint(x) +"\" not allowed.") }

  // checks that the actions `as` define a field

  def isField(as: List[Action]) = {
    containsOnlyContinuousAssignments(as)
	atMostOneContinuousAssignmentPerVariable(as)
	onlyContinuousAssignmentsToPrimedVariables(as)
  } 

  // note: assumes being called on desugared program
  def containsOnlyContinuousAssignments(as: List[Action]) =
    as.map(a => if (!a.isInstanceOf[Continuously]) 
      sys.error(
        "The statements in a case, outside if statements, must be continuous assignments.\n" +
      	"Not allowed in this position: " + pprint(a)))
    
  def atMostOneContinuousAssignmentPerVariable(as: List[Action]) {
    as.map(_.asInstanceOf[Continuously].a).
      filter(_.isInstanceOf[EquationT]).groupBy {
        case EquationT(Dot(Var(Name(self, 0)), Name(x, _)), _) => x
        case _ => sys.error("should never happen")
      }.map { case (x,a) => if (a.size != 1) 
        sys.error("Continuous reassignment to variable \"" + x + "\" is not allowed.")
      }
  }
    
  def onlyContinuousAssignmentsToPrimedVariables(as: List[Action]) {
    as.map(_.asInstanceOf[Continuously].a).
      filter(_.isInstanceOf[EquationT]).map{
        case EquationT(Dot(Var(Name(self, 0)), Name(n, primes)), _) => 
          if (primes <= 0) sys.error("Continuous assignment to unprimed variable \"" + n + "\" not allowed.")
        case _ => sys.error("should never happen")
      }
  }
  
}