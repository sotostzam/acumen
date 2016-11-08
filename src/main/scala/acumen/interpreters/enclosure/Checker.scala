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
	if (as.filter(_.isInstanceOf[Switch]).size != 1) sys.error("Program text is outside the subset of Acumen currently supported by the enclosure semantics.  The model must contain precisely one switch statement.")
  }
    
  def switchesOnUnprimedVariable(as: List[Action]) = as.map {
    case Switch(Dot(Var(Name(self, 0)), Name(x, 0)), _) => 
    case Switch(_, _) => sys.error("Program text is outside the subset of Acumen currently supported by the enclosure semantics.  The switch statement must switch on an unprimed variable.")
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
        Lit(GRational(_)))) => true
      case a => sys.error(
        "Program text is outside the subset of Acumen currently supported by the enclosure semantics.  All top-level non-switch statements must be assignments to a simulator parameter.\n" +
      	"Offending statement:\n" + pprint(a))
    }
  }

  // checks that the clause `c` defines a mode

  def isMode(modeVariable: Name, c: Clause) = c match {
    case Clause(_, _, as) =>
      as.filter(_.isInstanceOf[IfThenElse]).map(isEvent(modeVariable, _))
      isField(as.filterNot(_.isInstanceOf[IfThenElse]))
    case _ => sys.error("Program text is outside the subset of Acumen currently supported by the enclosure semantics.  The following switch statement case needs to set the mode variable:\n" + pprint(c))
  }

  // checks that the action `a` defines an event 

  def isEvent(modeName: Name, a: Action) = a match {
    case IfThenElse(cond, t, e) if e.isEmpty =>
      assignsModePreciselyOnce(modeName, t)
      containsOnlyAssignments(t)
      containsNoReassignments(t)
    case _ => sys.error("Program text is outside the subset of Acumen currently supported by the enclosure semantics.  If statements may not contain an else branch.")
  }
  
  def assignsModePreciselyOnce(modeName: Name, as: List[Action]) = if (as.filter {
    case Discretely(Assign(Dot(Var(Name(self, 0)), x), _)) => x == modeName
    case _ => false
  }.size != 1) sys.error(
      "Program text is outside the subset of Acumen currently supported by the enclosure semantics.There must be xactly one assignment to mode variable \"" + pprint(modeName) + "\" in each if statement.")
  
  def containsOnlyAssignments(as: List[Action]) = as.map {
    case Discretely(Assign(_, _)) => 
    case a => sys.error(
      "Program text is outside the subset of Acumen currently supported by the enclosure semantics.  All statements allowed in an if statement must be discrete assignments.\n" +
      "Offending statement: " + pprint(a))
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
        "Program text is outside the subset of Acumen currently supported by the enclosure semantics.  All statements in a case must be continuous assignments or if statements.\n" +
      	"Offending statement: " + pprint(a)))
    
  def atMostOneContinuousAssignmentPerVariable(as: List[Action]) {
    as.map(_.asInstanceOf[Continuously].a).
      filter(_.isInstanceOf[EquationT]).groupBy {
        case EquationT(Dot(Var(Name(self, 0)), Name(x, _)), _) => x
        case _ => sys.error("should never happen")
      }.map { case (x,a) => if (a.size != 1) 
        sys.error("Program text is outside the subset of Acumen currently supported by the enclosure semantics.  Multiple continuous assignments to the same variable are not allowed.  Check assignments to the variable \"" + x + "\".")
      }
  }
    
  def onlyContinuousAssignmentsToPrimedVariables(as: List[Action]) {
    as.map(_.asInstanceOf[Continuously].a).
      filter(_.isInstanceOf[EquationT]).map{
        case EquationT(Dot(Var(Name(self, 0)), Name(n, primes)), _) => 
          if (primes <= 0) sys.error("Program text is outside the subset of Acumen currently supported by the enclosure semantics.  All continuous assignments must be to primed variables (check assignments to \"" + n + "\").")
        case _ => sys.error("should never happen")
      }
  }
  
}
