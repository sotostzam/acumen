package acumen.interpreters.enclosure

import acumen._
import acumen.interpreters.enclosure.solver._
import Expression._
import Relation._
import Types._

trait Transform {

  /**
   * Extracts a hybrid automaton embedded as an Acumen class
   *
   * The automaton is represented by a single class.
   *
   * The state variables of the automaton are declared as private
   * variables, each by a single initialization assignment and by
   * a primed version also declared as a private variable by a
   * single initialization assignment.
   *
   * Higher derivatives are not allowed.
   *
   * The automaton graph is represented as a single switch statement
   * with the variable that is switched on acting as a mode designator.
   *
   * The domain invariant of the mode is declared by the 'assert' keyword
   * which may be provided after the value matched on in the case clause.
   * Omitted assertions are parsed as constant 'true' invariants.
   *
   * Each case in the switch statement corresponds to a node (a mode)
   * in the automaton.
   *
   * Each mode is specified by a field declared as a
   * continuous assignment to each of the state variables.
   *
   * Each case in the switch statement also contains a set of conditional
   * statements each corresponding to an edge (an event) in the automaton.
   *
   * Each event is specified by a guard, a reset map and a target mode.
   *
   * The guard is given by the guard of a conditional statement with no
   * else branch.
   *
   * The reset map is given in the body of the conditional statement by a
   * set of discrete assignments to the state variables. The components of
   * the reset map that are not given by discrete assignments are taken to
   * be the identity.
   *
   * The target mode of the event is given in the body of the conditional
   * statement  by a discrete assignment to the mode designator variable.
   *
   */
  def extract(classDef: ClassDef)(implicit rnd: Rounding): (HybridSystem, UncertainState) =
    classDef match {
      case ClassDef(
        name: ClassName,
        fields: List[Name],
        priv: List[Init],
        body: List[Action]) => {
        val stateVariables = getStateVariables(priv)
        body match {
          case List(stateMachine) => {
            stateMachine match {
              //              case Switch(Var(Name(modeVariable, 0)), clauses: List[Clause]) =>
              case Switch(Dot(Var(Name(self, 0)), Name(modeVariable, 0)), clauses: List[Clause]) =>
                val hybridSystem = getHybridSystem(modeVariable, stateVariables, clauses)
                val uncertainInitialState = getInitialState(modeVariable, priv)
                (hybridSystem, uncertainInitialState)
              case Switch(e, _) => sys.error("Switching on " + e + " not allowed!")
              case _ => sys.error("Handling of state machines expressed using control constructs other than switch not implemented!")
            }
          }
          case _ => sys.error("Handling of multiple switch statements not implemented!")
        }
      }
    }

  def getHybridSystem(
    modeVariable: String,
    stateVariables: List[String],
    clauses: List[Clause])(implicit rnd: Rounding) =
    clauses.foldLeft(HybridSystem.empty) {
      case (res, clause) => {
        val (mode, domain, field) = getMode(clause)
        val es = getEvents(modeVariable, stateVariables, clause)
        es.foldLeft(res.addMode(mode, domain, field)) {
          case (r, (event, guard, reset)) => r.addEvent(event, guard, reset)
        }
      }
    }

  def getInitialState(modeVariable: String, priv: List[Init])(implicit rnd: Rounding) = {
    val initialMode = priv.filter {
      case Init(Name(i, 0), m) => i == modeVariable
      case _ => false
    } match {
      case List(Init(_, ExprRhs(Lit(gv)))) => groundValueToMode(gv)
      case _ => sys.error("Could not identify initial mode!")
    }
    //TODO Update this to support higher order derivatives
    val initialCondition = priv.flatMap {
      case Init(Name(name, 0), ExprRhs(initialValueExpr)) if (name != modeVariable) => {
        val e = acumenExprToExpression(initialValueExpr)
        // Here we are assuming that e only contains constants
        List((name, e(Box.empty)))
      }
      case _ => List()
    }.toMap
    UncertainState(initialMode, initialCondition)
  }

  def getEvents(
    modeVariable: String,
    stateVariables: List[String],
    clause: Clause)(implicit rnd: Rounding): List[(Event, Guard, ResetMap)] = {
    clause match {
      case Clause(source: GroundValue, assertion: Expr, as: List[Action]) => as.flatMap {
        case IfThenElse(_, _, _ :: _) =>
          sys.error("Handling of else-branches in conditional statements not implemented!")
        case IfThenElse(cond: Expr, as: List[Action], _) => {
          val event = getEvent(modeVariable, source, as)
          val guard = getGuard(cond)
          val reset = getReset(modeVariable, stateVariables, as)
          List((event, guard, reset))
        }
        case _ => List()
      }
    }
  }

  def getEvent(modeVariable: String, source: GroundValue, as: List[Action]): Event =
    as.flatMap {
      case Discretely(Assign(Dot(Var(Name(self, 0)), Name(mv, 0)), Lit(target))) =>
        if (mv == modeVariable) List(Event(groundValueToMode(source), groundValueToMode(target)))
        else List()
      case _ => List()
    } match {
      case List(e) => e
      case _ => sys.error("Each if-branch in conditional statements must contain precisely one assignment to " + modeVariable)
    }

  def getGuard(cond: Expr)(implicit rnd: Rounding) = acumenExprToPredicate(cond).asInstanceOf[Guard]

  def getReset(
    modeVariable: String,
    stateVariables: List[String],
    as: List[Action])(implicit rnd: Rounding): ResetMap = as.flatMap {
    case Discretely(Assign(Var(Name(name, 0)), rhs)) if (name != modeVariable) =>
      List((name, acumenExprToExpression(rhs)))
    case _ => List()
  }.foldLeft(ResetMap.id(stateVariables.toSet)) { case (res, kv) => res + kv }

  def getMode(clause: Clause)(implicit rnd: Rounding): (Mode, Domain, Field) = {
    clause match {
      case Clause(modeVariable: GroundValue, assertion: Expr, as: List[Action]) => {
        val domain = acumenExprToPredicate(assertion).asInstanceOf[Domain]
        val field = getField(as)
        val mode = groundValueToMode(modeVariable)
        (mode, domain, field)
      }
    }
  }

  def groundValueToMode(gv: GroundValue) = Mode(gv match {
    case GInt(i) => i.toString
    case GDouble(d) => d.toString
    case GBool(b) => b.toString
    case GStr(s) => s
  })

  //  def getField(as: List[Action])(implicit rnd: Rounding) = Field(as.flatMap {
  //    case Continuously(Equation(Var(Name(name, 1)), rhs)) => List((name, rhs))
  //    case _ => List()
  //  }.toMap.mapValues(acumenExprToExpression(_)))

  def getField(as: List[Action])(implicit rnd: Rounding) = {
    val highestDerivatives = as.flatMap {
      case Continuously(EquationT(d @ Dot(Var(Name(self, 0)), _), rhs)) => List((d, rhs))
      case Continuously(EquationT(lhs, _)) => sys.error("Continuous assignment to " + lhs + " is not supported.")
      case _ => List()
    }.toMap
    Field(as.flatMap {
      case Continuously(EquationI(Dot(Var(Name(self, 0)), Name(name, n)), rhs @ Dot(_, _))) => List((name + "'" * n, rhs))
      case _ => List()
    }.toMap.mapValues(v => highestDerivatives.getOrElse(v, v)).mapValues(acumenExprToExpression(_)))
  }

  def getStateVariables(priv: List[Init]): List[String] = {
    val variablesByName = priv.groupBy { case Init(Name(name, _), _) => name }
    val ordersByName = variablesByName.mapValues { _.map { case Init(Name(_, order), _) => order } }
    ordersByName.filter {
      case (name, orders) => orders.toSet == Set(0, 1)
    }.keys.toList
  }

  def acumenExprToPredicate(e: Expr)(implicit rnd: Rounding): Predicate =
    All(acumenExprToRelations(e))

  def acumenExprToRelations(e: Expr)(implicit rnd: Rounding): List[Relation] = e match {
    case Op(Name("&&", 0), List(l, r)) => acumenExprToRelations(l) ++ acumenExprToRelations(r)
    case Op(Name("<=" | "<" | "==" | ">" | ">=", _), _) => List(acumenExprToRelation(e))
    case _ => sys.error("Handling of predicates " + e + "not implemented!")
  }

  def acumenExprToRelation(e: Expr)(implicit rnd: Rounding): Relation = e match {
    case Op(Name("<=", 0), List(Lit(GInt(0) | GDouble(0)), x)) => nonNegative(acumenExprToExpression(x))
    case Op(Name("<=", 0), List(x, Lit(GInt(0) | GDouble(0)))) => nonPositive(acumenExprToExpression(x))
    case Op(Name("<", 0), List(Lit(GInt(0) | GDouble(0)), x)) => positive(acumenExprToExpression(x))
    case Op(Name("<", 0), List(x, Lit(GInt(0) | GDouble(0)))) => negative(acumenExprToExpression(x))
    case Op(Name("==", 0), List(Lit(GInt(0) | GDouble(0)), x)) => equalToZero(acumenExprToExpression(x))
    case Op(Name("==", 0), List(x, Lit(GInt(0) | GDouble(0)))) => equalToZero(acumenExprToExpression(x))
    case Op(Name(">", 0), List(Lit(GInt(0) | GDouble(0)), x)) => negative(acumenExprToExpression(x))
    case Op(Name(">", 0), List(x, Lit(GInt(0) | GDouble(0)))) => positive(acumenExprToExpression(x))
    case Op(Name(">=", 0), List(Lit(GInt(0) | GDouble(0)), x)) => nonPositive(acumenExprToExpression(x))
    case Op(Name(">=", 0), List(x, Lit(GInt(0) | GDouble(0)))) => nonNegative(acumenExprToExpression(x))
    case _ => sys.error("Handling of relation " + e + " not implemented!")
  }

  def acumenExprToExpression(e: Expr)(implicit rnd: Rounding): Expression = e match {
    case Lit(GInt(d)) => Constant(d)
    case Lit(GDouble(d)) => Constant(d)
    case Var(Name(name, n)) => Variable(name + "'" * n)
    case Dot(Var(Name(self, 0)), Name(name, n)) => Variable(name + "'" * n)
    case Op(Name("-", 0), List(x)) => Negate(acumenExprToExpression(x))
    case Op(Name("+", 0), List(l, r)) => Plus(acumenExprToExpression(l), acumenExprToExpression(r))
    case Op(Name("/", 0), List(l, r)) => Divide(acumenExprToExpression(l), acumenExprToExpression(r))
    case Op(Name("*", 0), List(l, r)) => Multiply(acumenExprToExpression(l), acumenExprToExpression(r))
    case _ => sys.error("Handling of expression " + e + " not implemented!")
  }

}
