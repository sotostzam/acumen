package acumen.interpreters.enclosure

import acumen._
import acumen.interpreters.enclosure.tree._
import Interpreter._
import Expression._
import Relation._
import Types._

trait Extract {

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
   * The domain invariant of the mode is declared by the 'require' keyword
   * which may be provided after the value matched on in the case clause.
   * Omitted requiress are parsed as constant 'true' invariants.
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
        val (hybridSystem, uncertainInitialState) = body.filter(!_.isInstanceOf[Discretely]) match {
          case List(stateMachine) => stateMachine match {
            case Switch(Dot(Var(Name(self, 0)), Name(modeVariable, 0)), clauses: List[Clause]) =>
              (getHybridSystem(modeVariable, stateVariables - modeVariable, clauses),
                getInitialState(modeVariable, stateVariables - modeVariable, priv))
            case Switch(e, _) => sys.error("Switching on " + e + " not allowed!")
            case _ => sys.error("Handling of state machines expressed using control constructs other than switch not implemented!")
          }
          case _ => sys.error("Handling of multiple switch statements not implemented!")
        }
        (hybridSystem, uncertainInitialState)
      }
    }

  /**
   * Extracts solver parameter values embedded in an Acumen class
   *
   * The assignments must be in the top level block, i.e. on the
   * same level as the single switch statement encoding the hybrid
   * automaton and not nested within it.
   *
   * At most one assignment per simulator parameter may be made.
   *
   * The order of assignments does not matter.
   */
  def parameters(classDef: ClassDef): Parameters =
    classDef match {
      case ClassDef(
        name: ClassName,
        fields: List[Name],
        priv: List[Init],
        body: List[Action]) => {
        val assignments = body.filter(_.isInstanceOf[Discretely]).map {
          case Discretely(Assign(Dot(Dot(Var(Name(self, 0)), Name(simulator, 0)), Name(param, 0)), rhs @ Lit(GInt(_) | GDouble(_)))) => (param, rhs)
          case _ => sys.error("Top level assignments have to be a numeric constant assigned to a simulator parameter!")
        }
        val checkAssignments = assignments.groupBy { case (name, _) => name }.map {
          case (n, rhss) => rhss.size match {
            case 1 => ()
            case _ => sys.error("Muliple assignments to simulator." + n + " are not allowed!")
          }
        }
        // FIXME coercing each integer to a double and back is not ideal...
        val defaultParameters = Map[String, Double](
          "precision" -> 10,
          "startTime" -> 0,
          "endTime" -> 3,
          "initialConditionPadding" -> 0,
          "extraPicardIterations" -> 20,
          "maxPicardIterations" -> 200,
          "maxEventSequenceLength" -> 30,
          "minTimeStep" -> 0.01,
          "maxTimeStep" -> 3,
          "minImprovement" -> 0.0000001,
          "splittingDegree" -> 1)
        val params = assignments.foldLeft(defaultParameters) {
          case (res, (param, Lit(GInt(i)))) => res + (param -> i.toDouble)
          case (res, (param, Lit(GDouble(d)))) => res + (param -> d)
          case _ => sys.error("Should never happen!")
        }
        Parameters(
          params("precision").toInt,
          params("startTime"),
          params("endTime"),
          params("initialConditionPadding"),
          params("extraPicardIterations").toInt,
          params("maxPicardIterations").toInt,
          params("maxEventSequenceLength").toInt,
          params("minTimeStep"),
          params("maxTimeStep"),
          params("minImprovement"),
          params("splittingDegree").toInt)
      }
    }

  def getHybridSystem(
    modeVariable: String,
    stateVariables: List[String],
    clauses: List[Clause])(implicit rnd: Rounding) =
    clauses.foldLeft(HybridSystem.empty) {
      case (res, clause) => {
        val (mode, domain, field) = getMode(stateVariables, clause)
        val es = getEvents(modeVariable, stateVariables, clause)
        es.foldLeft(res.addMode(mode, domain, field)) {
          case (r, (event, guard, reset)) => r.addEvent(event, guard, reset)
        }
      }
    }

  def getInitialState(modeVariable: String, stateVariables: List[VarName], priv: List[Init])(implicit rnd: Rounding) = {
    val initialMode = priv.filter {
      case Init(Name(i, 0), m) => i == modeVariable
      case _ => false
    } match {
      case List(Init(_, ExprRhs(Lit(gv)))) => groundValueToMode(gv)
      case _ => sys.error("Could not identify initial mode!")
    }
    val intializations = priv.map {
      case Init(name, ExprRhs(initialValueExpr)) => (name.x + "'" * name.primes) -> initialValueExpr
    }.toMap
    val initialCondition = intializations.filterKeys(stateVariables.contains(_)).
      mapValues(acumenExprToExpression(_)(rnd)(Box.empty))
    UncertainState(initialMode, initialCondition)
  }

  def getEvents(
    modeVariable: String,
    stateVariables: List[String],
    clause: Clause)(implicit rnd: Rounding): List[(Event, Guard, ResetMap)] = {
    var countEventsWithSameTarget = Map[Mode, Int]()
    clause match {
      case Clause(source: GroundValue, assertion: Expr, as: List[Action]) => as.flatMap {
        case IfThenElse(_, _, _ :: _) =>
          sys.error("Handling of else-branches in conditional statements not implemented!")
        case IfThenElse(cond: Expr, as: List[Action], _) => {
          val event = getEvent(modeVariable, source, as)
          val guard = getGuard(cond)
          val reset = getReset(modeVariable, stateVariables, as)
          countEventsWithSameTarget += event.tau -> (countEventsWithSameTarget.getOrElse(event.tau, 0) + 1)
          val uniqueEvent = Event(event.name + " " + countEventsWithSameTarget(event.tau), event.sigma, event.tau)
          List((uniqueEvent, guard, reset))
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
    as: List[Action])(implicit rnd: Rounding): ResetMap = {
    val resetComponents = as.flatMap {
      case Discretely(Assign(lhs @ Dot(Var(Name("self", 0)), Name(name, _)), rhs)) =>
        if (name == modeVariable) List()
        else List((lhs, rhs))
      case Discretely(Assign(lhs, rhs)) => sys.error("Assignment of " + rhs + " to " + lhs + " is not supported.")
      case _ => List()
    }.toMap.map {
      case (Dot(_, kName), v) =>
        val Variable(k) = acumenExprToExpression(Var(kName))
        k -> acumenExprToExpression(v)
    }
    ResetMap(stateVariables.map(name => name -> resetComponents.getOrElse(name, Variable(name))).toMap)
  }

  def getMode(stateVariables: List[VarName], clause: Clause)(implicit rnd: Rounding): (Mode, Domain, Field) = {
    clause match {
      case Clause(modeVariable: GroundValue, assertion: Expr, as: List[Action]) => {
        val domain = acumenExprToPredicate(assertion).asInstanceOf[Domain]
        val field = getField(stateVariables, as)
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

  def getField(stateVariables: List[VarName], as: List[Action])(implicit rnd: Rounding) = {
    val highestDerivatives = as.flatMap {
      case Continuously(EquationT(d @ Dot(Var(Name(self, 0)), _), rhs)) => List((d, rhs))
      case Continuously(EquationT(lhs, rhs)) => sys.error("Continuous assignment of " + rhs + " to " + lhs + " is not supported.")
      case _ => List()
    }.filter { case (Dot(_, Name(_, n)), _) => n != 0 }.toMap
    val fieldComponents = as.flatMap {
      case Continuously(EquationI(lhs @ Dot(Var(Name("self", 0)), nameLhs), rhs @ Dot(Var(Name("self", 0)), nameRhs))) =>
        List((lhs, rhs))
      case _ => List()
    }.toMap.map {
      case (Dot(_, kName), v) =>
        val Variable(k) = acumenExprToExpression(Var(kName))
        val e = acumenExprToExpression(highestDerivatives.getOrElse(v, v))
        k -> acumenExprToExpression(highestDerivatives.getOrElse(v, v))
    }
    //    Field(stateVariables.map(name => name -> fieldComponents.getOrElse(name, Variable(name))).toMap)
    Field(stateVariables.map(name => name -> fieldComponents.getOrElse(name, Constant(0))).toMap)
  }

  def getStateVariables(priv: List[Init]): List[String] = {
    val initializationsByName = priv.groupBy { case Init(Name(name, _), _) => name }
    val ordersByName = initializationsByName.mapValues { _.map { case Init(Name(_, order), _) => order } }
    val maxOrderByName = ordersByName.mapValues(_.max)
    priv.
      filter {
        case Init(Name(name, order), _) => maxOrderByName(name) == 0 || maxOrderByName(name) > order
      }.
      map { case Init(Name(name, order), _) => name + "'" * order }.toList
  }

  def acumenExprToPredicate(e: Expr)(implicit rnd: Rounding): Predicate = e match {
    case Lit(GBool(b)) if b => True
    case _ => All(acumenExprToRelations(e))
  }

  def acumenExprToRelations(e: Expr)(implicit rnd: Rounding): List[Relation] = e match {
    case Op(Name("&&", 0), List(l, r)) => acumenExprToRelations(l) ++ acumenExprToRelations(r)
    case Op(Name("<=" | "<" | "==" | ">" | ">=", _), _) => List(acumenExprToRelation(e))
    case _ => sys.error("Handling of predicates " + e + "not implemented!")
  }

  def acumenExprToRelation(e: Expr)(implicit rnd: Rounding): Relation = e match {
    case Op(Name("<=", 0), List(x, y)) => lessThanOrEqualTo(acumenExprToExpression(x), acumenExprToExpression(y))
    case Op(Name("<", 0), List(x, y)) => lessThan(acumenExprToExpression(x), acumenExprToExpression(y))
    case Op(Name("==", 0), List(x, y)) => equalTo(acumenExprToExpression(x), acumenExprToExpression(y))
    case Op(Name("~=", 0), List(x, y)) => notEqualTo(acumenExprToExpression(x), acumenExprToExpression(y))
    case Op(Name(">", 0), List(x, y)) => lessThan(acumenExprToExpression(y), acumenExprToExpression(x))
    case Op(Name(">=", 0), List(x, y)) => lessThanOrEqualTo(acumenExprToExpression(y), acumenExprToExpression(x))
    //    case Op(Name("<=", 0), List(x, y)) => nonPositive(acumenExprToExpression(x) - acumenExprToExpression(y))
    //    case Op(Name("<", 0), List(x, y)) => negative(acumenExprToExpression(x) - acumenExprToExpression(y))
    //    case Op(Name("==", 0), List(x, y)) => equalToZero(acumenExprToExpression(x) - acumenExprToExpression(y))
    //    case Op(Name(">", 0), List(x, y)) => positive(acumenExprToExpression(x) - acumenExprToExpression(y))
    //    case Op(Name(">=", 0), List(x, y)) => nonNegative(acumenExprToExpression(x) - acumenExprToExpression(y))
    case _ => sys.error("Handling of relation " + e + " not implemented!")
  }

  def acumenExprToExpression(e: Expr)(implicit rnd: Rounding): Expression = e match {
    case Lit(GInt(d)) => Constant(d)
    case Lit(GDouble(d)) => Constant(d)
    case Lit(GConstPi) => Constant(Interval.pi)
    case ExprInterval(lo, hi) => Constant(foldConstant(lo).value /\ foldConstant(hi).value)
    case ExprIntervalM(mid0, pm0) =>
      val mid = foldConstant(mid0).value
      val pm = foldConstant(pm0).value
      Constant((mid - pm) /\ (mid + pm))
    case Var(Name(name, n)) => Variable(name + "'" * n)
    case Dot(Var(Name(self, 0)), Name(name, n)) => Variable(name + "'" * n)
    case Op(Name("-", 0), List(x)) => Negate(acumenExprToExpression(x))
    case Op(Name("abs", 0), List(x)) => Abs(acumenExprToExpression(x))
    case Op(Name("sqrt", 0), List(x)) => Sqrt(acumenExprToExpression(x))
    case Op(Name("-", 0), List(l, r)) => acumenExprToExpression(l) - acumenExprToExpression(r)
    case Op(Name("+", 0), List(l, r)) => acumenExprToExpression(l) + acumenExprToExpression(r)
    case Op(Name("/", 0), List(l, r)) => Divide(acumenExprToExpression(l), acumenExprToExpression(r))
    case Op(Name("*", 0), List(l, r)) => acumenExprToExpression(l) * acumenExprToExpression(r)
    case _ => sys.error("Handling of expression " + e + " not implemented!")
  }

  def foldConstant(e: Expr)(implicit rnd: Rounding): Constant = e match {
    case Lit(GInt(i)) => Constant(i)
    case Lit(GDouble(d)) => Constant(d)
    case Lit(_) => sys.error("foldConstant called with non-numeric expression!")
    case Op(Name("-", 0), List(x)) => Constant(-foldConstant(x).value)
    case Op(Name("-", 0), List(l, r)) => Constant(foldConstant(l).value - foldConstant(r).value)
    case Op(Name("+", 0), List(l, r)) => Constant(foldConstant(l).value + foldConstant(r).value)
    case Op(Name("*", 0), List(l, r)) => Constant(foldConstant(l).value * foldConstant(r).value)
    case Op(Name("/", 0), List(l, r)) => Constant(foldConstant(l).value / foldConstant(r).value)
    case _ => sys.error("foldConstant called with nonconstant expression!")
  }

  def toGDouble(gv: GroundValue): GDouble = gv match {
    case GInt(i) => GDouble(i)
    case GDouble(_) => gv.asInstanceOf[GDouble]
    case _ => sys.error("found: " + gv + " , expected: GInt or GDouble")
  }

}
