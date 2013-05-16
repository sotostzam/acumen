package acumen.interpreters.enclosure

import Relation._
import Types._
import acumen.interpreters.enclosure.Box.toBox
import acumen.interpreters.enclosure.solver.tree.HybridSystem
import acumen.interpreters.enclosure.solver.tree.ResetMap
import acumen._

trait Extract {

  /** Extracts a hybrid automaton from a single Acumen class. */
  def extract(classDef: ClassDef)(implicit rnd: Rounding): (HybridSystem, UncertainState) =
    classDef match {
      case ClassDef(
        name: ClassName,
        fields: List[Name], // will only contain "simulator"
        priv: List[Init],
        body: List[Action]) =>
        val stateVarNames = priv.map(i => nameToVarName(i.x))
        val stateVarInits = stateVarNames.map(n => n -> Constant(0)).toMap[VarName, Expression]
        val ics = initialConditions(priv)
        val defaultMode = Mode("defaultMode")
        val f = field(stateVarInits, body)
        val hs = HybridSystem.empty.addMode(defaultMode, True, f)
        val initialState = UncertainState(defaultMode, ics)
        val res = (hs, initialState)
        res
    }

  /** Extracts an initial condition box from a list of initialization statements. */
  private def initialConditions(priv: List[Init])(implicit rnd: Rounding) =
    Box.toBox(priv.map { 
      case Init(n, ExprRhs(Lit(GInt(i)))) => nameToVarName(n) -> Interval(i)
    }.toMap[VarName, Interval])

  /** Updates an initial field with information extracted from a list of actions (the body of the class). */
  //FIXME Handling of continuous assignments is not implemented!
  private def field(initialField: Map[VarName, Expression], body: List[Action])(implicit rnd: Rounding): Field = {
    def simplifyEquations[A] = body
      .filter(i => i.isInstanceOf[Continuously] && i.asInstanceOf[Continuously].a.isInstanceOf[A])
      .map { 
      	case Continuously(EquationI(Dot(_, n), rhs)) => (n -> rhs)
      	case Continuously(EquationT(Dot(_, n), rhs)) => (n -> rhs)
      }.toMap
    val equationsI = simplifyEquations[EquationI]
    val equationsT = simplifyEquations[EquationT]
    Field(equationsI.foldLeft(initialField) {
      case (res, (n, r)) =>
        val newName = Name(n.x, n.primes + 1)
        res + (nameToVarName(n) -> acumenExprToExpression(equationsT.getOrElse(newName, r)))
    })
  }

  /** Extracts the local name from a qualified name (Dot). */
  private def nameFromDot(dot: Expr) = dot match { case Dot(_, n) => n }

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
  
  /** Converts a name with explicit number of primes into a VarName (a string). */
  private def nameToVarName(n: Name) = n.x + List.fill(n.primes)("'").mkString("")

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
          "bigDecimalDigits" -> 10,
          "startTime" -> 0,
          "endTime" -> 3,
          "initialPicardPadding" -> 0,
          "picardImprovements" -> 20,
          "maxPicardIterations" -> 200,
          "maxEventTreeSize" -> 30,
          "minTimeStep" -> 0.01,
          "minSolverStep" -> 0.01,
          "minLocalizationStep" -> 0.001,
          "maxTimeStep" -> 3,
          "minComputationImprovement" -> 0.0001,
          "splittingDegree" -> 1,
          "maxIterations" -> 100)
        val params = {
          val assignedParameters = assignments.map(_._1)
          val updatedParameters = assignments.foldLeft(defaultParameters) {
            case (res, (param, l)) =>
              if (defaultParameters.keySet contains param) res + (param -> toDouble(l))
              else sys.error(param + " is not a recognized parameter.")
            case _ => sys.error("Should never happen!")
          }
          if (assignedParameters contains "maxTimeStep")
            updatedParameters
          else {
            val startTime = updatedParameters("startTime")
            val endTime = updatedParameters("endTime")
            updatedParameters + ("maxTimeStep" -> (endTime - startTime))
          }
        }
        Parameters(
          params("bigDecimalDigits").toInt,
          params("startTime"),
          params("endTime"),
          params("initialPicardPadding"),
          params("picardImprovements").toInt,
          params("maxPicardIterations").toInt,
          params("maxEventTreeSize").toInt,
          params("minTimeStep"),
          params("minSolverStep"),
          params("minLocalizationStep"),
          params("maxTimeStep"),
          params("minComputationImprovement"),
          params("splittingDegree").toInt,
          params("maxIterations").toInt)
      }
    }

  /** Extracts the ground value double stored in the Lit. */
  private def toDouble(l: Lit): Double = l match {
    case Lit(GInt(i)) => i
    case Lit(GDouble(d)) => d
    case _ => sys.error("Non numeric literal cannot be cast to Double.")
  }

}
