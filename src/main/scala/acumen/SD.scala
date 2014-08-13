/**
 * Symbolic differentiation
 * Authors: Yingfu Zeng, Adam Duracz
 */

package acumen

object SD {

  import util.Names.name

  /**
   * Apply symbolic differentiation (dif) to all expressions in the program.
   */
  def run(t: Prog): Prog = t match {
    case Prog(defs) => Prog(
      defs.map(d => d match {
        case ClassDef(cName, fields, privs, body) =>
          ClassDef(
            cName,
            fields, // TODO Do we need to apply dif here?
            privs, // TODO Do we need to apply dif here?
            body map runAction)
      }))
  }

  /**
   * Apply symbolic differentiation (dif) to the expressions contained in an action.
   */
  private def runAction(action: Action): Action = action match {
    case IfThenElse(cond, t, as) => {
      IfThenElse(runExpr(cond), t map runAction, as map runAction)
    }
    case Switch(subject, clauses) => Switch(runExpr(subject), clauses map runClause)
    case ForEach(it, col, body) => ForEach(it, runExpr(col), body map runAction)
    case Continuously(continousAction) => Continuously(continousAction match {
      case Equation(lhs, rhs) =>
        Equation(
          runExpr(lhs),
          runExpr(rhs))
      case EquationI(lhs, rhs) =>
        EquationI(
          runExpr(lhs),
          runExpr(rhs))
      case EquationT(lhs, rhs) =>
        EquationT(
          runExpr(lhs),
          runExpr(rhs))
    })
    case Discretely(discreteAction) => Discretely(discreteAction match {
      case Assign(lhs: Expr, rhs: Expr) =>
        Assign(
          runExpr(lhs),
          runExpr(rhs))
      case _ => discreteAction
    })
    case Claim(cond) => Claim(runExpr(cond)) 
    case Hypothesis(s, cond) => Hypothesis(s, runExpr(cond)) 
  }

  /**
   * Apply symbolic differentiation (dif) to the expressions contained in a clause.
   * A clause is a case in a switch statement. Affected expressions are 1) those
   * contained in the actions of the case statement and 2) the expression which is
   * used to decide that this specific case should be taken.
   */
  private def runClause(c: Clause): Clause = Clause(c.lhs, runExpr(c.assertion), c.rhs map runAction)

  /**
   * Apply symbolic differentiation (dif) to an expression.
   * This is done by traversing the expression tree, looking for subexpressions
   * of the form "dif(f)(n)". Such subexpressions are then replaced with the
   * result of applying the dif function.
   */
  private def runExpr(e: Expr): Expr =
    e match {
      /* Unary function */
      case Op(opName, args) => opName.x match {
        case "dif" => args match {
          // f is the function that we are diffing, 
          // n is the variable w.r.t which we are diffing 
          case List(f, Var(n)) => dif(f)(n)
        }
        // Example: 1 + dif(x^2)
        case _ => Op(opName, args map runExpr).setPos(e.pos)
      }
      case _ => e
    }

  /**
   * Type synonyms
   */
  type Context = scala.collection.mutable.HashMap[Expr, Expr]

  /**
   * The expression context. Used to store bindings of expressions to
   * variable names, keyed by the expressions themselves.
   */
  val ctx = new Context()

  /**
   * Memoization funciton, used for hash-consing of subexpressions.
   * This function is called by the smart constructors and helps to
   * avoid that duplicates of subexpressions are stored in memory.
   * The resulting structure of references on the heap can be used
   * to build a reduced expression using a series of let-statements
   * corresponding to the hash-consed subexpressions.
   */
  private def mem(e: Expr): Expr =
    ctx.get(e) match {
      /* If the expression already exists in the context, return the 
       * cached reference.
       */
      case Some(ec) => ec
      /* Otherwise save a reference to the expression e in the context 
       * ctx along with a unique variable name x_i, where i is the 
       * index of the expression in the context.
       */
      case _ => { ctx += (e -> e); e }
    }

  /* Smart constructors */ // TODO Make regular constructors private to force use of these. 

  /** Smart constructor for the Literal case class */
  def literal(value: Double) = mem(Lit(GDouble(value)))
  /** Smart constructor for the Literal case class */
  def literal(value: Int) = mem(Lit(GInt(value)))

  /** Smart constructor for the Variable case class */
  def variable(name: Name) = mem(Var(name))

  /** Smart constructor for the Op case class */
  def op(n: String, es: List[Expr]): Expr = {
    n match {
      /* Operators */
      case "sin" => es match {
        case Lit(GDouble(0)) :: Nil => literal(0)
        case _ => mem(Op(name("sin"), es))
      }
      case "cos" => es match {
        case Lit(GDouble(0)) :: Nil => literal(1)
        case _ => mem(Op(name("cos"), es))
      }
      case "exp" => es match {
        case Lit(GDouble(0)) :: Nil => literal(1)
        case _ => mem(Op(name("exp"), es))
      }
      // Natural logarithm
      case "log" => es match {
        case List(Op(Name("^", 0), List(l, r))) =>
          op("*",
            List(mem(op("log", List(l))), r))
        case _ => mem(Op(name("log"), es))
      }
      /* Operators */
      case "+" => es match {
        case List(Lit(GDouble(0)), r) => r
        case List(l, Lit(GDouble(0))) => l
        case List(Lit(GDouble(n1)), Lit(GDouble(n2))) => literal(n1 + n2)
        case _ => mem(Op(name("+"), es))
      }
      case "-" => es match {
        case _ => mem(Op(name("-"), es))
      }
      case "*" => es match {
        case _ => mem(Op(name("*"), es))
      }
      case "/" => es match {
        case _ => mem(Op(name("/"), es))
      }
      case "^" => es match {
        case _ => mem(Op(name("^"), es))
      }
    }
  }

  /** Smart constructor for the ExprVector case class */
  def exprVector(l: List[Expr]) = mem(ExprVector(l))

  /** Smart constructor for the Sum case class */
  def sum(e: Expr, i: Name, col: Expr, cond: Expr) = mem(Sum(e, i, col, cond))

  /**
   *  Symbolic differentiation.
   *
   *  Differentiates the expression "e" with respect to a named variable "n".
   *
   *  The function looks for occurrences of the "dif" operator in "e". For
   *  exmaple, "dif(e1)(x)" is replaced with an expression "e2" corresponding
   *  to the analytic solution of differentiating "e1" with respect to "x".
   */
  def dif(e: Expr)(n: Name): Expr = {
    e match {
      case Lit(_) =>
        literal(0)
      case Var(m) =>
        if (m == n) literal(1) else literal(0)
      case Dot(_, x) =>
        if (x == n) literal(1) else literal(0)
      case ExprVector(es) =>
        exprVector(es.map(dif(_)(n)))
      case Sum(e, i, col, cond) =>
        sum(dif(e)(n), i, dif(col)(n), dif(cond)(n))
      /* Unary function */
      case Op(opName, List(arg)) => // Chain rule
        op("*",
          /* op.x is the string itself  */
          List(
            opName.x match {
              case "sin" => op("cos", List(arg))
              case "cos" => op("*", List(literal(-1), op("sin", List(arg))))
              case "exp" => op("exp", List(arg))
              case "log" => op("/", List(literal(1), arg))
            },
            dif(arg)(n)))
      /* Binary operator */
      case Op(opName, List(l, r)) =>
        opName.x match {
          case "+" => op("+", List(dif(l)(n), dif(r)(n)))
          // Product (Leibnitz) rule
          case "*" =>
            op("+", List(
              op("*", List(dif(l)(n), r)),
              op("*", List(l, dif(r)(n)))))
          case "-" => op("-", List(dif(l)(n), dif(r)(n)))
          // Quotient rule
          case "/" =>
            op("/",
              List(
                op("-",
                  List(
                    op("*",
                      List(dif(l)(n), r)),
                    op("*",
                      List(l, dif(r)(n))))),
                op("*", List(r, literal(2)))))
          case "^" =>
            r match {
              case Lit(GInt(m)) =>
                op("*",
                  List(
                    literal(m),
                    op("*", List(
                      op("^", List(l, literal(m - 1))),
                      dif(l)(n)))))
              /* General solutions for D[x1^x2,x], where x1 and x2 are exps containing x:
             *  D[x1^x2,x] = (x1^x2) *(x2'*ln(x1) + (x1'/x1)*x2 )
             */
              case _ => op("*", List(
                op("^", List(l, r)),
                op("+", List(
                  op("*", List(dif(r)(n), op("log", List(l)))),
                  op("*", List(op("/", List(dif(l)(n), l)), r))))))
            }
        }
      /* Other case classes pass through unaffected */
      case e => e
    }
  }

}
