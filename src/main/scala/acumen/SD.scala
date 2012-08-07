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

  def runAction(action: Action): Action = action match {
    case IfThenElse(cond, t, as) => {
      IfThenElse(runExpr(cond), t map runAction, as map runAction)} // TODO Do we need to apply dif here?
    case Switch(subject, clauses) => Switch(runExpr(subject), clauses map runClause) // TODO Do we need to apply dif here?
    case ForEach(it, col, body) => ForEach(it, runExpr(col), body map runAction) // TODO Do we need to apply dif here?
    case Continuously(continousAction) => Continuously(continousAction match {
      case Equation(lhs, rhs) =>
        Equation(
          lhs, // TODO To support DAEs we probably need to apply dif here also  
          runExpr(rhs))
      case EquationI(lhs, rhs) =>
        EquationI(
          lhs, // TODO To support DAEs we probably need to apply dif here also  
          runExpr(rhs))
      case EquationT(lhs, rhs) =>
        EquationT(
          lhs, // TODO To support DAEs we probably need to apply dif here also  
          runExpr(rhs))
    })
    case Discretely(discreteAction) => Discretely(discreteAction match{
      case Assign(lhs:Expr, rhs:Expr) => 
        Assign(
          lhs, 
          runExpr(rhs))
      case _ => discreteAction
    }) // TODO Do we need to apply dif here?
  }
  
  def runClause(c:Clause): Clause = Clause(c.lhs, c.rhs map runAction)
  
  /**
   * Apply symbolic differentiation (dif) to an expression.
   * This is done by traversing the expression tree, looking for subexpressions 
   * of the form "dif(f)(n)". Such subexpressions are then replaced with the 
   * result of applying the dif function.
   */
  def runExpr(e: Expr): Expr =
    e match {
      /* Unary function */
      case Op(opName, args) => opName.x match {
        case "dif" => args match {
          // f is the funciton that we are diffing, 
          // n is the variable w.r.t which we are diffing 
          case List(f, Var(n)) => dif(f)(n)
        }
        // Example: 1 + dif(x^2)
        case _ => Op(opName,args map runExpr)
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

  def mem(e: Expr): Expr =
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

  /* Smart constructors */

  def literal(value: Double) = mem(Lit(GDouble(value)))
  def literal(value: Int) = mem(Lit(GInt(value)))

  def variable(name: Name) = mem(Var(name))

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

  /**
   *  Symbolic differentiation
   */
  def dif(e: Expr)(n: Name): Expr = {
    e match {
      case Lit(_) =>
        literal(0)
      case Var(m) =>
        if (m == n) literal(1) else literal(0)
      case Dot(_, x) => if (x == n) literal(1) else literal(0)
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

      /* Binary operator*/
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
    }
  }

}