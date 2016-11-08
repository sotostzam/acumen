/**
 * Symbolic differentiation
 * Authors: Yingfu Zeng, Adam Duracz
 */

package acumen

import Specialization._
import GenSym._
import Pretty._
import Errors._
import spire.math.Rational

/**
 * Hashed version of symbolic differentiation
 * */
object SD {
  import util.Names.name
  val hashFlag = false
  val print = new Pretty()
  /**
   * Apply symbolic differentiation (dif) to all expressions in the program.
   */
  def run(t: Prog): Prog = t match {
    case Prog(defs) => Prog(
      defs.map(d => d match {
        case ClassDef(cName, fields, privs, body) =>
          ClassDef(
            cName,
            fields, 
            privs,
            body.map(runAction(_, collectVars(privs))))
      }))
  }

  /* Collect all the variables been initialized */
  def collectVars(privs: List[Init]): List[Var] =
    privs.map(init => Var(init.x))
  /**
   * Apply symbolic differentiation (dif) to the expressions contained in an action.
   */
  private def runAction(action: Action, vars: List[Var]): Action = action match {
    case IfThenElse(cond, t, as) => {
      IfThenElse(runExpr(cond, vars), t.map(runAction(_, vars)), as.map(runAction(_, vars)))
    }
    case Switch(subject, clauses) => Switch(runExpr(subject, vars), clauses map (runClause(_, vars)))
    case ForEach(it, col, body) => ForEach(it, runExpr(col, vars), body map (runAction(_, vars)))
    case Continuously(continousAction) => Continuously(continousAction match {
      case Equation(lhs, rhs) =>
        Equation(
          runExpr(lhs, vars),
          runExpr(rhs, vars))
      case EquationI(lhs, rhs) =>
        EquationI(
          runExpr(lhs, vars),
          runExpr(rhs, vars))
      case EquationT(lhs, rhs) =>
        EquationT(
          runExpr(lhs, vars),
          runExpr(rhs, vars))
      case Assignment(lhs, rhs) =>
        Assignment(
          runExpr(lhs, vars).asInstanceOf[Pattern],
          runExpr(rhs, vars))
    })
    case Discretely(discreteAction) => Discretely(discreteAction match {
      case Assign(lhs: Expr, rhs: Expr) =>
        Assign(
          runExpr(lhs, vars),
          runExpr(rhs, vars))
      case _ => discreteAction
    })
    case Claim(cond) => Claim(runExpr(cond,vars)) 
    case Hypothesis(s, cond) => Hypothesis(s, runExpr(cond,vars)) 
  }

  /**
   * Apply symbolic differentiation (dif) to the expressions contained in a clause.
   * A clause is a case in a switch statement. Affected expressions are 1) those
   * contained in the actions of the case statement and 2) the expression which is
   * used to decide that this specific case should be taken.
   */

  private def runClause(c: Clause, vars: List[Var]): Clause = Clause(c.lhs, runExpr(c.assertion, vars), c.rhs map (runAction(_, vars)))

  /**
   * This function merely checks the existence of symbolic differentiation operator
   * without enabling the partial evaluator
   */
  private def runExpr(e: Expr, vars: List[Var]): Expr =
    e match {
      /* Unary function */
      case Op(opName, args) => opName.x match {
        case "dif" => 
          // Reaching this point means having partial derivative without enabling BTA
          throw symbolicDifWithoutBTA(e)
        // Example: 1 + dif(x^2)
        case _ => Op(opName, args.map(runExpr(_, vars)))
      }
      case Pattern(es) => Pattern(es map (runExpr(_, vars)))
      case ExprVector(es) => ExprVector(es.map(runExpr(_, vars)))
      case _ => e
    }

  /**
   * Type synonyms
   */
  type Context = scala.collection.mutable.HashMap[Expr, (Expr, List[Action])]

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
  private def mem(e: Expr): (Expr,List[Action]) =
    ctx.get(e) match {
      /* If the expression already exists in the context, return the 
       * cached reference.
       */
      case Some(ec) => ec
      /* Otherwise save a reference to the expression e in the context 
       * ctx along with a unique variable name x_i, where i is the 
       * index of the expression in the context.
       */
      case _ => {
        if (hashFlag) {
          val newVar = gensym(e :: Nil)
          val hashEquation = Continuously(Equation(newVar, e))
          ctx += (e -> (newVar, hashEquation :: Nil)); (newVar, hashEquation :: Nil)
        } else {
          ctx += (e -> (e, Nil)); (e, Nil)
        }
      }
    }
  
  private def mem(e1: Expr,e2:Expr,neweqs:List[Action]): (Expr,List[Action]) =
    ctx.get(e1) match {
      /* If the expression already exists in the context, return the 
       * cached reference.
       */
      case Some(ec) => ec
      /* Otherwise save a reference to the expression e in the context 
       * ctx along with a unique variable name x_i, where i is the 
       * index of the expression in the context.
       */
      case _ => {
        if (hashFlag) {
          val newVar = gensym(e2 :: Nil)
          val hashEquation = Continuously(Equation(newVar, e2))
          ctx += (e1 -> (newVar, hashEquation :: neweqs)); (newVar, hashEquation :: neweqs)
        } else {
          ctx += (e1 -> (e2, Nil)); (e2, Nil)
        }
      }
    }
  
  def clear() = { ctx.clear(); GenSym.clear()}
  
   class HashExpr(val expr:Expr, val eqs:List[Action]){
    def pair = (expr,eqs)
  }
   object HashExpr{
    val rationalZero = Lit(GRational(Rational.zero))
    def apply(e:Expr):HashExpr = e match{
      case Dot(_,_) => new HashExpr(e,Nil)
      case Lit(GRational(value)) => literal(value)
      case Var(n) => variable(n)
      case Op(Name(f,_),es) => Operator(f,es.map(x => apply(x)))
      case ExprVector(es) => HashVector(es.map(x=> HashExpr(x)))
  }
   def apply(he:HashExpr, eqs:List[Action]):HashExpr = 
     new HashExpr(he.expr, he.eqs:::eqs.distinct)
   }
   class Literal(expr:Expr, eqs:List[Action]) extends HashExpr(expr,eqs){

  }
   object Literal{
     def apply(value:Rational)=  new Literal(Lit(GRational(value)), Nil)
   }
   
   class Variable(expr:Expr, eqs:List[Action]) extends HashExpr(expr,eqs){

  }
   class Operator(expr:Expr,eqs:List[Action]) extends HashExpr(expr,eqs){
     
   }
   object Operator{
     // Memorize every expression
    def apply(n:String, hes:List[HashExpr]):HashExpr= {
      val es = hes.map(_.expr)
      Simplifier.mkOp(n,es:_*) match{
      case Lit(GRational(r)) => literal(r)
      case op => val em = mem(op); new HashExpr(em._1,em._2)
      }
   }
   }
  class HashVector(expr: Expr, eqs: List[Action]) extends HashExpr(expr, eqs)
  object HashVector {
    def apply(es: List[HashExpr]): HashExpr = {
      val hop = ExprVector(es.map(x => x.expr))
      val neweqs = es.foldLeft(List[Action]())((r, x) => x.eqs ::: r).distinct
      val em = mem(hop);
      new HashExpr(em._1, em._2 ::: neweqs)
    }
  }
     
  /* Smart constructors */ // TODO Make regular constructors private to force use of these. 

  /** Smart constructor for the Literal case class */
  def literal(value: Int) = Literal(Rational(value))
  /** Smart constructor for the Literal case class */
  def literal(value: Rational) = Literal(value)

  /** Smart constructor for the Variable case class */
  def variable(name: Name) = new Variable(Var(name),Nil)
 
  /** Smart constructor for the Op case class */
  def op(n: String, es: List[HashExpr]):HashExpr = Operator(n,es)

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
   *  
   *  Also, when dealing with implcite PDs, for example we have an equation
   *  for x'' = x*p, and ask for dif(x,p).  Instead of returning 
   */
  def dif(e: Expr,n: Name, env:Map[Expr,Expr],equations:List[Action]): HashExpr = {
    val difE = dif(_:Expr,n,env,equations)
    implicit def convert (e:Expr) = HashExpr(e)
    
    e match {
      case Lit(_) =>
        literal(0)
      case Var(m) => 
        val difmn = Op(Name("dif",0), List(Var(m), Var(n)))
        if (m == n)
          literal(1)
        else if (ctx.contains(difmn))
            ctx.get(difmn).get match{
          case (result,neweqs) => 
            new Operator(result,
             // Filter out additional equations that are already in Env
             neweqs.filter{x => !equations.contains(x)}) 
        }           
        else if (env.contains(Var(m))){
          dif(env(Var(m)), n,env,equations)        
         }
          else {         
            equations.map(findDirectedVars(_, Var(m))).filter(x => x != None) match {
              case Some(e @ Continuously(Equation(lhs, rhs))) :: res => (lhs,rhs) match {
                case (_,Lit(_)) =>  literal(0)
                case (Var(Name(_, p)),_) =>
                  val newVar = gensym(Var(m) :: Var(n)::Nil)
                  val newVarprime = Var(Name(newVar.name.x, p))               
                  val difrhs = difE(rhs)                
                  for(i <- 0 to p-1)
                    mem(Op(Name("dif",0), List(Var(Name(m.x,i)), Var(n))), Var(Name(newVar.name.x, i)),difrhs.eqs)
                 difrhs
                case _ => error("Not a valid assignemnt")

              }
              case other =>             
               mem(difmn, literal(0).expr,Nil); literal(0)
            }
          }
              
      case Dot(_, x) =>
        if (x == n)  literal(1) else literal(0)
      case ExprVector(es) => 
        val esd = es map difE  
        HashVector(esd)
      /* Unary function */
      case Op(opName, List(arg)) => // Chain rule
        val difarg = difE(arg)
        Operator("*",
          /* op.x is the string itself  */
          List(
            opName.x match {
              case "sin" => op("cos", List(arg))
              case "cos" => op("*", List(literal(-1), op("sin", List(arg))))
              case "tan" =>  op("^",List(op("/",List(literal(1),op("cos",List(arg)))),literal(2)))
              case "atan" => op("/", List(literal(1), op("+", List(literal(1), op("^", List(arg, literal(2)))))))
              case "acos" => op("/", List(literal(-1),
                op("^", List(op("-", List(literal(1), op("^", List(arg, literal(2))))), literal(Rational(1,2))))))
              case "asin" => op("/", List(literal(1),
                op("^", List(op("-", List(literal(1), op("^", List(arg, literal(2))))), literal(Rational(1,2))))))
              case "exp"  => op("exp", List(arg))
              case "log"  => op("/", List(literal(1), arg))
              case "sqrt" => op("*", List(literal(Rational(1,2)), op("^", List(arg, literal(-Rational(1,2))))))

            },
            difarg))
      /* Binary operator */
      case Op(opName, List(l, r)) =>
        val difl = difE(l); val difr = difE(r)
        if (opName.x != "dif")
          opName.x match {
            case "+" => op("+", List(difl, difr))
            // Product (Leibnitz) rule
            case "*" =>
              op("+", List(
                op("*", List(difl, r)),
                op("*", List(l, difr))))
            case "-" => op("-", List(difl, difr))
            // Quotient rule
            case "/" =>
              op("/",
                List(
                  op("-",
                    List(
                      op("*",
                        List(difl, r)),
                      op("*",
                        List(l, difr)))),
                  op("^", List(r, literal(2)))))
            case "^" =>
              r match {
                case Lit(GRational(m)) if m.isWhole =>
                  op("*",
                    List(
                      literal(m),
                      op("*", List(
                        op("^", List(l, literal(m - 1))),
                        difl))))
                /* General solutions for D[x1^x2,x], where x1 and x2 are exps containing x:
             *  D[x1^x2,x] = (x1^x2) *(x2'*ln(x1) + (x1'/x1)*x2 )
             */
                case _ => op("*", List(
                  op("^", List(l, r)),
                  op("+", List(
                    op("*", List(difr, op("log", List(l)))),
                    op("*", List(op("/", List(difl, l)), r))))))
              }
          }
        else{
          val dif1 = dif(l,r.asInstanceOf[Var].name,env,equations)
          val dif2 = dif(dif1.expr,n,env,dif1.eqs:::equations)
          dif2
        }
    }
  }

  /**
   *  Symbolic differentiation with respect to time
   *
   *  Assumption(IMPORTANT): Every variable in "e" is assumed to be a time varing vairiable,
   *  in other words, every constant variable should already been replaced by its value in the
   *  BTA phase.
   *
   */
  def td(e: Expr,env:Map[Expr,Expr],conditionalAction:List[Action]): HashExpr = {
    val dt = td(_:Expr,env,conditionalAction)
    implicit def convert (e:Expr) = HashExpr(e)
    e match {
      case Lit(_) => literal(0)
      case Var(n) => 
        if (env.contains(Var(n))){
         td(env(Var(n)), env, conditionalAction)        
        }
        else{
           conditionalAction.map(findDirectedVars(_, Var(n))).filter(x => x != None) match {
              case Some(e @ Continuously(Equation(lhs, rhs))) :: res => (lhs,rhs) match {
                case (Var(Name(_, p)),_) =>
                 if (p  == n.primes)
                   dt(rhs)
                 else
                  variable(Name(n.x, n.primes + 1))
                case _ => error("Not a valid assignemnt")

              }
              case other =>             
               variable(Name(n.x, n.primes + 1))
            }   
        }       
      
      case Dot(_, x) => e
      case ExprVector(es) => HashVector(es map dt)     
      /* Unary function */
      case Op(opName, List(arg)) => // Chain rule
        val difarg = dt(arg)
        if (opName.x != "dif")
          op("*",
            /* op.x is the string itself  */
            List(
              opName.x match {
                case "sin" => op("cos", List(arg))
                case "cos" => op("*", List(literal(-1), op("sin", List(arg))))
                case "tan" =>  op("^",List(op("/",List(literal(1),op("cos",List(arg)))),literal(2)))
                case "atan" => op("/", List(literal(1), op("+", List(literal(1), op("^", List(arg, literal(2)))))))
                case "acos" => op("/", List(literal(-1), 
                                 op("^", List(op("-",List(literal(1),op("^",List(arg,literal(2))))), literal(Rational(1,2))))))
                case "asin" => op("/", List(literal(1), 
                                 op("^", List(op("-",List(literal(1),op("^",List(arg,literal(2))))), literal(Rational(1,2))))))
                case "exp" => op("exp", List(arg))
                case "log" => op("/", List(literal(1), arg))
               
              },
              difarg))
        else
          HashExpr(dt(difarg.expr),difarg.eqs)
      /* Binary operator */
      case Op(opName, List(l, r)) =>
        val dtl = dt(l); val dtr = dt(r)
        opName.x match {
          case "+" => op("+", List(dtl, dtr))
          // Product (Leibnitz) rule
          case "*" =>
            op("+", List(
              op("*", List(dtl, r)),
              op("*", List(l, dtr))))
          case "-" => op("-", List(dtl, dtr))
          // Quotient rule
          case "/" =>
            op("/",
              List(
                op("-",
                  List(
                    op("*",
                      List(dtl, r)),
                    op("*",
                      List(l, dtr)))),
                op("*", List(r, literal(2)))))
          case "^" =>
            r match {
              case Lit(GRational(m)) if m.isWhole =>
                op("*",
                  List(
                    literal(m),
                    op("*", List(
                      op("^", List(l, literal(m - 1))),
                      dtl))))
              /* General solutions for D[x1^x2,x], where x1 and x2 are exps containing x:
             *  D[x1^x2,x] = (x1^x2) *(x2'*ln(x1) + (x1'/x1)*x2 )
             */
              case _ => op("*", List(
                op("^", List(l, r)),
                op("+", List(
                  op("*", List(dtr, op("log", List(l)))),
                  op("*", List(op("/", List(dtl, l)), r))))))
            }
          case "atan2" => 
            op("+",
                List(
                    op("*",List(literal(-1), 
                        op("*", List(op("/", List(l, op("+", 
                            List(op("^",List(l,literal(2))),op("^",List(r,literal(2))))))),
                                   dtr)))),
                    op("*", List(op("/", List(r, op("+", 
                            List(op("^",List(l,literal(2))),op("^",List(r,literal(2))))))),
                                   dtl))))
          /* Example d(df/dx)/dt */
          case "dif" => r match {      
            case Var(name) => 
               // Fix me !
               val difarg = dif(l,name,Map[Expr,Expr](),conditionalAction)
               val dtarg = dt(difarg.expr)
              HashExpr(dtarg,difarg.eqs)
            // TODO: Fix error message
            case _ => sys.error(r + " is not a variable")
          }

        }
    }
  }
  def findDirectedVars(a:Action,v:Var):Option[Action] = {
    a match{
      case IfThenElse(cond,t,e) =>
        val at = t.filter(x => x match{
          case Continuously(e@Equation(lhs,rhs)) => lhs match{
            case Var(v.name) => true
            case _ => false
          }
        })
       val ae = e.filter(x => x match{
          case Continuously(e@Equation(lhs,rhs)) => lhs match{
            case Var(v.name) => true
            case _ => false
          }
        })
     (at,ae) match{
          case (Continuously(tt@Equation(lhs1,rhs1))::Nil, Continuously(ee@Equation(lhs2,rhs2))::Nil) =>
            if (lhs1 == lhs2 && lhs1 == Var(v.name))
              Some(IfThenElse(cond,at,ae))
            else
              None
          case (_,_) => None
        }
      case Continuously(e @ Equation(lhs, rhs)) => lhs match {
        case Var(n) =>
          if (n.x == v.name.x && ((n.primes > v.name.primes) || (n.primes == v.name.primes))) {
            Some(a)
          } else {
            None
          }
      }
      case _ => None
    }
  }
}
