package acumen
import Array._
import util.Names.name
import Pretty._
import acumen.interpreters.Common._
import Specialization._
import Simplifier._
import Error._
import interpreters.Common.printMatrix
import scala.collection.immutable.Vector
import spire.math.Rational

/** Gaussian elimination */
object GE {
  /* Coefficient matrix */
  case class CMatrix(val M: Matrix) {
    def updated(i: Int, j: Int, e: Expr): CMatrix = {
      val newM = M.updated(i, M(i).updated(j, e))
      CMatrix(newM)
    }
    def updated(i: Int, row: Vector[Expr]) = CMatrix(M.updated(i, row))
    def apply(i: Int) = M(i)

    /* Swap row i and j*/
    def swap(i: Int, j: Int): CMatrix = {
      val temp = M(i)
      CMatrix(M.updated(i, M(j)).updated(j, temp))
    }
    def length = M.length
  }
  val zero = Lit(GRational(0))
  type Matrix = Vector[Vector[Expr]]
  def init(es: List[Equation], q: List[Var]) = {
    equationsToMatrix(es, q)
  }

  // (x,y,z) = (1,2,3) => x = 1, y = 2, z = 3
  def vectorEquations(lhs: Expr, rhs: Expr): List[Equation] = {
    (lhs, rhs) match {
      case (ExprVector(ls1), ExprVector(ls2)) =>
        (ls1 zip ls2).foldLeft(List[Equation]())((r, x) =>
          vectorEquations(x._1, x._2) ::: r)
      case (_, _) => Equation(lhs, rhs) :: Nil
    }
  }
  /* Transform a set of equations into a coefficient matrix B and rhs array.
  *  Requriment(IMPORTANT): the lhs of any input equation should be in the form of
  *  Op("+", c1*x1 :: c2*x2 :: ... cn*xn :: Nil)
  */
  def equationsToMatrix(es: List[Equation], q: List[Var]): (CMatrix, Vector[Expr]) = {
    // Size of Matrix N * N
    val N = es.length
    if (N != q.length)
      throw Errors.GEVarsNotEqualToEquations(es, q)
    // Initialize zero coefficient matrix and rhs array
    val B: Vector[Expr] = fill[Expr](N)(zero).toVector
    val M: CMatrix = CMatrix(fill[Vector[Expr]](N)(B).toVector)
    // Update coefficient
    (0 until N).foldLeft((M, B)) {
      case ((newM, newB), i) =>
        (0 until N).foldLeft((newM, newB.updated(i, es(i).rhs))) {
          case ((rM, rB), j) =>
            val coefs = getCoef(es(i).lhs, q(j))
            (coefs.length > 0) match {
              case true => (rM.updated(i, j, mkPlus(coefs)), rB)
              case _    => (rM, rB)
            }
        }
    }

  }
  def printMatrix(matrix:CMatrix) = {
    println("[")
    matrix.M.map(x => {print("[") ;x.map(y => print(pprint(y)+" , ")) ; print("]");println("")})
    println("]")
  }
  /* Main GE algorithm
   * @param es input DAEs; q input variables to be directed
   * @param hashMap input environment with known var -> expr pair
   * @return Directed ODEs
   *  */
  def run(es: List[Equation], q: List[Var], hashMap: Map[Expr, Expr]): List[Equation] = {
    def findVars(e: Expr): List[Var] = Specialization.findVars(e, hashMap)(Nil)
    def hasTrueVariable(e: Expr) = (findVars(e).toSet intersect q.toSet).size > 0
    // Break an expression into a list of basic terms, which are either constant or constant * variable
    def breakExpr(exp: Expr, vars: List[Var]): List[Expr] = exp match {
      case Lit(_) => List(exp)
      case Var(n) =>
        if (hashMap.contains(Var(n))) {
          val hashExpr = hashMap(Var(n))
          if (hasTrueVariable(hashExpr))
            breakExpr(hashExpr, vars)
          else
            List(exp)
        } else
          List(exp)
      case Dot(e1, e2) => List(exp)
      // Return x(0) as an expression 
      case Index(_, _) => List(exp)
      case Op(f, es) =>
        f.x match {
          case "+" => es.foldLeft(List[Expr]())((r, x) => r ::: breakExpr(x, vars))
          case "-" => breakExpr(es(0), vars) :::
            es.drop(1).foldLeft(List[Expr]())((r, x) => r :::
              breakExpr(mkOp("*", Lit(GRational(-1)), x), vars))
          case "*" => es match {
            case e1 :: e2 :: Nil => (hasTrueVariable(e1), hasTrueVariable(e2)) match {
              case (true, false)  => breakExpr(e1, vars).map(x => mkOp("*", x, e2))
              case (false, true)  => breakExpr(e2, vars).map(x => mkOp("*", x, e1))
              case (false, false) => List(exp)
              case (true, true)   => error("Non-linear equations")
            }

            // 1 * 2 * x case 
            case _ => error("Non-binary operator" + pprint(Op(f, es).asInstanceOf[Expr]))
          }
          case "/" => es match {
            case e1 :: e2 :: Nil => breakExpr(e1, vars).map(x => mkOp("/", x, e2))
          }
          case _ => List(exp)
        }
    }

    // Example: 2 * 3 * x => 6 * x
    def evalVariableTerm(exp: Expr): (Expr, Expr) = {
      //implicit val exceptVars = (List.empty)
      exp match {
        case Var(n) => (Lit(GRational(1)), Var(n))
        case Op(f, es) => f.x match {
          case "*" => es match {
            case el :: er :: Nil =>
              if (findVars(el).length > 0 && findVars(er).length == 0) {
                (evalConstant(mkOp("*", er, evalVariableTerm(el)._1)),
                  evalVariableTerm(el)._2)

              } else if (findVars(er).length > 0 && findVars(el).length == 0) {
                (evalConstant(mkOp("*", el, evalVariableTerm(er)._1)),
                  evalVariableTerm(er)._2)

              } else {
                val lhs = evalVariableTerm(el)
                val rhs = evalVariableTerm(er)
                (evalConstant(mkOp("*", lhs._1, rhs._1)),
                  mkOp("*", lhs._2, rhs._2))
              }
          }
          case _ => (Lit(GRational(1)), exp)
        }
        case Index(_, _) => (Lit(GRational(1)), exp)
        case _           => error(exp.toString + " is not a basic term")
      }
    }
    // Simplify a constant expression
    def evalConstant(exp: Expr): Expr = exp match {
      case _                  => exp
    }

    /* Example 2 * (3*x) => (2*3, x)*/
    def evalTrueVarTerm(exp: Expr, trueVar: Var): (Expr, Var) = {
      exp match {
        case Var(trueVar.name) => (Lit(GRational(1)), trueVar)
        case Op(Name("*", 0), e1 :: e2 :: Nil) => (e1, e2) match {
          case (Var(trueVar.name), _) => (e2, trueVar)
          case (_, Var(trueVar.name)) => (e1, trueVar)
          case _ =>
            if (findVars(e1).contains(trueVar)) {
              val runE1 = evalTrueVarTerm(e1, trueVar)
              (mkOp("*", runE1._1, e2),
                runE1._2)

            } else {
              val runE2 = evalTrueVarTerm(e2, trueVar)
              (mkOp("*", runE2._1, e1),
                runE2._2)

            }
        }
        case Op(Name("/", 0), e1 :: e2 :: Nil) =>
          if (findVars(e2).contains(trueVar))
            throw error("Variable to be solved:" + trueVar + "appears in denominator")
          else {
            val runE1 = evalTrueVarTerm(e1, trueVar)
            (mkOp("/", runE1._1, e2), runE1._2)
          }
      }

    }
    /* Divide an expr into a list of variable terms and a constant */
    def normalizeExpr(e: Expr, q: List[Var]): (Option[List[Expr]], Expr) = {
      val terms = breakExpr(e, q)
      // Find all the constants terms
      val constants = terms.filter(x => findVars(x).length == 0)
      val varTerms = terms.filter(x => findVars(x).length > 0).map(x =>
        mkOp("*", evalVariableTerm(x)._1, evalVariableTerm(x)._2))
      // Terms contain true variables
      val trueVarTerms = varTerms.filter(x => findVars(x).exists(y => q.contains(y)))
      val constVarTerms = varTerms.filterNot(x => findVars(x).exists(y => q.contains(y)))
      val trueVars = trueVarTerms.map(x =>
        evalTrueVarTerm(x,
          findVars(x).find(y => q.contains(y)).get))
      val evaledConstants = constants.map(evalConstant(_))
      val finalConst = evaledConstants.length > 0 match {
        case true => evalConstant(evaledConstants.drop(1).foldLeft(evaledConstants(0))((r, x) =>
          Op(Name("+", 0), r :: x :: Nil)))
        case _ => Lit(GRational(0))
      }
      val finalVarTerms = varTerms.length > 0 match {
        case true => Some(trueVars.map(x => mkOp("*", x._1, x._2)))
        case _    => None
      }

      if (constVarTerms.length > 0) {
        (finalVarTerms, mkOp("+", combineConstVarTerms(constVarTerms), finalConst))
      } else
        (finalVarTerms, finalConst)
    }
    /* Nomalize an arbitray equation  */
    def normalizeEquation(e: Equation, q: List[Var]): Equation = {
      (normalizeExpr(e.lhs, q), normalizeExpr(e.rhs, q)) match {
        case ((Some(vs), cl), (None, cr)) =>
          Equation(mkPlus(vs),
            mkOp("-", cr, cl))
        case ((None, cr), (Some(vs), cl)) =>
          Equation(mkPlus(vs),
            mkOp("-", cr, cl))
        case ((Some(vs), cl), (Some(vs2), cr)) =>
          Equation(combineConstVarTerms(vs ::: vs2.map(x => mkOp("*", Lit(GRational(-1)), x))),
            mkOp("-", cr, cl))
      }
    }
    // Test whether exp has any variables from vars in it
    def hasVar(exp: Expr, vars: List[Var]): Boolean = {
      findVars(exp).exists(x => vars.contains(x))
    }
    def swap(B:Vector[Expr],i:Int, j:Int):Vector[Expr] = {
      val temp = B(i);
      B.updated(i, B(j)).updated(j, temp)
    }
    // Main algorithm
    def run (initialM : CMatrix, initialB: Vector[Expr]): (CMatrix,Vector[Expr]) = {
      val N = initialM.length
      // Outer loop
      (0 until N).foldLeft((initialM,initialB)){case ((outerM,outerB),p) =>
        // Find pivot row and swap
        val pivot = (p+1 until N).foldLeft(p){case (max,pivoti) => 
          if ((outerM(pivoti)(p).isInstanceOf[Lit] && !isZero(outerM(pivoti)(p))) || isZero(outerM(max)(p)))
            pivoti
          else
            max  
            }
          val swapedM = outerM.swap(p, pivot)
          val swapedB = swap(outerB,p,pivot)
          // Check singularity here
          if (isZero(swapedM(p)(p))) {
            sys.error("Matrix is singular can't be solved")
          }
          // Normalization
          val NB = swapedB.updated(p, mkOp("/", swapedB(p), swapedM(p)(p)))
          val NM = swapedM.updated(p, normalize(swapedM(p)(p), swapedM(p)))
          // Normalize every elements 
          val IMB = (p+1 until N).foldLeft((NM,NB)){case ((innerM,innerB),i) =>
            val alpha = mkOp("/", innerM(i)(p), innerM(p)(p))
            val IB = innerB.updated(i, mkOp("-", innerB(i), mkOp("*", alpha, innerB(p))))
            val Mupdated = (p until N).foldLeft(innerM){case (r,j) =>
              if (j == p)
                r.updated(i, j, zero)
              else
               r.updated(i, j, mkOp("-", r(i)(j), mkOp("*", alpha, r(p)(j))))
              }
            (Mupdated,IB)
          }
         IMB
      }      
    }
    
     val ses = es.map(e =>
      vectorEquations(e.lhs, e.rhs)).flatten.map(normalizeEquation(_, q))
    // coefficent matrix and rhs column
    val MBInit = equationsToMatrix(ses, q)
    val MB = run(MBInit._1,MBInit._2)
    val M = MB._1; val B = MB._2
    val N = M.length
    // Back substitution
    val xInit = new Array[Expr](N).toVector
    val x = ( N - 1 to 0 by -1).toList.foldLeft(xInit) { case(xr,i) =>
      val newRhs = (i + 1 until N).foldLeft(zero: Expr)((r, j) =>
        mkOp("+", mkOp("*", M(i)(j), xr(j)), r))
      xr.updated(i, mkOp("-", B(i), newRhs))
    }
    // Output equations
    val result = (0 until N).toList.foldLeft(List[Equation]())((r, i) =>
      Equation(q(i), x(i)) :: r)
    result.reverse
  }

  def combineConstVarTerms(varTerms: List[Expr]): Expr = {
    mkPlus(varTerms)
  }
  /* Example: 3*x => 3 */
  def getCoef(e: Expr, v: Var): List[Expr] = e match {
    case Op(Name("*", 0), v1 :: v2 :: Nil) => (getCoef(v1, v), getCoef(v2, v)) match {
      case (Nil, l) => l.map(x => mkTimes(v1 :: x :: Nil))
      case (l, Nil) => l.map(x => mkTimes(v2 :: x :: Nil))
    }
    case v1: Var              => { if (v1.name == v.name) List(Lit(GRational(1))); else Nil }
    case Op(Name("+", 0), es) => es.foldLeft(List[Expr]())((r, x) => r ::: getCoef(x, v))
    case _                    => Nil
  }

  /* A helper function to allow us to write Op("+", ...) instead of Op(Nmae("+",0),...) */
  implicit def stringToName(op: String) = Name(op, 0)

  /* Length measure for pivoting:
   * In the case of numerical stability, the largest (in absolute value)
   * is chosen, whereas for coefficient growth cases, it is the smallest
   * (according to some given size metric) that is chosen.
   * Here we choose the number of symbolic terms as a length measure*/
  def length(e: Expr): Int = e match {
    case Op(_, es) => es.length
    case _         => 1
  }

  /* A superfical way for checking zero entry
   * Todo: What about l*sin(t), where it might becomes zero at certain time?*/
  def isZero(e: Expr) = e match {
    case Lit(GRational(Rational.zero)) => true
    case _                             => false
  }

  def mkBinOp(o: String, xs: List[Expr]) = {
    xs.drop(1).foldLeft(xs(0))((r, x) => mkOp(o, r, x))
  }

  def normalize(mpp: Expr, mp: Vector[Expr]): Vector[Expr] = {
    (0 until mp.length).map(i => mkOp("/", mp(i), mpp)).toVector
  }

  /* Example: (x,2,3,y)  => ((x + 2) + 3) + y*/
  def mkPlus(es: List[Expr]): Expr = es match {
    case Nil             => Lit(GRational(0))
    case e :: Nil        => e
    case e1 :: e2 :: Nil => mkOp("+", e1, e2)
    case _ => es.drop(1).foldLeft(es(0))((r, x) =>
      mkOp("+", r, x))
  }
  def mkTimes(es: List[Expr]): Expr = es match {
    case e :: Nil => e
    case _ => es.drop(1).foldLeft(es(0))((r, x) =>
      mkOp("*", r, x))
  }

}

