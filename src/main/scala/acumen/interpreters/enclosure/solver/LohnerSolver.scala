package acumen.interpreters.enclosure.solver

import acumen.interpreters.enclosure.Box
import acumen.interpreters.enclosure.Field
import acumen.interpreters.enclosure.Interval
import acumen.interpreters.enclosure.Rounding
import acumen.interpreters.enclosure.Types.{ VarName }
import acumen.interpreters.enclosure.affine.UnivariateAffineEnclosure
import acumen.interpreters.enclosure.Expression
import acumen.interpreters.enclosure.Variable
import acumen.interpreters.enclosure.Constant
import acumen.interpreters.enclosure.Multiply
import acumen.interpreters.enclosure.Negate
import acumen.interpreters.enclosure.Plus

trait LohnerSolver extends SolveIVP {

  /**
   * TODO add description!
   */
  def solveVt(
    F: Field, // field
    T: Interval, // domain of t
    A: Box, // (A1,...,An), initial condition
    delta: Double, // padding 
    m: Int, // extra iterations after inclusion of iterates
    n: Int, // maximum number of iterations before inclusion of iterates
    degree: Int // number of pieces to split each initial condition interval
    )(implicit rnd: Rounding): (UnivariateAffineEnclosure, Box) = null // TODO unimplemented method

  /**
   * The d+1 first terms of the Taylor expansion of `e` at `b` in the direction `h`.
   *
   * Note: See "Introduction to Automatic Differentiation" by Rall and Corliss, pp. 4-5.
   */
  def taylorMethod(e: Expression, d: Int, b: Box, h: Box)(implicit rnd: Rounding): Array[Interval] = {
    require(e.varNames.forall(b.keySet.contains(_)), "each variable in " + e + " must have have a domain in " + b)
    val table = new scala.collection.mutable.HashMap[Expression, Array[Interval]]()
    def addToTable(x: Expression) = {
      def multiply(ls: Array[Interval], rs: Array[Interval], k: Int) = {
        var res = ls(0) * rs(k)
        for (j <- 1 to k) res += ls(j) * rs(k - j)
        res
      }
      x match {
        case Constant(v)    => table += x -> (v :: (1 to d).toList.map(_ => Interval(0))).toArray
        case Variable(n)    => table += x -> (b(n) :: h(n) :: (1 until d).toList.map(_ => Interval(0))).toArray
        case Plus(l, r)     => table += x -> (table(l) zip table(r)).map { case (l, r) => l + r }
        case Multiply(l, r) => table += x -> (0 to d).map(k => multiply(table(l), table(r), k)).toArray
      }
    }
    codeList(e).map(addToTable)
    table(e)
  }

  /**
   * Produces a code list for the an Expression.
   */
  def codeList(e: Expression): List[Expression] = {
    val ctx = scala.collection.mutable.MutableList[Expression]()
    def mem(x: Expression) = if (ctx contains x) x else { ctx += x; x }
    def memoise(e: Expression): Expression = e match {
      case Constant(_) | Variable(_) => mem(e)
      case Negate(e)                 => mem(Negate(memoise(e)))
      case Plus(l, r)                => mem(Plus(memoise(l), memoise(r)))
      case Multiply(l, r)            => mem(Multiply(memoise(l), memoise(r)))
      case _                         => sys.error("memoisation of " + e + " not supported!")
    }
    memoise(e)
    ctx.toList
  }

}

object LohnerSolverApp extends LohnerSolver with App {

  implicit val rnd = Rounding(10)
  val x = Variable("x")
  val y = Variable("y")
  val z = Variable("z")
  val e = -10

  println(taylorMethod(e, 5, Box("x" -> 0, "y" -> 0, "z" -> 0), Box("x" -> 1, "y" -> 2, "z" -> 3)).toList)

}
