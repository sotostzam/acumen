package acumen.interpreters.enclosure.taylor

import scala.collection.mutable.ArrayBuffer
import acumen.interpreters.enclosure.Box
import acumen.interpreters.enclosure.Constant
import acumen.interpreters.enclosure.Cos
import acumen.interpreters.enclosure.Divide
import acumen.interpreters.enclosure.Exp
import acumen.interpreters.enclosure.Expression
import acumen.interpreters.enclosure.Field
import acumen.interpreters.enclosure.Interval
import acumen.interpreters.enclosure.Log
import acumen.interpreters.enclosure.Multiply
import acumen.interpreters.enclosure.Negate
import acumen.interpreters.enclosure.Plus
import acumen.interpreters.enclosure.Sin
import acumen.interpreters.enclosure.Sqrt
import acumen.interpreters.enclosure.Variable
import acumen.interpreters.enclosure.Parameters

trait TaylorMethod {

  /**
   * Taylor method for the ODE-IVP x' = f(x), x(0) = b with time step h.
   *
   * Note: See "Introduction to Automatic Differentiation" by Rall and Corliss, pp 7.
   *
   * TODO: assuming each call within a single simulation is with the same d the
   * map allocated inside should be re-used!
   */
  def taylorMethod(f: Field, d: Int, b: Box, h: Interval) = {
    val polyTable = taylorTable(f, d, b, h)
    //    val restTable = taylorTable(f, d + 1, b.mapValues(_ => h /\ 0), h)
    val polyValue = Box.toBox(f.components.map {
      case (variable, _) =>
        variable -> polyTable(variable).reverse.tail.fold(Interval.zero)(_ + _)
    })
    //    val restValue = Box.toBox(f.components.map {
    //      case (variable, _) =>
    //        variable -> table(variable).last
    //    })
    polyValue
  }

  /**
   * FIXME add description.
   */
  def taylorTable(f: Field, d: Int, b: Box, h: Interval) = {
    val table = new scala.collection.mutable.HashMap[Expression, scala.collection.mutable.ArrayBuffer[Interval]]
    val variables = f.components.keys.map(Variable(_))
    for (variable <- variables) table += variable -> (new scala.collection.mutable.ArrayBuffer() += b(variable.name))
    val expressions = codeList(f.components.values.toList: _*)
    for (e <- expressions) table += e -> (new scala.collection.mutable.ArrayBuffer() += (e match {
      case Constant(v) => v
      case _           => e(b)
    }))
    def updateTable(e: Expression) = {
      def multiply(ls: ArrayBuffer[Interval], rs: ArrayBuffer[Interval], k: Int) = {
        var res = ls(0) * rs(k)
        for (j <- 1 to k) res += ls(j) * rs(k - j)
        res
      }
      table(e) = e match {
        case Constant(v) => table(e) += Interval.zero
        case Variable(n) => table(e)
        case Negate(e)   => table(e).map(-(_))
        case Sqrt(e)     => sys.error("undefined")
        case Exp(e)      => sys.error("undefined")
        case Log(e)      => sys.error("undefined")
        case Sin(e)      => sys.error("undefined")
        case Cos(e)      => sys.error("undefined")
        case Plus(l, r)  => (table(l) zip table(r)).map { case (l, r) => l + r }
        case Multiply(l, r) =>
          val ls = table(l)
          val rs = table(r)
          var res = new scala.collection.mutable.ArrayBuffer[Interval]()
          for (k <- 0 until ls.length) res += multiply(ls, rs, k)
          res
        case _ => sys.error("operator " + e.getClass + " not supported!")
      }
    }
    for (_ <- 0 until d) {
      for (variable <- variables) {
        val component = table(f.components(variable.name))
        table(variable) += (component.last * h / component.size)
      }
      expressions.map(updateTable)
    }
    table
  }

  /**
   * The d+1 first terms of the Taylor expansion of `e` at `b` in the direction `h`.
   *
   * Note: See "Introduction to Automatic Differentiation" by Rall and Corliss, pp. 4-5.
   */
  def taylorTerms(e: Expression, d: Int, b: Box, h: Box): Array[Interval] = {
    require(e.varNames.forall(b.keySet.contains(_)), "each variable in " + e + " must have have a domain in " + b)
    val table = new scala.collection.mutable.HashMap[Expression, Array[Interval]]()
    def addToTable(x: Expression) = {
      def multiply(ls: Array[Interval], rs: Array[Interval], k: Int) = {
        var res = ls(0) * rs(k)
        for (j <- 1 to k) res += ls(j) * rs(k - j)
        res
      }
      def divide(ls: Array[Interval], rs: Array[Interval], d: Int) = {
        var res = (Array.fill(d + 1)(1 / rs(0)))
        for (k <- 0 to d) {
          var rk = ls(k)
          for (j <- 1 to k) rk -= rs(j) * res(k - j)
          res(k) *= rk
        }
        res
      }
      x match {
        case Constant(v)    => table += x -> (v :: (1 to d).toList.map(_ => Interval.zero)).toArray
        case Variable(n)    => table += x -> (b(n) :: h(n) :: (1 until d).toList.map(_ => Interval.zero)).toArray
        case Negate(e)      => table += x -> table(e).map(-(_))
        case Sqrt(e)        => sys.error("undefined")
        case Exp(e)         => sys.error("undefined")
        case Log(e)         => sys.error("undefined")
        case Sin(e)         => sys.error("undefined")
        case Cos(e)         => sys.error("undefined")
        case Plus(l, r)     => table += x -> (table(l) zip table(r)).map { case (l, r) => l + r }
        case Multiply(l, r) => table += x -> (0 to d).map(k => multiply(table(l), table(r), k)).toArray
        case Divide(l, r)   => table += x -> divide(table(l), table(r), d)
      }
    }
    codeList(e).map(addToTable)
    table(e)
  }

  /**
   * Produces a code list for an Expression.
   */
  def codeList(es: Expression*): List[Expression] = {
    val ctx = scala.collection.mutable.MutableList[Expression]()
    def mem(x: Expression) = if (ctx contains x) x else { ctx += x; x }
    def memoise(e: Expression): Expression = e match {
      case Constant(_) | Variable(_) => mem(e)
      case Negate(e)                 => mem(Negate(memoise(e)))
      case Sqrt(e)                   => mem(Sqrt(memoise(e)))
      case Exp(e)                    => mem(Exp(memoise(e)))
      case Plus(l, r)                => mem(Plus(memoise(l), memoise(r)))
      case Multiply(l, r)            => mem(Multiply(memoise(l), memoise(r)))
      case Divide(l, r)              => mem(Divide(memoise(l), memoise(r)))
      case _                         => sys.error("memoisation of " + e + " not supported!")
    }
    es.map(memoise(_))
    ctx.toList
  }

}

object TaylorMethodApp extends App with TaylorMethod {

  implicit val rnd = Parameters.default.rnd

  val f = Field(Map("" -> Variable("")))
  val d = 10
  val b = Box("" -> Interval.one)

  println(taylorMethod(f, d, b, 1)("")) // FIXME account for truncation error in taylorMethod 

}
