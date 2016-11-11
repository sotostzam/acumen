package acumen

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.Gen
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck.Properties
import testutil.Generators._
import Pretty._
import SD._
import interpreters.reference2015.Interpreter._
import interpreters.Common._
import util.Canonical._
import interpreters.Common.Env
import scala.compat.Platform

object SDTest extends Properties("SD") {
  // How big the arbitrary expression is 
  var N = 100

  val epsilon = Math.pow(10, -10)
  val ctx2 = Map[Expr, Expr]()
  val ctx = scala.collection.mutable.HashMap[Expr, Expr]()
  val id = CId(0)
  val env = emptyEnv + (self, VObjId(Some(CId(0))))
  val cobject = Map(Name("x", 0) -> VLit(GInt(1)), Name("y", 0) -> VLit(GInt(1)))
  val st = emptyStore + (id -> cobject)
  def mkOp(o: String, xs: Expr*): Expr =
    {
      val e = Op(Name(o, 0), xs.toList)
      ctx.get(e) match {
        case Some(e) => e
        case _       => ctx += (e -> e); e
      }
    }
  val x = Var(Name("x", 0)); val y = Var(Name("y", 0))
  // Make a copy of each identity function 10 times
  def duplicate[T](list: List[T], N: Int): List[T] =
    list.flatMap(x => for (i <- 0 to N) yield x)

  def oriIdentityFunctions(x: Expr): List[Expr] =
    x ::
      mkOp("^", x, x) ::
      mkOp("asin", mkOp("sin", x)) :: // This helps me find out "asin" is not implemented in PD
      mkOp("acos", mkOp("cos", x)) ::
      mkOp("atan", mkOp("tan", x)) :: // This helps me find out "atan" is not implemented in PD
      mkOp("sqrt", mkOp("^", x, Lit(GRational(2)))) ::
      mkOp("/", mkOp("^", x, Lit(GRational(2))), x) ::
      mkOp("/", mkOp("*", x, mkOp("exp", x)), mkOp("exp", x)) ::
      mkOp("log", mkOp("exp", x)) ::
      mkOp("-", mkOp("*", x, Lit(GRational(2))), x) ::
      Nil
  val ops: List[(String, String)] = ("*", "/") :: ("+", "-") :: ("*", "/") :: Nil

  def identityFunctions(x: Expr) = duplicate(oriIdentityFunctions(x), N)


  case class DoubleSimilar(val v: Double) {
    def ~=(l: DoubleSimilar) =
      Math.abs(v - l.v) < epsilon
  }

  implicit def CValueToSimilar(x: CValue): DoubleSimilar = DoubleSimilar(unwrap(x))
  implicit def DoubleToSimilar(x: Double): DoubleSimilar = DoubleSimilar(x)

  def printExpr(expr: Option[Expr]) = expr match {
    case Some(e) => println(pprint(e))
    case None    =>
  }

  /*
   * This property helps me find the bug with division, x/x, which instead of
   * giving ../x^2 gives an incorrect one .../2*x
   * */

  property("PD on addition of identity functions") = {
    forAll(mkPlusExpr(x)) { (e: Expr) =>
      val time1 = Platform.currentTime
      val (expr, newbindings) = dif(e, x.name, ctx2, Nil).pair
      val bindings = mkBindings(newbindings)
      var dexpr = mkDot(expr)   
      val evalResult = evalExpr(dexpr, env, st)(bindings)
      SD.clear()
      // Property
      evalResult ~= N
    }
  }

  // PD of an identify function should be 1
  property("PD on arithmatic of identity functions") = {
    forAll(fx(x)) { (e: Expr) =>
      val (expr, newbindings) = dif(e, x.name, ctx2, Nil).pair
      val bindings = mkBindings(newbindings)
      var dexpr = mkDot(expr)
      val evalResult = evalExpr(dexpr, env, st)(bindings)      
      // Property
      SD.clear()
      evalResult ~= 1
    }
  }
  /*(fx + fy)'[x] ~= (fx + fy)'[y] ~= 1 */
  property("PD on two demensional functions") =
    forAll(genFxPlusFy) { (e: Expr) =>
      val (px, xbindings) = dif(e, x.name, ctx2, Nil).pair
      val (py, ybindings) = dif(e, y.name, ctx2, Nil).pair
      var dpx = mkDot(px)
      var dpy = mkDot(py)
      val bindings = mkBindings(xbindings ::: ybindings)
      val evalpx = evalExpr(dpx, env, st)(bindings)
      val evalpy = evalExpr(dpy, env, st)(bindings)
      // Property
      (evalpx ~= evalpy) && (evalpx ~= 1)
    }

  property("PD on two demensional functions") =
    forAll(genFxTimesFy) { (e: Expr) =>
      val (px, xbindings) = dif(e, x.name, ctx2, Nil).pair
      val (py, ybindings) = dif(e, y.name, ctx2, Nil).pair
      var dpx = mkDot(px)
      var dpy = mkDot(py)
      val bindings = mkBindings(xbindings ::: ybindings)
      val evalpx = evalExpr(dpx, env, st)(bindings)
      val evalpy = evalExpr(dpy, env, st)(bindings)
      // Property
      (evalpx ~= evalpy) && (evalpx ~= 1)
    }
  property("PD on two demensional functions") =
    forAll(genFxTimesFy) { (e: Expr) =>
      val (px, xbindings) = dif(e, x.name, ctx2, Nil).pair
      val (py, ybindings) = dif(e, y.name, ctx2, Nil).pair
      var dpx = mkDot(px)
      var dpy = mkDot(py)
      val bindings = mkBindings(xbindings ::: ybindings)
      val evalpx = evalExpr(dpx, env, st)(bindings)
      val evalpy = evalExpr(dpy, env, st)(bindings)
      // Property
      (evalpx ~= evalpy) && (evalpx ~= 1)
    }
  /* Generators */
  def genExprSeq(x: Expr): Gen[Seq[Expr]] =
    pick(N, identityFunctions(x))

  def genFxPlusFy: Gen[Expr] =
    for { x <- fx(x); y <- fx(y) } yield mkOp("+", x, y)
  def genFxTimesFy: Gen[Expr] =
    for { x <- fx(x); y <- fx(y) } yield mkOp("*", x, y)

  def mkPlusExpr(x: Expr): Gen[Expr] =
    for {
      es <- genExprSeq(x)
    } yield mkBalancedExpr(es.toList)

  def mkBalancedExpr(es: List[Expr]): Expr = {
    if (es.length <= 1)
      es(0)
    else {
      val (les, res) = es.splitAt(es.length / 2)
      val (l, r) = (mkBalancedExpr(les), mkBalancedExpr(res))
      mkOp("+", l, r)
    }

  }
  def fx(x: Expr): Gen[Expr] = {
    val opPool = duplicate(ops, N / 2)
    val paris = for {
      es <- genExprSeq(x)
      (l1, l2) <- es.splitAt(es.length / 2)
    } yield l1 zip l2
    for {
      pairExprs <- paris
      opss <- pick(N, opPool)
    } yield mkOp(pairExprs, opss, x)
  }
  def mkDot(e: Expr): Expr = e match {
    case Var(n)    => Dot(Var(self), n)
    case Op(f, es) => Op(f, es map mkDot)
    case _         => e
  }
  def unwrap(v: CValue): Double = v match {
    case VLit(GInt(n))    => n.toDouble
    case VLit(GDouble(n)) => n
  }
  def mkOp(exprs: Seq[(Expr, Expr)], ops: Seq[(String, String)], init: Expr): Expr = {
    var result: Expr = init
    for (i <- 0 to exprs.length - 1) {
      result = mkOp(ops(i)._2, mkOp(ops(i)._1, result, exprs(i)._1),
        exprs(i)._2)
    }
    result
  }

  def mkBindings(eqs: List[Action]): Bindings = {
    eqs.map(a => a match {
      case Continuously(a @ Equation(lhs, rhs)) => lhs match {
        case Var(n) => ((id, n, Nil), UnusedBinding(mkDot(rhs), env))
      }
    }).toMap
  }

}