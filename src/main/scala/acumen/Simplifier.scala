package acumen

import Constants._
import Errors._
import util.Names._
import spire.math.Rational

object Simplifier {
  
  private val intervalMidpointSimplifier = new util.ASTMap {
    override def mapExpr(e: Expr) : Expr = super.mapExpr(e match {
      case ExprInterval(lo, hi) => Op(Name("/",0),List(Op(Name("+",0), List(lo, hi)), Lit(GRational(Rational(2)))))
      case ExprIntervalM(mid, _) => mid
      case expr => expr})
    def run(t: Prog): Prog = mapProg(t)
  }
  
  /** Remove Intervals and replace them with the mid point. */
  def replaceIntervalsByMidpoints(t: Prog): Prog = intervalMidpointSimplifier.run(t)

  // Smart construct 
  def mkOp(o: String, xs: Expr*): Expr = {
    val zero = Lit(GRational(0)); val two = Lit(GRational(2))
    val one = Lit(GRational(1)); val half = Lit(GRational(0.5))
    (o, xs.toList) match {
      case ("+", Lit(GRational(n)) :: Lit(GRational(m)) :: Nil)              => Lit(GRational(n + m))
      case ("-", Lit(GRational(n)) :: Lit(GRational(m)) :: Nil)              => Lit(GRational(n - m))
      case ("*", Lit(GRational(n)) :: Lit(GRational(m)) :: Nil)              => Lit(GRational(n * m))
      case ("/", Lit(GRational(n)) :: Lit(GRational(m)) :: Nil)              => Lit(GRational(n / m))
      case ("^", Lit(GRational(n)) :: Lit(GRational(m)) :: Nil) if m.isWhole => Lit(GRational(n pow m.toInt))
      case ("*", x :: y :: Nil) if ~=(x, y)                                  => mkOp("^", x, two)
      case ("*", Lit(GRational(Rational.zero)) :: x :: Nil)                  => zero
      case ("*", x :: Lit(GRational(Rational.zero)) :: Nil)                  => zero
      case ("*", Lit(GRational(Rational.one)) :: x :: Nil)                   => x
      case ("*", x :: Lit(GRational(Rational.one)) :: Nil)                   => x
      case ("*", Lit(GRational(n)) :: Op(Name("*", 0), Lit(GRational(m)) :: x :: Nil) :: Nil) =>
        mkOp("*", Lit(GRational(n * m)), x)
      case ("*", Lit(n) :: Op(Name("-", 0), x :: y :: Nil) :: Nil) if n == GRational(-1) =>
        mkOp("-", y, x)
      case ("^", Lit(GRational(Rational.zero)) :: y :: Nil) => zero
      case ("^", x :: Lit(GRational(Rational.one)) :: Nil) => x
      case ("/", x :: Lit(GRational(Rational.one)) :: Nil) => x
      case ("/", Lit(GRational(Rational.one)) :: Op(Name("/", 0), y :: z :: Nil) :: Nil) => mkOp("/", z, y)
      case ("/", x :: y :: Nil) if x == y => one
      case ("/", Lit(GRational(Rational.zero)) :: x :: Nil) => zero
      case ("-", x :: Lit(GRational(Rational.zero)) :: Nil) => x
      case ("-", a :: b:: Nil) if ~=(a,b) => zero
      case ("-", Lit(GRational(Rational.zero)) :: Op(Name("*", 0), Lit(GRational(m)) :: x :: Nil) :: Nil) =>
        mkOp("*", Lit(GRational(-m)), x)
      case ("-", Lit(GRational(Rational.zero)) :: Op(Name("*", 0), x :: Lit(GRational(m)) :: Nil) :: Nil) =>
        mkOp("*", Lit(GRational(-m)), x)
      case ("/", Lit(GRational(n)) :: Op(Name("*", 0), Lit(GRational(m)) :: x :: Nil) :: Nil) =>
        mkOp("/", Lit(GRational(n / m)), x)
      case ("/", Lit(GRational(n)) :: Op(Name("*", 0), x :: Lit(GRational(m)) :: Nil) :: Nil) =>
        mkOp("/", Lit(GRational(n / m)), x)
      case ("*", Lit(GRational(n)) :: Op(Name("/", 0), Lit(GRational(m)) :: x :: Nil) :: Nil) =>
        mkOp("/", Lit(GRational(n * m)), x)
      case ("*", Lit(GRational(n)) :: Op(Name("^", 0),  x :: Lit(GRational(m)) :: Nil) :: Nil) if n == Rational(-1) && m == two =>
        mkOp("^", x, two)
      case ("*", Lit(GRational(n)) :: Op(Name("/", 0), x :: Lit(GRational(m)) :: Nil) :: Nil) =>
        mkOp("*", Lit(GRational(n / m)), x)
      case ("+", x :: Lit(GRational(Rational.zero)) :: Nil) => x
      case ("*", x :: Lit(GRational(n)) :: Nil)             => Op(Name("*", 0), Lit(GRational(n)) :: x :: Nil)
      case ("+", Lit(GRational(Rational.zero)) :: x :: Nil) => x
      case ("-", Op(Name("*", 0), Lit(GRational(n)) :: x :: Nil) :: y :: Nil) if x == y  => mkOp("*", Lit(GRational(n-1)), x)
      case ("-", Op(Name("*", 0), Lit(GRational(n)) :: x :: Nil) :: Op(Name("*", 0), Lit(GRational(m)) :: y :: Nil) :: Nil) if x == y  => mkOp("*", Lit(GRational(n-m)), x)
      case ("+", x :: y :: Nil) if x == y                   => mkOp("*", two, x)
      case ("*", Op(Name("*", 0), Lit(GRational(n)) :: a :: Nil) :: b :: Nil) =>
        mkOp("*", Lit(GRational(n)), mkOp("*", a, b))
      case ("-", Op(Name("*", 0), a :: b :: Nil) :: Op(Name("*", 0), c :: d :: Nil) :: Nil) if a == c =>
        mkOp("*", a, mkOp("-", b, d))
      case ("-", Op(Name("*", 0), Lit(GRational(n)) :: b :: Nil) :: Op(Name("*", 0), Lit(GRational(m)) :: d :: Nil) :: Nil) if ~=(b, d) =>
        mkOp("*",Lit(GRational(n-m)) ,b)
      case ("-", Op(Name("*", 0), Lit(GRational(n)) :: b :: Nil) :: d :: Nil) if ~=(b, d) =>
        mkOp("*",Lit(GRational(n-1)) ,b)
      case ("+", Op(Name("-", 0), a :: b :: Nil) :: Op(Name("+", 0), c :: d :: Nil) :: Nil) if b == d =>
        mkOp("+", a, c)
      case ("+", Op(Name("-", 0), a :: b :: Nil) :: Op(Name("-", 0), c :: d :: Nil) :: Nil) if b == c =>
        mkOp("-", a, d)
      case ("-", Op(Name("-", 0), a :: b :: Nil) :: Op(Name("-", 0), c :: d :: Nil) :: Nil) if b == c =>
        mkOp("-", mkOp("+", a, d), mkOp("*", two, b))
      case ("-", Op(Name("*", 0), a :: b :: Nil) :: Op(Name("+", 0), c :: d :: Nil) :: Nil) if b == d =>
        mkOp("-", mkOp("*", mkOp("-", a, one), b), c)
      case ("+", Op(Name("+", 0), c :: d :: Nil) :: Op(Name("-", 0), a :: b :: Nil) :: Nil) if b == d =>
        mkOp("+", c, a)
      case ("+", Op(Name("+", 0), b :: c :: Nil) :: a :: Nil) =>
        val ab = mkOp("+", a, b)
        if (ab != Op(Name("+", 0), a :: b :: Nil))
          mkOp("+", ab, c)
        else {
          val ac = mkOp("+", a, c)
          if (ac != Op(Name("+", 0), a :: c :: Nil))
            mkOp("+", ac, b)
          else
            Op(Name("+", 0), Op(Name("+", 0), b :: c :: Nil) :: a :: Nil)
        }
       case ("-", a :: Op(Name("+", 0), b :: c :: Nil) ::  Nil) =>
        val ab = mkOp("-", a, b)
        if (ab != Op(Name("-", 0), a :: b :: Nil))
          mkOp("-", ab, c)
        else {
          val ac = mkOp("-", a, c)
          if (ac != Op(Name("-", 0), a :: c :: Nil))
            mkOp("-", ac, b)
          else
            Op(Name("-", 0), a :: mkOp("+", b, c) ::  Nil)
        }
    
      case ("*", Op(Name("*", 0), b :: c :: Nil) :: a :: Nil) =>
        val ab = mkOp("*", a, b)
        if (ab != Op(Name("*", 0), a :: b :: Nil))
          mkOp("*", ab, c)
        else {
          val ac = mkOp("*", c, a)
          if (ac != Op(Name("*", 0), c :: a :: Nil))
            mkOp("*", ac, b)
          else
            Op(Name("*", 0), mkOp("*", b, c) :: a :: Nil)
        }
      case ("*", a :: Op(Name("*", 0), b :: c :: Nil) :: Nil) =>
        val ab = mkOp("*", a, b)
        if (ab != Op(Name("*", 0), a :: b :: Nil))
          mkOp("*", ab, c)
        else {
          val ac = mkOp("*", a, c)
          if (ac != Op(Name("*", 0), a :: c :: Nil))
            mkOp("*", ac, b)
          else
            Op(Name("*", 0), a :: mkOp("*", b, c) :: Nil)
        }
      case ("+", Op(Name("*", 0), a :: b :: Nil) :: Op(Name("*", 0), c :: d :: Nil) :: Nil) if a == c =>
        mkOp("*", a, mkOp("+", b, d))
      case ("+", a :: Op(Name("*", 0), Lit(GRational(n)) :: b :: Nil) :: Nil) if a == b =>
        mkOp("*", Lit(GRational(n + 1)), a)
      // Trigonometric identity rule
      case ("*", Op(Name("sin", 0), t1 :: Nil) :: Op(Name("cos", 0), t2 :: Nil) :: Nil) if t1 == t2 =>
        mkOp("*", half, mkOp("sin", mkOp("*", two, t1)))
      case ("*", Op(Name("cos", 0), t2 :: Nil) :: Op(Name("sin", 0), t1 :: Nil) ::  Nil) if t1 == t2 =>
        mkOp("*", half, mkOp("sin", mkOp("*", two, t1)))
      case ("+", Op(Name("^", 0), Op(Name("sin", 0), t1 :: Nil) :: m :: Nil) ::
        Op(Name("^", 0), Op(Name("cos", 0), t2 :: Nil) :: n :: Nil) :: Nil) if t1 == t2 && m == two && n == two => one
      case ("+", Op(Name("^", 0), Op(Name("cos", 0), t1 :: Nil) :: m :: Nil) ::
        Op(Name("^", 0), Op(Name("sin", 0), t2 :: Nil) :: n :: Nil) :: Nil) if t1 == t2 && m == two && n == two => one
      case ("+", Op(Name("*", 0), Op(Name("cos", 0), t1 :: Nil) :: Op(Name("cos", 0), t2 :: Nil) :: Nil) ::
        Op(Name("*", 0), Op(Name("sin", 0), t3 :: Nil) :: Op(Name("sin", 0), t4 :: Nil) :: Nil) :: Nil) if t1 == t3 && t2 == t4 => Op(Name("cos", 0), Op(Name("-", 0), t1 :: t2 :: Nil) :: Nil)
      case ("+", Op(Name("*", 0), Op(Name("sin", 0), t1 :: Nil) :: Op(Name("sin", 0), t2 :: Nil) :: Nil) ::
        Op(Name("*", 0), Op(Name("cos", 0), t3 :: Nil) :: Op(Name("cos", 0), t4 :: Nil) :: Nil) :: Nil) if t1 == t3 && t2 == t4 => Op(Name("cos", 0), Op(Name("-", 0), t1 :: t2 :: Nil) :: Nil)
      case ("*", Op(Name("cos", 0), Op(Name("-", 0), a :: b :: Nil) :: Nil) ::
        Op(Name("cos", 0), Op(Name("-", 0), c :: d :: Nil) :: Nil) :: Nil) if a == d && b == c => mkOp("^", mkOp("cos", mkOp("-", a, b)), two)
       case ("+", 
        Op(Name("cos", 0), Op(Name("+", 0), a :: b :: Nil) :: Nil) ::
        Op(Name("cos", 0), Op(Name("-", 0), c :: d :: Nil) :: Nil) :: Nil) if a == c && b == d => 
          mkOp("*", Lit(GRational(2)),mkOp("*",mkOp("cos", a),mkOp("cos", b)))
      case ("+", 
        Op(Name("cos", 0), Op(Name("-", 0), a :: b :: Nil) :: Nil) ::
        Op(Name("cos", 0), Op(Name("+", 0), c :: d :: Nil) :: Nil) :: Nil) if a == c && b == d => 
          mkOp("*", Lit(GRational(2)),mkOp("*",mkOp("cos", a),mkOp("cos", b)))
      case ("-", 
        Op(Name("cos", 0), Op(Name("-", 0), a :: b :: Nil) :: Nil) ::
        Op(Name("cos", 0), Op(Name("+", 0), c :: d :: Nil) :: Nil) :: Nil) if a == c && b == d => 
          mkOp("*", Lit(GRational(2)),mkOp("*",mkOp("sin", a),mkOp("sin", b)))
       case ("*", 
        Op(Name("cos", 0), Op(Name("-", 0), a :: b :: Nil) :: Nil) ::
        Op(Name("cos", 0), Op(Name("-", 0), c :: d :: Nil) :: Nil) :: Nil) if b == d => 
          mkOp("*", Lit(GRational(0.5)),mkOp("+",mkOp("cos", mkOp("-",c,d)),
              mkOp("cos", mkOp("-", mkOp("-", mkOp("*", two,a),c),a)))
              )
     case ("*", 
        Op(Name("cos", 0), Op(Name("-", 0), a :: b :: Nil) :: Nil) ::
        Op(Name("cos", 0), Op(Name("-", 0), c :: d :: Nil) :: Nil) :: Nil) if a == d => 
          mkOp("*", Lit(GRational(0.5)),mkOp("+",mkOp("cos", mkOp("-",c,b)),
              mkOp("cos", mkOp("-", mkOp("-", mkOp("*", two,a),b),c)))
              )
      case _ => Op(Name(o, 0), xs.toList)
    }
  }

  def ~=(e1: Expr, e2: Expr): Boolean = (e1, e2) match {
    case (Op(f1, es1), Op(f2, es2)) if f1 == f2 => f1.x match {
      case "+" => (es1, es2) match {
        case ((a :: b :: Nil), (c :: d :: Nil)) =>
          (~=(a, c) && ~=(b, d)) || (~=(a, d) && ~=(b, c))
      }
      case "-" => (es1, es2) match {
        case ((Op(Name("-", 0), a :: b :: Nil) :: c :: Nil), (Op(Name("-", 0), d :: e :: Nil) :: f :: Nil)) =>
          ~=(a,d) && ((~=(b,d) && ~=(c,f)) || (~=(b,f) && ~=(c,e)))
        case ((a :: b :: Nil), (c :: d :: Nil)) =>
          (~=(a, c) && ~=(b, d))
      }
      case "*" => (es1, es2) match {
        case ((a :: b :: Nil), (c :: d :: Nil)) =>
          (~=(a, c) && ~=(b, d)) || (~=(a, d) && ~=(b, c))
      }
      case "cos" => (es1, es2) match {
        case ((a :: Nil), (b :: Nil)) =>
          ~=(a, b) || ~=(a, mkOp("*", Lit(GRational(-1)), b))
      }
      case _ => !(es1 zip es2).exists { case (x, y) => ! ~=(x, y) }
    }
    case (_, _) => e1 == e2
  }
}

