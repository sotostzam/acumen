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
  
  /** Smart constructors */
  def mkOp(o: String, xs: Expr*): Expr = {
    val two = Lit(GRational(2))
    (o, xs.toList) match {
      case ("+", Lit(GRational(n)) :: Lit(GRational(m)) :: Nil) => Lit(GRational(n + m))
      case ("-", Lit(GRational(n)) :: Lit(GRational(m)) :: Nil) => Lit(GRational(n - m))
      case ("*", Lit(GRational(n)) :: Lit(GRational(m)) :: Nil) => Lit(GRational(n * m))
      case ("/", Lit(GRational(n)) :: Lit(GRational(m)) :: Nil) => Lit(GRational(n / m))
      case ("^", Lit(GRational(n)) :: Lit(GRational(m)) :: Nil) if m.isWhole => Lit(GRational(n pow m.toInt))
      case ("+", x :: RationalZeroLit :: Nil) => x
      case ("+", RationalZeroLit :: x :: Nil) => x
      case ("+", x :: y :: Nil) if x == y => mkOp("*",two,x)
      case ("*", x :: y :: Nil) if x == y => mkOp("^",x,two)
      case ("*", RationalZeroLit :: x :: Nil) => RationalZeroLit
      case ("*", x :: RationalZeroLit :: Nil) => RationalZeroLit
      case ("*", RationalOneLit :: x :: Nil) =>  x
      case ("*", x :: RationalOneLit ::  Nil) => x
      case ("*", Lit(GRational(n)) :: Op(Name("*",0),Lit(GRational(m)) :: x :: Nil) :: Nil) => 
        mkOp("*",Lit(GRational(n * m)),x)
      case ("^", RationalZeroLit :: y :: Nil) => RationalZeroLit
      case ("^", x :: RationalOneLit  :: Nil) => x
      case ("/", x :: RationalOneLit :: Nil) => x
      case ("/", RationalOneLit :: Op(Name("/",0), y::z::Nil) :: Nil) => mkOp("/",z,y) 
      case ("/", x :: y :: Nil) if x == y => RationalOneLit
      case ("/", RationalZeroLit :: x:: Nil) => RationalZeroLit
      case ("-", x :: RationalZeroLit :: Nil) => x
      case ("-", RationalZeroLit :: Op(Name("*",0),Lit(GRational(m)) :: x :: Nil) :: Nil) => 
        mkOp("*",Lit(GRational(-m)),x)
      case ("-", RationalZeroLit :: Op(Name("*",0),x :: Lit(GRational(m)) ::  Nil) :: Nil) => 
        mkOp("*",Lit(GRational(-m)),x)
      case ("/", Lit(GRational(n)) :: Op(Name("*",0),Lit(GRational(m)) :: x :: Nil) :: Nil) => 
        mkOp("/",Lit(GRational(n/m)),x)
      case ("/", Lit(GRational(n)) :: Op(Name("*",0),x :: Lit(GRational(m)) ::  Nil) :: Nil) => 
        mkOp("/",Lit(GRational(n/m)),x)
      case ("*", Lit(GRational(n)) :: Op(Name("/",0),Lit(GRational(m)) :: x ::   Nil) :: Nil) => 
        mkOp("/",Lit(GRational(n*m)),x)
      case ("*", Lit(GRational(n)) :: Op(Name("/",0),x :: Lit(GRational(m)) ::   Nil) :: Nil) => 
        mkOp("*",Lit(GRational(n/m)),x)  
      case ("*", Op(Name("*",0), a::b::Nil) :: Op(Name("*",0), c::d::Nil) :: Nil)  =>
        val orig = Op(Name("*",0),mkOp("*",a,b)::mkOp("*",c,d)::Nil) 
        val try1 = Op(Name("*",0),mkOp("*",a,c)::mkOp("*",b,d)::Nil) 
        val try2 = Op(Name("*",0),mkOp("*",a,d)::mkOp("*",b,c)::Nil) 
        val try1l = try1.toString.length
        val try2l = try2.toString.length
        val origl = orig.toString.length
        val m = List(origl -> orig, try1l -> try1, try2l -> try2)
        m.sortBy(_._1).head._2
      case _ => Op(Name(o, 0), xs.toList)
    }
  }
}