package acumen

import net.java.jinterval.rational.Rational

import Errors._
import acumen.util.Canonical
import interpreters.enclosure.Interval

/** Pass to replace arbitrary precision GRationals with ground 
 *  values that wrap an approximation suitable for a particular 
 *  InterpreterType. */
object ApproximateRationals {
  
  def run(p: Prog, interpreterType: InterpreterType) : Prog =
    new util.ASTMap {
      override def mapExpr(e: Expr): Expr = e match {
        case Lit(GRational(d)) => Lit(approximate(d, interpreterType))
        case _ => super.mapExpr(e)
      }
    }.mapProg(p)
    
  def run(s: CStore, interpreterType: InterpreterType): CStore =
    s.map {
      case (cid, co) => 
        (cid, co.map {
            case (n, v) => (n, approximateValue(v, interpreterType)) 
        })
    }

  private def approximateValue(v: CValue, interpreterType: InterpreterType): CValue =
    v match {
      case VLit(GRational(r)) => VLit(approximate(r, interpreterType))
      case VVector(vs)        => VVector(vs.map(approximateValue(_, interpreterType)))
      case _                  => v
    }
    
  private def approximate(r: spire.math.Rational, interpreterType: InterpreterType): GroundValue =
    interpreterType match {
      case TraditionalInterpreterType => 
        GDouble(r.toDouble)
      case _: RigorousInterpreterType => 
        GInterval(Interval(Rational.valueOf(r.numerator.bigInteger, r.denominator.bigInteger)))
    }
    
}