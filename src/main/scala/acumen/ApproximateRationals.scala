package acumen

import net.java.jinterval.rational.Rational

import Errors._
import acumen.util.Canonical
import interpreters.enclosure.Interval

/** Pass to replace arbitrary precision GRationals with ground 
 *  values that wrap an approximation suitable for a particular 
 *  InterpreterType. */
object ApproximateRationals {
  
  def mkApproximationMap(p: Prog, interpreterType: InterpreterType) : util.ASTMap =
    new util.ASTMap {
      override def mapExpr(e: Expr): Expr = e match {
        case Lit(GRational(d)) => 
          Lit(approximate(d, interpreterType)).setPos(e.pos)
        case ExprInterval(Lit(GRational(lo)), Lit(GRational(hi))) =>
          interpreterType match {
            case TraditionalInterpreterType => 
              ExprInterval(Lit(approximate(lo, interpreterType)), Lit(approximate(hi, interpreterType)))
            case _: RigorousInterpreterType =>
              Lit(GConstantRealEnclosure(Interval(lo) /\ Interval(hi)))
          }
        case _ => super.mapExpr(e)
      }
      override def mapGroundValue(gv: GroundValue): GroundValue = gv match {
        case GRational(r) => approximate(r, interpreterType)
        case _ => gv
      } 
      override def mapClassDef(d: ClassDef) : ClassDef = d match {
        // NOTE: Simulator fields are approximated as in the traditional interpreters
//        case ClassDef(name, fields, priv, body) if name == Canonical.cmagic =>
//          mkApproximationMap(p, TraditionalInterpreterType).mapClassDef(d)
        case _ => super.mapClassDef(d)
      }
    } 
  
  def run(p: Prog, interpreterType: InterpreterType) : Prog =
    mkApproximationMap(p,interpreterType).mapProg(p)
  
  def run(e: Expr, interpreterType: InterpreterType) : Expr =
    mkApproximationMap(Prog(Nil),interpreterType).mapExpr(e)
    
  def run(s: CStore, interpreterType: InterpreterType): CStore =
    s.map {
      case (cid, co) => 
        (cid, co.map {
            case (Name("time",0), VLit(GRational(n))) if n == spire.math.Rational.zero =>
              (Name("time",0), VLit(GDouble(0.0)) )
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
      case TraditionalInterpreterType => if (r.isWhole) GInt(r.toInt) else GDouble(r.toDouble)
      case _: RigorousInterpreterType => GConstantRealEnclosure(Interval(r))
    }
}