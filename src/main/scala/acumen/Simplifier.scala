package acumen

import Errors._
import util.Names._

// Simplifier: remove Intervals and replace them with the mid point.

object Simplifier extends util.ASTMap {
  override def mapExpr(e: Expr) : Expr = super.mapExpr(e match {
    case ExprInterval(lo, hi) => Op(Name("/",0),List(Op(Name("+",0), List(lo, hi)), Lit(GDouble(2.0))))
    case ExprIntervalM(mid, _) => mid
    case expr => expr})
  def run(t: Prog): Prog = mapProg(t)
}

