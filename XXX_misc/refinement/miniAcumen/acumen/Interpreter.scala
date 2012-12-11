package miniAcumen.acumen

import scala.collection.immutable.HashMap

object Interpreter {
  
  def evalE(s:Store, e:Expr[Desugared]) : Expr[Values] = e match {
    case Num(i)    => Num(i)
    case Bool(b)   => Bool(b)
    case Var(n)    => s.m(n)
    case Plus(x,y) => (evalE(s,x),evalE(s,y)) match { case (Num(i),Num(j)) => Num (i+j) }
    case Eq(x,y)   => (evalE(s,x),evalE(s,y)) match { case (Num(i),Num(j)) => Bool(i == j) }
  }
  
  def evalI(s:Store, i:Instr[Desugared]) : Store = i match {
      case Nop()             => s
      case Assign(n,e)       => Store (s.m + new Tuple2(n, evalE(s,e)))
      case IfThenElse(c,t,e) => evalE (s,c) match {
        case Bool(b) => if (b) evalP(s,t) else evalP(s,e)
      }
  }
  
  def evalP(s:Store, p:Prog[Desugared]) : Store = p match {
    case Seq(is) => is.foldLeft(s)(evalI)
  }
  
  def eval(p:Prog[Desugared]) : Store = evalP(Store(HashMap.empty),p)
  
}
