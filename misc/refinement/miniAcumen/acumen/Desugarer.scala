package miniAcumen.acumen

object Desugarer {
  
  def desugar(p:Prog[Full]) : Prog[Desugared] = p match {
    case Seq(is) => Seq(is.map(desugar))
  }
  
  def desugar(i:Instr[Full]) : Instr[Desugared] = i match {
    case Nop()             => Nop()
    case Assign(x,e)       => Assign(x,desugar(e))
    case IfThenElse(c,t,e) => IfThenElse(desugar(c),desugar(t),desugar(e))
    case When(c,t)         => IfThenElse(desugar(c),desugar(t),Seq(Nop()::Nil))
  }
  
  def desugar(e:Expr[Full]) : Expr[Desugared] = e match {
    case Num(i)    => Num(i)
    case Bool(b)   => Bool(b)
    case Var(n)    => Var(n)
    case Plus(x,y) => Plus(desugar(x),desugar(y))
    case Eq(x,y)   => Eq(desugar(x),desugar(y))
  }
}
