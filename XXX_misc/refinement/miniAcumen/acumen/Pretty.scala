package miniAcumen.acumen

import scala.text._

object Pretty {
  
  type E = Expr[Full]
  type I = Instr[Full]
  type P = Prog[Full]
  
  /* combinators */
 
  def parens(d:Document) = DocText("(") :: d :: DocText(")")
    
  /* the actual pretty-printer */
  
  def pretty(p:P) : Document = p match { 
    case Seq(is) => pretty(is) 
  }
  
  def pretty(is:List[I]) : Document = {
    is match {
	  case Nil    => DocNil
      case i::Nil => pretty(i)
      case i::is  => pretty(i) :: ";" :/: pretty(is)
	}
  }
  
  def pretty(i:I) : Document =
    i match {
      case Nop()             => DocText("pass")
      case Assign(lhs,rhs)   => pretty(lhs) :: " := " :: pretty(rhs)
      case IfThenElse(c,t,e) => new DocNest(2,
                                  "if " :: pretty(c) :/:                                 
                                  DocGroup(DocNest(2,"then" :/: pretty(t))) :/: 
                                  DocGroup(DocNest(2,"else" :/: pretty(e)))) :/:
                                  DocText("end")
      case When(c,i)         => new DocNest(2,
                                 "when " :: pretty(c) :: " do" :/:                                 
                                    pretty(i)) :/:
                                  DocText("end")
    }
    
  def pretty(e:E) : Document =
    e match {
      case Num(i)    => DocText(i.toString())
      case Bool(b)   => DocText(b.toString())
      case Var(n)    => pretty(n)
      case Plus(x,y) => parens(pretty(x) :: " + " :: pretty(y))
      case Eq(x,y)   => parens(pretty(x) :: " == " :: pretty(y))
    }
  
  def pretty(n:Name) : Document = DocText(n.x)
  
  def render(d:Document) = {
    val w = new java.io.StringWriter()
    d.format(60,w)
    w.close()
    w.toString()
  }
  
  def breaks(ds:List[Document]) : Document =
    ds match {
	  case Nil    => DocNil
      case x::Nil => x
      case x::xs  => x :/: breaks(xs)
	}
  
  def pretty(s:Store) : Document = {
    s.m.foldRight(DocNil:Document)({ 
      case ((x,v),r) => pretty(x) :: " = " :: pretty(v) :/:  r
    })
  }

}
