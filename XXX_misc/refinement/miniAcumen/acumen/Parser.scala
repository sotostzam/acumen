package miniAcumen.acumen

import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.combinator.syntactical.StdTokenParsers
import scala.util.parsing.input.CharArrayReader.EofCh

object Parser extends StdTokenParsers {

  type E = Expr[Full]
  type I = Instr[Full]
  type P = Prog[Full]
  
  type Tokens = StdLexical ; 
  val lexical = new StdLexical

  lexical.delimiters ++= List("(",")",";",":=","+","==")
  lexical.reserved   ++= List("if","then","else","end","when","do","pass","true","false") 

  /* main parser method */
  
  def parse(s:String) : P = {
    val res = phrase(prog)(new lexical.Scanner(s))
    if (res.successful) res.get else throw new Exception(res.toString)
  }
  
  /* combinators */
  
  def parens[A](p:Parser[A]) : Parser[A] = "(" ~> p <~ ")"
      
  /* the actual parser */
  
  def name : Parser[Name] = ident ^^ Name
  
  def prog : Parser[P] = rep1sep(instr, ";") ^^ (is => Seq(is))
   
  def instr : Parser[I] = nop | assign | ifThenElse | when 
  
  def nop : Parser[I] = "pass" ^^^ Nop()
  
  def assign : Parser[I] = name ~ ":=" ~ expr ^^ { case lhs ~ _ ~ rhs => Assign(lhs,rhs) }

  def ifThenElse : Parser[I] = 
    "if" ~ expr ~ "then" ~ prog ~ "else" ~ prog ~ "end" ^^
       { case _ ~ c ~ _ ~ t ~ _ ~ e ~ _ => IfThenElse(c,t,e) }

  def when : Parser[I] = "when" ~ expr ~ "do" ~ prog ~ "end" ^^
       { case _ ~ c ~ _ ~ p ~ _ => When(c,p) }
    
  def expr  : Parser[E] = arith ~ "==" ~ arith ^^ { case x ~ _ ~ y => Eq(x,y) } | arith
  
  def arith : Parser[E] = atom * ("+" ^^^ { (x:E, y:E) => Plus(x,y) })
                                 
  def atom  : Parser[E] = ( "true" ^^^ Bool(true)
                          | "false" ^^^ Bool(false)
                          | numericLit ^^ (x => Num(x.toInt))
                          | name ^^ { x => Var(x) }
                          | parens(expr))
}
