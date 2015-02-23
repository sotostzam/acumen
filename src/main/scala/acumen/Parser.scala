package acumen

import scala.util.parsing.input.{Reader,StreamReader}
import scala.util.parsing.combinator.lexical.{Scanners,StdLexical}
import scala.util.parsing.combinator.syntactical.StdTokenParsers
import scala.util.parsing.input.CharArrayReader.EofCh
import scala.util.parsing.combinator.token.StdTokens
import scala.collection.immutable.HashMap
import scala.util.parsing.input.Position
import java.io.File
import java.io.BufferedReader
import scala.collection.immutable.PagedSeq

import Errors._

/* enhance StdLexical with some more tokens (doubles) */
class MyLexical extends StdLexical {

  /* more tokens than is StdTokens (adding new constructors to Token)*/
  case class FloatLit(chars: String) extends Token {
    override def toString = chars
  }

  case class CIdLit(chars: String) extends Token {
    override def toString = chars
  }

  /* we don't want to parse characters, and we want to parse floats,
   * so we override the definition of the token parser */
  override def token: Parser[Token] =
    (ident | numlit | stringlit | eof
      | unclosedString | delim | cid | failure("illegal character"))

  // opt combinator with a default value
  private def option[A](p: Parser[A], default: A) = opt(p) ^^ {
    case Some(s) => s
    case None => default
  }
  private def ident = (letter | '_') ~ rep(letter | digit | '_') ^^ {
    case first ~ rest => processIdent(first :: rest mkString "")
  }
  private def stringlit = '\"' ~ rep(chrExcept('\"', '\n', EofCh)) ~ '\"' ^^ {
    case '\"' ~ chars ~ '\"' => StringLit(chars mkString "")
  }
  private def eof = EofCh ^^^ EOF
 
  private def unclosedString = '\"' ~> failure("unclosed string literal")
  private def intlit = rep1(digit) ^^ (_ mkString "")
  private def numlit: Parser[Token] =
    intlit ~ opt(fraction ~ option(exponent, "")) ^^ {
      case intlit ~ Some(frac ~ exp) => FloatLit(List(intlit, frac, exp) mkString "")
      case intlit ~ None => NumericLit(intlit)
    }
  private def sign = elem('+') | elem('-')
  def fraction = '.' ~ rep(digit) ^^ {
    case dot ~ ff => dot :: (ff mkString "") :: Nil mkString ""
  }
  def exponent = (elem('e') | elem('E')) ~ option(sign, "") ~ intlit ^^ {
    case e ~ s ~ exp => List(e, s, exp) mkString ""
  }
  def cid =
    elem('#') ~> elem('0') ~>
      (elem('.') ~> repsep(intlit, elem('.')) ^^
        (id => new CIdLit(("#0" :: id).mkString(".")))
        | success(CIdLit("#0")))
}

/* enhance StdTokenParsers with some more primitives (doubles, lIdent, uIdent) */
class MyStdTokenParsers extends StdTokenParsers {

  type Tokens = MyLexical;
  val lexical = new MyLexical

  def floatLit: Parser[String] =
    elem("floating number", _.isInstanceOf[lexical.FloatLit]) ^^ (_.chars)

  def cidLit: Parser[String] =
    elem("canonical id", _.isInstanceOf[lexical.CIdLit]) ^^ (_.chars)
 
}

/* Position class enhanced the filename of the source file */
abstract class EnhancedPosition extends Position {
  def file: Option[File]
  override def toString: String = file match {
    case Some(file) => file.toString + ": " + super.toString
    case None => super.toString
  }
}

/* Reimplemented StreamReader so that we can add a filename to the source location.
 * Reimplemented, because StreamReader is sealed and can not be extended.
 * Orignal copyright of copyrighted code: Scala API, (c) 2006-2013, LAMP/EPFL,
 * http://scala-lang.org/ */
object MyReader {
  def apply(in: java.io.Reader, f: Option[File]): MyReader = {
    new MyReader(PagedSeq.fromReader(in), 0, 1, f)
  }
}
sealed class MyReader(seq: PagedSeq[Char], override val offset: Int, lnum: Int, f: Option[File]) extends Reader[Char]
{
  import MyReader._
  def file = f

  override lazy val source: java.lang.CharSequence = seq

  override def atEnd = !seq.isDefinedAt(offset)

  override def first = if (seq.isDefinedAt(offset)) seq(offset) else EofCh

  override def rest: MyReader =
    if (offset == seq.length) this
    else if (seq(offset) == '\n')
      new MyReader(seq.slice(offset + 1), 0, lnum + 1, file)
    else new MyReader(seq, offset + 1, lnum, file)

  private def nextEol = {
    var i = offset
    while (i < seq.length && seq(i) != '\n' && seq(i) != EofCh) i += 1
    i
  }

  override def drop(n: Int): MyReader = {
    val eolPos = nextEol
    if (eolPos < offset + n && eolPos < seq.length)
      new MyReader(seq.slice(eolPos + 1), 0, lnum + 1, file).drop(offset + n - (eolPos + 1))
    else
      new MyReader(seq, offset + n, lnum, file)
  }

  override def pos: EnhancedPosition = new EnhancedPosition {
    override def line = lnum
    override def column = offset + 1
    override def lineContents = seq.slice(0, nextEol).toString
    override def file = f
  }
}

object Parser extends MyStdTokenParsers {
  lexical.delimiters ++=
    List("(", ")", "{", "}", "[", "]", ";", "=", ":=", "=[i]", "=[t]", "'", ","," ",
      ".", "+", "-", "*", "/", "^", ".+", ".-", ".*", "./", ".^",
      ":", "<", ">", "<=", ">=", "~=", "||","->","==",
      "&&", "<<", ">>", "&", "|", "%", "@", "..", "+/-", "#include", "#semantics")

  lexical.reserved ++=
    List("foreach", "end", "if", "else","elseif", "create", "move", "in", "terminate", "model","then","initially","always",
         "sum", "true", "false", "init", "match","with", "case", "type", "claim", "hypothesis", "let","noelse",
         "Initial", "Continuous", "Discrete", "FixedPoint", "none","cross","do","dot","for","_3D")

  /* token conversion */

  def toCId(s: String) = {
    new CId(s.tail.split('.').toList.map(_.toInt).tail.reverse)
  }

  /* main parser method */
  def run[A](p: Parser[A], s: String, f: Option[File] = None): A =
    run(p, new java.io.StringReader(s), f)

   
  def run[A](p: Parser[A], s: java.io.Reader, f: Option[File]): A = {
    val res = phrase(p)(new lexical.Scanner(MyReader(s, f)))
    res match {
      case _ if res.successful => res.get
      case f:NoSuccess => throw ParseError(f.msg).setPos(f.next.pos)
    }
    if (!res.successful) 
      throw ParseError(res.toString)
    else res.get
  }
  /* constructor functions */

  def mkOp(o: String, xs: Expr*) =
    Op(Name(o, 0), xs.toList)

  def smartMinus(e: Expr): Expr = {
    e match {
      case e@Lit(GDouble(x)) => Lit(GDouble(-x)).setPos(e.pos)
      case e@Lit(GInt(x)) => Lit(GInt(-x)).setPos(e.pos)
      case other => mkOp("-", other)
    }
  }

  /* combinators */

  def parens[A](p: => Parser[A]): Parser[A] = "(" ~> p <~ ")"

  def braces[A](p: => Parser[A]): Parser[A] = "{" ~> p <~ "}"
  
  def optParens[A](p: => Parser[A]):Parser[A] = 
    opt("(") >> { x => x match{
      case Some(_) => p <~ ")"
      case None => p
    }}
  def rep2sep[T](p : => Parser[T], q : => Parser[Any]): Parser[List[T]] =
    p ~ q ~ rep1sep(p,q) ^^ {case x~ _ ~ y => x :: y}  	
    

  def brackets[A](p: => Parser[A]): Parser[A] = "[" ~> p <~ "]"

  def args[A](p: => Parser[A]): Parser[List[A]] = parens(repsep(p, ","))

  /* ground values parsers */

  def gvalue = gint | gfloat | gstr | gbool | gpattern

  def gint = opt("-") ~ numericLit ^^ {
    case m ~ x =>
      val res = x.toInt
      GInt(if (m.isDefined) -res else res)
  }
  def gfloat = opt("-") ~ floatLit ^^ {
    case m ~ x =>
      val res = x.toDouble
      GDouble(if (m.isDefined) -res else res)
  }
  def gstr = stringLit ^^ GStr
  def gbool = "true" ^^^ GBool(true) | "false" ^^^ GBool(false)
  def gpattern: Parser[GPattern] = parens(repsep(gvalue, ",")) ^^{case ls => GPattern(ls)}

  /* the actual parser */

  def prog = rep(classDef) ^^ Prog

  def getSemantics = opt(semantics) <~ rep(acceptIf( _ => true)(_ => ""))
  
  def fullProg = opt(semantics) ~> rep(include) ~ rep(classDef) ^^ { case incl ~ defs => (incl, defs) }

  def semantics = positioned("#semantics" ~! stringLit ^^ { case _ ~ str => SemanticsSpec(str) })

  def include = positioned("#include" ~! stringLit ^^ { case _ ~ str => Include(str) })

  def classInit = "model" ~ className ~ args(name) ~ "="  ~ inits 
  def classDef = positioned(
      classInit ~ "always" ~ actions  ^^ { case  _ ~ c ~ fs ~ _ ~ is ~_ ~ b   => ClassDef(c, fs, is, b) } |
      classInit ~opt("always")^^ {case  _ ~ c ~ fs ~ _ ~ is~_    => ClassDef(c, fs, is, List()) }
     )

  def inits: Parser[List[Init]] =
    ("initially" ~> repsep(init, ",") 
      | success(Nil))

  def init = name ~! "=" ~! initrhs ^^ { case x ~ _ ~ rhs => Init(x, rhs) }|
             "_3D" ~ "=" ~ threeDRhs ^^ {
             	case _ ~ _ ~ ls => Init(Name("_3D",0), ExprRhs(ls))}

  def initrhs =
    ("create" ~! className ~! args(expr) ^^ { case _ ~ cn ~ es => NewRhs(Var(Name(cn.x,0)), es) }
      | expr ^^ ExprRhs)

  def actions = repsep(action, ",") 

  def action: Parser[Action] =
    switchCase | ifThenElse | forEach | discretelyOrContinuously | claim | hypothesis

  def switchCase =
    "match" ~ expr ~ "with" ~"[" ~! clauses ~! "]" ^^
      { case _ ~ s ~ _ ~_~ cls  ~ _ => Switch(s, cls) }

  def clauses = repsep(clause,"|")

  def clause =
      gvalue ~ opt(claimExpr)  ~ "->"  ~ actions ^^
      { case  lhs ~ clm ~ _ ~ rhs => clm match{
        case Some(invariant) => Clause(lhs, invariant, rhs)
        case None => Clause(lhs, Lit(GBool(true)),rhs) }}  
  def claimExpr = "claim" ~! expr ^^ { case "claim" ~ expr => expr }

  def claim = claimExpr ^^ { case predicate => Claim(predicate) }

  def hypothesis = "hypothesis" ~! opt(stringLit) ~ expr ^^ 
    { case "hypothesis" ~ statement ~ predicate => Hypothesis(statement, predicate) }
  
  // Make the else branch mandatory
  def elseif = "elseif" ~ expr ~ "then" ~ actions ^^ {case _ ~ b ~_ ~ ac => (b,ac)}
  
  private def elseifHelper(eis:List[(Expr, List[Action])]):IfThenElse = eis match{
    case ei :: Nil => IfThenElse(ei._1, ei._2, List())
    case ei :: tail => IfThenElse(ei._1, ei._2, List(elseifHelper(tail))) 
  }
  
  private def elseHelper(els:List[Action], ite:IfThenElse):IfThenElse = ite match{
    case IfThenElse(e,t,Nil) => IfThenElse(e,t,els)
    case IfThenElse(e,t,s) =>IfThenElse(e,t,List(elseHelper(els,s(0).asInstanceOf[IfThenElse])))
  }
  
  def actionsBranch: Parser[List[Action]] = "else" ~> action ^^{x => List(x)}  | "else" ~> parens(actions)|
                                            "noelse" ^^^ List() 
		                                                   
  def ifThenElse = 
    "if" ~! expr ~! "then" ~ actions ~rep(elseif) ~  actionsBranch ^^ 
    	{case _~ c ~_ ~t ~ elseifs ~ e => elseifs match{
    	  case Nil => IfThenElse(c,t,e)
    	  case ss => IfThenElse(c,t, List(elseHelper(e,elseifHelper(ss))))
    	  }   	
    	}
  
  def forhelp = action ^^ {x => List(x)} | parens(actions)
  def forEach =
    "foreach" ~ name ~ "in" ~ expr ~"do"~ forhelp  ^^
      { case _ ~ i ~ _ ~ e ~_~ b  => ForEach(i, e, b) }
   
   // For positionlize pattern matching 
   def varP : Parser[Var] = name ^^ {x => Var(x)}
   def dotP : Parser[Dot] = varP ~ "." ~ name ^^ {case e ~ _ ~ n => Dot(e,n)}
   def pattern : Parser[Pattern] =  positioned(dotP) ^^ {case d => Pattern(List(d))}|		                          
		   							positioned(varP) ^^ {case x => Pattern(List(x))}|
		                          parens(repsep(pattern,",")) ^^ {case ls => Pattern(ls.map(x => x.ps match{
		                            case s::Nil => x.ps
		                            case ss => List(Pattern(ss))}).flatten)}
  //println(run(action, " b' == b''' "))	                           
  def patternMatch : Parser[Continuously] = positioned(pattern) ~ "=" ~ expr ^^{case p ~ _ ~ e => Continuously(Assignment(p,e))}

  def discretelyOrContinuously =
    (newObject(None) ^^ Discretely | elim ^^ Discretely 
      | move ^^ Discretely | patternMatch | assignOrEquation | _3DAction)

  def assignOrEquation =
    // Make sure lhs won't be an expr like a == b, which has the same syntax as equation
    access >> { e =>
      (	"=" ~> expr ^^ (e1 => Continuously(Equation(e, e1)))
        |"+" ~> "=" ~> assignrhs(e) ^^ Discretely     
        | "=[i]" ~> expr ^^ (e1 => Continuously(EquationI(e, e1)))
        | "=[t]" ~> expr ^^ (e1 => Continuously(EquationT(e, e1))))
    }
    
  def _3DAction = 
    "_3D" ~ "="  ~ threeDRhs ^^ {
    case _ ~ _ ~ ls => Continuously(Equation(Var(Name("_3D",0)), ls))}|
    "_3D" ~ "+"~"="  ~ threeDRhs ^^ {
    case _ ~ _~_ ~ ls => Discretely(Assign(Var(Name("_3D",0)), ls))
  }
  def assignrhs(e: Expr) =
    (expr ^^ (Assign(e, _)) | newObject(Some(e)))

  def newObject(lhs: Option[Expr]) =
    positioned("create" ~! className ~! args(expr) ^^
      { case _ ~ cn ~ args => Create(lhs, Var(Name(cn.x,0)), args) })

  def elim = "terminate" ~> expr ^^ Elim

  def move =
    "move" ~ access ~ expr ^^ { case _ ~ o ~ p => Move(o, p) }

  def binding = name ~! "=" ~! expr ^^ { case x ~ _ ~ e => (x, e) }

  def bindings = repsep(binding, ",") <~opt(",")

  def let:Parser[Expr] =
      positioned("let" ~! bindings ~! "in" ~! expr  ^^
                  { case _ ~bs ~ _~ e => ExprLet(bs, e) })

  def levelTop:Parser[Expr] =
      positioned(level12)

  def expr: Parser[Expr] = levelTop | let
       

  def level12: Parser[Expr] =
    level10 * ("||" ^^^ { (x: Expr, y: Expr) => mkOp("||", x, y) })

  // no level 11 : ^ is used for exponentiation

  def level10: Parser[Expr] =
    level9 * ("&&" ^^^ { (x: Expr, y: Expr) => mkOp("&&", x, y) })

  def level9: Parser[Expr] =
    level8 * ("==" ^^^ { (x: Expr, y: Expr) => mkOp("==", x, y) }
      | "~=" ^^^ { (x: Expr, y: Expr) => mkOp("~=", x, y) })

  def level8: Parser[Expr] =
    level7 * ("<" ^^^ { (x: Expr, y: Expr) => mkOp("<", x, y) }
      | ">" ^^^ { (x: Expr, y: Expr) => mkOp(">", x, y) }
      | "<=" ^^^ { (x: Expr, y: Expr) => mkOp("<=", x, y) }
      | ">=" ^^^ { (x: Expr, y: Expr) => mkOp(">=", x, y) })
  

  def level7: Parser[Expr] =
    levelColon * ("<<" ^^^ { (x: Expr, y: Expr) => mkOp("<<", x, y) }
      | ">>" ^^^ { (x: Expr, y: Expr) => mkOp(">>", x, y) })
      
  def levelColon: Parser[Expr] =
    level6 >> { x =>
      (":" ~! level5 >> {
        case _ ~ y =>
          (":" ~! level5 ^^ { case _ ~ z => mkOp("_:_:_", x, y, z) }
            | success(mkOp("_:_:_", x, Lit(GInt(1)), y)))
      }
        | success(x))
    }

  def level6: Parser[Expr] =
    level5 * ("+" ^^^ { (x: Expr, y: Expr) => mkOp("+", x, y) }
      | "-" ^^^ { (x: Expr, y: Expr) => mkOp("-", x, y) }
      | ".+" ^^^ { (x: Expr, y: Expr) => mkOp(".+", x, y) }
      | ".-" ^^^ { (x: Expr, y: Expr) => mkOp(".-", x, y) })

  def level5: Parser[Expr] =
    level4 * ("*" ^^^ { (x: Expr, y: Expr) => mkOp("*", x, y) }
      | "cross" ^^^ { (x: Expr, y: Expr) => mkOp("cross", x, y) }
      | "dot" ^^^ { (x: Expr, y: Expr) => mkOp("dot", x, y) }
      | "/" ^^^ { (x: Expr, y: Expr) => mkOp("/", x, y) }
      | ".*" ^^^ { (x: Expr, y: Expr) => mkOp(".*", x, y) }
      | "./" ^^^ { (x: Expr, y: Expr) => mkOp("./", x, y) }
      | "%" ^^^ { (x: Expr, y: Expr) => mkOp("%", x, y) })

  def level4: Parser[Expr] =
    levelI * ("^" ^^^ { (x: Expr, y: Expr) => mkOp("^", x, y) }
      | ".^" ^^^ { (x: Expr, y: Expr) => mkOp(".^", x, y) })

  def levelI: Parser[Expr] =
    level3 * ("+/-" ^^^ { (mid:Expr, pm:Expr) => ExprIntervalM(mid,pm) })

  def level3: Parser[Expr] =
    ("-" ~! call ^^ { case _ ~ e => smartMinus(e) }
      | call)

  def call: Parser[Expr] =
    access >> { e => args(expr) ^^ { es => /*println(Call(e,es))*/; Call(e, es) } | success(e) }


  def access: Parser[Expr] = 
    atom >> { e =>
      (positioned("." ~ name ^^ { case _ ~ x => Dot(e, x) })
        | success(e))
    }
  // For column matrix like ((1),(1),(1))
  def colVector = parens(rep2sep(parens(expr),",")) ^^ 
    {case ls => ExprVector(ls.map(x => ExprVector(List(x))))}
  def atom: Parser[Expr] =
    positioned( sum
      | interval
      |"type" ~! parens(className) ^^ { case _ ~ cn => TypeOf(cn) }
      | name >> { n => args(expr) ^^ { es => Op(n, es) } | success(Var(n)) }
      | colVector	
      | parens(rep2sep(expr, ",")) ^^ ExprVector
      | parens(expr)
      | gvalue ^^ Lit
      )


  def sum: Parser[Expr] =
    "sum" ~! expr ~! "for" ~! name ~! "in" ~! expr ~! opt("if" ~! expr) ^^
      { case _ ~ e ~ _ ~ i ~ _ ~ c ~ t =>
          Sum(e, i, c, t match {
            case None        => Lit(GBool(true)) // No "if" is same as "if true"
            case Some(_ ~ f) => f
          })} 
     
 
  def interval: Parser[Expr] =
//    nlit ~ ".." ~ nlit ^^ { case lo ~ ".." ~ hi => ExprInterval(lo,hi) }
      "[" ~> nlit ~ ".." ~ nlit <~ "]" ^^ { case lo ~ ".." ~ hi => ExprInterval(lo,hi) }

  def lit = positioned((gint | gfloat | gstr) ^^ Lit)

  def name: Parser[Name] =
    (ident) ~! rep("'") ^^ { case id ~ ps => Name(id, ps.size) }

  def className: Parser[ClassName] = ident ^^ (ClassName(_))

  def nlit = (gfloat | gint) ^^ Lit
  
  def get(gv: GroundValue): Double = gv match {
    case GInt(i) => i
    case GDouble(d) => d
    case _ => sys.error("could not convert " + gv + "to GDouble")
  }
  /* 3d configurations parser */
  val defaultCenter = ExprVector(List(Lit(GInt(0)),Lit(GInt(0)),Lit(GInt(0))))
  val defaultScale = Lit(GDouble(0.2))
  val defaultSize = ExprVector(List(Lit(GDouble(0.4)),Lit(GDouble(0.4)),
                                    Lit(GDouble(0.4))))
  val defaultLength = Lit(GDouble(0.4))
  val defaultRadius = Lit(GDouble(0.2))
  val defaultContent = Lit(GStr(" "))
  val defaultColor = ExprVector(List(Lit(GInt(1)),Lit(GInt(1)),Lit(GInt(1))))
  val defaultRotation = ExprVector(List(Lit(GInt(0)),Lit(GInt(0)),Lit(GInt(0))))
  
  def threeDPara: Parser[(Name, Expr)] = name ~ "=" ~ expr ^^ {case n ~_~ e => 
    if (n.x == "center" | n.x == "length" | n.x == "radius" | n.x == "size" | n.x == "color" |
        n.x == "rotation" | n.x == "content") 
    	  (n,e)
    else throw new PositionalAcumenError{
         def mesg = n.x + " is not a valid _3D parameter" 
         }.setPos(e.pos) }
  def threeDObject:Parser[ExprVector] = optParens(name ~ rep(threeDPara)) ^^ {case n ~ ls =>threeDParasProcess(n,ls)}
  def threeDRhs = parens(repsep(threeDObject, ",")) ^^ {case ls => ls match{
    case List(single) => single
    case _ => ExprVector(ls)
  }}
  
  def _3DVectorHelper(n:Name,v:Expr):Expr = v match{
    case ExprVector(ls) => 
      if (ls.forall(x => x match{
        case _ @ Lit(GStr(_) | GBool(_)) => false
        case _ => true
       }) && ls.length == 3) ExprVector(ls) 
      else
       throw new PositionalAcumenError{
         def mesg = "_3D parameter " + n + "'s value is not a valid vector of 3 numbers: " + Pretty.pprint(v)
         }.setPos(v.pos)
    case _ => v 
  } 
  /* Process the 3d object information and adding default values*/
  def threeDParasProcess(n:Name, paras:List[(Name, Expr)]):ExprVector = {
    val name = n.x
    val center = paras.find(_._1.x == "center") match{
      case Some(x) => _3DVectorHelper(x._1, x._2)
      case None => defaultCenter
    }
    val color = paras.find(_._1.x == "color") match{
      case Some(x) => _3DVectorHelper(x._1, x._2)
      case None => defaultColor
    }
    val rotation = paras.find(_._1.x == "rotation") match{
      case Some(x) => _3DVectorHelper(x._1, x._2)
      case None => defaultRotation
    }

    val size = paras.find(_._1.x == "size") match{
      case Some(x) => 
        if(name == "Box" | name == "Text" | name == "Obj" | name == "Sphere"){
          x._2 match{
            case ExprVector(ls) => 
              if(name == "Box") 
                _3DVectorHelper(x._1, x._2)
              else
                error("_3D object " + name + " size parameter can't be " + Pretty.pprint(x._2))
            case _ @ Lit(GBool(_) | GStr(_))  =>           
                error("_3D object " + name + " size parameter can't be " + Pretty.pprint(x._2))
            case _ => x._2
          }
        }         
        else throw new PositionalAcumenError{
          def mesg = "_3D object " + name + " can't have 'size' parameter"
        }.setPos(x._2.pos)
      case None => if(name == "Box") defaultSize else defaultScale
    }

    val radius = paras.find(_._1.x == "radius") match{
      case Some(x) => 
      	if(name == "Cylinder" | name == "Cone") 
          x._2 match{
      	   case _ @ Lit(GStr(_) | GBool(_)) => 
      	     error("_3D object " + name + "'s 'radius' parameter is not a number")
      	   case _ => x._2
      	  }
        else error("_3D object " + name + " can't have 'radius' parameter")
      case None => defaultRadius
    }
    
    val length = paras.find(_._1.x == "length") match{
      case Some(x) => 
        if(name == "Cylinder" | name == "Cone") 
          x._2 match{
      	   case _ @ Lit(GStr(_) | GBool(_)) => 
      	     error("_3D object " + name + "'s 'length' parameter is not a number")
      	   case _ => x._2
      	  }
        else error("_3D object " + name + " can't have 'length' parameter")
      case None => defaultLength
    }

    val content = paras.find(_._1.x == "content") match{
      case Some(x) => 
        if(name == "Text" | name == "Obj") 
          x._2 match{
      	   case Lit(GStr(_)) => x._2
      	   case Lit(_) => error("_3D object " + name + "'s 'content' parameter is not a string")
      	   case _ => x._2
      	  }
        else error("_3D object " + name + " can't have 'content' parameter")
      case None => defaultContent
    } 
    val rl = ExprVector(List(radius,length))
     name match{
      case "Cylinder" => ExprVector(List(Lit(GStr("Cylinder")),center,rl,color,rotation))
      case "Cone" => ExprVector(List(Lit(GStr("Cone")),center,rl,color,rotation))
      case "Box" => ExprVector(List(Lit(GStr("Box")),center,size,color,rotation))
      case "Sphere" => ExprVector(List(Lit(GStr("Sphere")),center,size,color,rotation))
      case "Text" => ExprVector(List(Lit(GStr("Text")),center,size,color,rotation,content))
      case "Obj" => ExprVector(List(Lit(GStr("OBJ")),center,size,color,rotation,content))
      case _ => error("Unsupported 3D object " + name)
    }

  }
  /* interpreter configurations parser */

  def store: Parser[CStore] =
    rep(entry) ^^ { es => HashMap.empty ++ es }

  def entry = objid ~! obj ^^ { case id ~ o => (id, o) }

  def obj = braces(objBody)

  def objBody = repsep(field, ",") ^^ (fs => HashMap.empty ++ fs)

  def field = name ~! "=" ~! value ^^ { case x ~ _ ~ v => (x, v) }

  def value: Parser[CValue] =
    gvalue ^^ VLit | vObjId | vClassName | vStepType

  def objid = cidLit ^^ toCId

  def vObjId = ("none" ^^^ None | objid ^^ (id => Some(id))) ^^ VObjId[CId]
  def vClassName = className ^^ VClassName
  def vStepType = "@" ~! ("Initial" ^^^ Initial | "Continuous" ^^^ Continuous | "FixedPoint" ^^^ FixedPoint | "Discrete" ^^^ Discrete) ^^
    { case _ ~ st => VResultType(st) }
  
}
