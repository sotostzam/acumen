package acumen

import scala.util.parsing.input.StreamReader
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.combinator.syntactical.StdTokenParsers
import scala.util.parsing.input.CharArrayReader.EofCh
import scala.util.parsing.syntax.StdTokens
import scala.collection.immutable.HashMap

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

object Parser extends MyStdTokenParsers {

  lexical.delimiters ++=
    List("(", ")", "{", "}", "[", "]", ";", "=", ":=", "=[i]", "=[t]", "'", ",",
      ".", "+", "-", "*", "/", "^", ".+", ".-", ".*", "./", ".^",
      ":", "<", ">", "<=", ">=", "==", "~=", "||",
      "&&", "<<", ">>", "&", "|", "%", "@", "..", "+/-")

  lexical.reserved ++=
    List("for", "end", "if", "else", "create", "move", "in",
      "terminate", "class", "sum", "true", "false",
      "private", "switch", "case", "Continuous", "Discrete", "none", "type", "claim",
      "pi")

  /* token conversion */

  def toCId(s: String) = {
    new CId(s.tail.split('.').toList.map(_.toInt).tail.reverse)
  }

  /* main parser method */

  def run[A](p: Parser[A], s: String): A =
    run(p, new java.io.StringReader(s))

  def run[A](p: Parser[A], s: java.io.Reader): A = {
    val res = phrase(p)(new lexical.Scanner(StreamReader(s)))
    if (!res.successful) 
      throw ParseError(res.toString)
    else res.get
  }

  /* constructor functions */

  def mkOp(o: String, xs: Expr*) =
    Op(Name(o, 0), xs.toList)

  def smartMinus(e: Expr): Expr = {
    e match {
      case Lit(GDouble(x)) => Lit(GDouble(-x))
      case Lit(GInt(x)) => Lit(GInt(-x))
      case other => mkOp("-", other)
    }
  }

  /* combinators */

  def parens[A](p: => Parser[A]): Parser[A] = "(" ~> p <~ ")"

  def braces[A](p: => Parser[A]): Parser[A] = "{" ~> p <~ "}"

  def brackets[A](p: => Parser[A]): Parser[A] = "[" ~> p <~ "]"

  def args[A](p: => Parser[A]): Parser[List[A]] = parens(repsep(p, ","))

  /* ground values parsers */

  def gvalue = gint | gfloat | gstr | gbool

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

  /* the actual parser */

  def prog = rep(classDef) ^^ Prog

  def classDef = "class" ~! className ~! args(name) ~! inits ~! actions ~! "end" ^^
    { case _ ~ c ~ fs ~ is ~ b ~ _ => ClassDef(c, fs, is, b) }

  def inits: Parser[List[Init]] =
    ("private" ~> repsep(init, ";") <~ (opt(";") ~ "end")
      | success(Nil))

  def init = name ~! ":=" ~! initrhs ^^ { case x ~ _ ~ rhs => Init(x, rhs) }

  def initrhs =
    ("create" ~! className ~! args(expr) ^^ { case _ ~ cn ~ es => NewRhs(cn, es) }
      | expr ^^ ExprRhs)

  def actions = repsep(action, ";") <~ opt(";")

  def action: Parser[Action] =
    switchCase | ifThenElse | forEach | discretelyOrContinuously

  def switchCase =
    "switch" ~! expr ~! clauses ~! "end" ^^
      { case _ ~ s ~ cls ~ _ => Switch(s, cls) }

  def clauses = rep(clause)

  def clause =
    "case" ~ gvalue ~ assertion ~! actions ^^
      { case _ ~ lhs ~ invariant ~ rhs => Clause(lhs, invariant, rhs) } |
      "case" ~! gvalue ~! actions ^^
      { case _ ~ lhs ~ rhs => Clause(lhs, Lit(GBool(true)), rhs) }

  def assertion = "claim" ~! expr ^^ { case "claim" ~ expr => expr }

  def ifThenElse =
    ("if" ~! expr ~! actions) >> {
      case _ ~ c ~ t =>
        ("else" ~! actions ~! "end" ^^ { case _ ~ e ~ _ => IfThenElse(c, t, e) }
          | "end" ^^^ IfThenElse(c, t, List()))
    }

  def forEach =
    "for" ~! name ~! "=" ~! expr ~! actions ~! "end" ^^
      { case _ ~ i ~ _ ~ e ~ b ~ _ => ForEach(i, e, b) }

  def discretelyOrContinuously =
    (newObject(None) ^^ Discretely | elim ^^ Discretely
      | move ^^ Discretely | assignOrEquation)

  def assignOrEquation =
    expr >> { e =>
      (":=" ~> assignrhs(e) ^^ Discretely
        | "=" ~> expr ^^ (e1 => Continuously(Equation(e, e1)))
        | "=[i]" ~> expr ^^ (e1 => Continuously(EquationI(e, e1)))
        | "=[t]" ~> expr ^^ (e1 => Continuously(EquationT(e, e1))))
    }

  def assignrhs(e: Expr) =
    (expr ^^ (Assign(e, _)) | newObject(Some(e)))

  def newObject(lhs: Option[Expr]) =
    "create" ~! className ~! args(expr) ^^
      { case _ ~ cn ~ args => Create(lhs, cn, args) }

  def elim = "terminate" ~> expr ^^ Elim

  def move =
    "move" ~! expr ~! expr ^^ { case _ ~ o ~ p => Move(o, p) }

  def expr: Parser[Expr] =
    level13 * ("||" ^^^ { (x: Expr, y: Expr) => mkOp("||", x, y) })

  def level13: Parser[Expr] =
    level12 * ("&&" ^^^ { (x: Expr, y: Expr) => mkOp("&&", x, y) })

  def level12: Parser[Expr] =
    level10 * ("|" ^^^ { (x: Expr, y: Expr) => mkOp("|", x, y) })

  // no level 11 : ^ is used for exponentiation

  def level10: Parser[Expr] =
    level9 * ("&" ^^^ { (x: Expr, y: Expr) => mkOp("&", x, y) })

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
      | "/" ^^^ { (x: Expr, y: Expr) => mkOp("/", x, y) }
      | ".*" ^^^ { (x: Expr, y: Expr) => mkOp(".*", x, y) }
      | "./" ^^^ { (x: Expr, y: Expr) => mkOp("./", x, y) }
      | "%" ^^^ { (x: Expr, y: Expr) => mkOp("%", x, y) })

  def level4: Parser[Expr] =
    levelI * ("^" ^^^ { (x: Expr, y: Expr) => mkOp("^", x, y) }
      | ".^" ^^^ { (x: Expr, y: Expr) => mkOp("^", x, y) })

  def levelI: Parser[Expr] =
    level3 * ("+/-" ^^^ { (mid:Expr, pm:Expr) => ExprIntervalM(mid,pm) })

  def level3: Parser[Expr] =
    ("-" ~! access ^^ { case _ ~ e => smartMinus(e) }
      | access)

  def access: Parser[Expr] = 
    atom >> { e =>
      ("." ~ name ^^ { case _ ~ x => Dot(e, x) }
        | success(e))
    }

  def atom: Parser[Expr] =
    ("sum" ~! expr ~! "for" ~! name ~! "in" ~! expr ~! "if" ~! expr ^^ { case _ ~ e ~ _ ~ i ~ _ ~ c ~ _ ~ t => Sum(e, i, c, t) }
      | interval
      | constant
      | "type" ~! parens(className) ^^ { case _ ~ cn => TypeOf(cn) }
      | name >> { n => args(expr) ^^ { es => Op(n, es) } | success(Var(n)) }
      | brackets(repsep(expr, ",")) ^^ ExprVector
      | gvalue ^^ Lit
      | parens(expr))

  def interval: Parser[Expr] =
//    nlit ~ ".." ~ nlit ^^ { case lo ~ ".." ~ hi => ExprInterval(lo,hi) }
      "[" ~> nlit ~ ".." ~ nlit <~ "]" ^^ { case lo ~ ".." ~ hi => ExprInterval(lo,hi) }

  def constant =
    "pi" ^^^ Lit(GConstPi)
      
  def lit = (gint | gfloat | gstr) ^^ Lit

  def name: Parser[Name] =
    ident ~! rep("'") ^^ { case id ~ ps => Name(id, ps.size) }

  def className: Parser[ClassName] = ident ^^ (ClassName(_))

  def nlit = (gfloat | gint) ^^ Lit
  
  def get(gv: GroundValue): Double = gv match {
    case GInt(i) => i
    case GDouble(d) => d
    case _ => sys.error("could not convert " + gv + "to GDouble")
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
  def vStepType = "@" ~! ("Continuous" ^^^ Continuous | "FixedPoint" ^^^ FixedPoint | "Discrete" ^^^ Discrete) ^^
    { case _ ~ st => VResultType(st) }

}
