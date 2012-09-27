package acumen

import scala.text._
import scala.collection.immutable.StringOps
import util.Stores._
import util.Collections._

object Pretty {

  // A "typeclass" for types that can be pretty-printed
  trait PrettyAble[A] {
    def pretty(x:A) : Document
  }
  
  // constructor for PrettyAble instances
  object PrettyAble {
    def apply[A](f:A => Document) : PrettyAble[A] = 
      new PrettyAble[A] {
        def pretty(x:A) = f(x)
      }
  }

  // a generic pretty function
  def pretty[A](x:A)(implicit prettyAble:PrettyAble[A]) : Document = 
    prettyAble.pretty(x)

  // a generic render function
  def pprint[A](x:A)(implicit prettyAble:PrettyAble[A]) : String = 
    pprint(pretty(x))

  def pprint(d:Document) : String = {
    val w = new java.io.StringWriter()
    d.format(60,w)
    w.close()
    w.toString()
  }


  // should be in the standard lib instead of their overloaded methods
  implicit def convert(s:String) : Document = DocText(s)

  /* combinators */
 
  def space = DocText(" ")
  
  def comma = DocText(",")
  
  def semi = DocText(";")
  
  def parens(d:Document) = "(" :: d :: ")"
  
  def brackets(d:Document) = "[" :: d :: "]"
  
  def dquotes(d:Document) = "\"" :: d :: "\""
  
  def sepBy(sep:Document, ds:List[Document]) : Document =
    intersperse(sep)(ds).foldRight(DocNil:Document)(_ :: _)
  
  def breaks(ds:List[Document]) : Document = {
    def helper(ds:List[Document], acc:Document) : Document = { 
      ds match {
        case Nil    => acc
        case x::xs  => helper(xs, acc :/: x)
	    }
    }
    ds match {
      case x::xs => helper(xs, x)
      case Nil   => DocNil
    }
  }
  
  def breakWith(sep:Document, as:List[Document]) : Document = {
    def helper(as:List[Document], acc:Document) : Document = {
      as match {
        case Nil    => acc
        case d::ds  => helper(ds, acc :: sep :/: d)
	    }
    }
    as match {
      case x::xs => helper(xs, x)
      case Nil   => DocNil
    }
  }
  
  def args(xs:List[Document]) = parens(sepBy(comma, xs))
  
  def block(d:Document) = DocGroup(DocNest(2,"{" :/: d) :/: "}")
  
  /* the actual pretty-printer */
 
  implicit def prettyProg : PrettyAble[Prog] =
    PrettyAble { p =>
      breaks(p.defs map pretty[ClassDef])
    }
  

  implicit def prettyClassDef : PrettyAble[ClassDef] =
    PrettyAble { d =>
      DocNest(2, 
        "class " :: pretty(d.name) :: 
        args(d.fields map pretty[Name]) :/:
        DocGroup(DocNest(2, 
          "private" :/: breakWith(";", d.priv map pretty[Init])) :/: "end") :/:
        pretty(d.body)) :/: "end"
    }

  implicit def prettyInit : PrettyAble[Init] =
    PrettyAble { 
      case Init(x,rhs) =>
        pretty(x) :: " = " :: (
          rhs match {
            case NewRhs(cn,es) => "create " :: pretty(cn) :: args(es map pretty[Expr])
            case ExprRhs(e) => pretty(e)
          })
    }

  implicit def prettyActions : PrettyAble[List[Action]] =
    PrettyAble { as => breakWith(semi, as map pretty[Action]) }
 
  implicit def prettyClauses : PrettyAble[List[Clause]] =
    PrettyAble { cls => breaks(cls map pretty[Clause]) }

  implicit def prettyClause : PrettyAble[Clause] =
    PrettyAble {
      case Clause(lhs,assertion:Expr,rhs) => 
        DocNest(2, "case " :: pretty(lhs) :: " assume " :: pretty(assertion) :/: pretty(rhs)) 
    }

  implicit def prettyAction : PrettyAble[Action] =
    PrettyAble { 
      case IfThenElse(c,t,e) => new DocNest(2,
                                  "if " :: pretty(c) :/: 
                                  DocGroup(pretty(t))) :/: 
                                  DocGroup(DocNest(2, "else" :/: pretty(e))) :/: 
                                  "end"
      case Switch(s,cls) => new DocNest(2,"switch " :: pretty(s) :/:
                                          pretty(cls)) :/: "end"
      case ForEach(i,e,b) => new DocNest(2,
                               "for " :: pretty(i) :: 
                               " = " :: pretty(e) :/: pretty(b)) :/: "end"
      case Continuously(ca) => pretty(ca)
      case Discretely(da) => pretty(da)
    }
  
  implicit def prettyContinuousAction : PrettyAble[ContinuousAction] =
    PrettyAble {
      case Equation(lhs,rhs)  => pretty(lhs) :: " [=] " :: pretty(rhs) 
      case EquationI(lhs,rhs) => pretty(lhs) :: " =[i] " :: pretty(rhs) 
      case EquationT(lhs,rhs) => pretty(lhs) :: " =[t] " :: pretty(rhs) 
    }

  implicit def prettyDiscreteAction : PrettyAble[DiscreteAction] =
    PrettyAble {
      case Assign(lhs,rhs) => pretty(lhs) :: " = " :: pretty(rhs)
      case Create(lhs,c,as) =>
        (lhs match { case Some(e) => pretty(e) :: " = " case None => DocNil }) ::
        "create " :: pretty(c) :: args(as map pretty[Expr]) 
      case Elim(e) => "terminate" :: pretty(e)
      case Move(o,p) => "move" :: " " :: pretty(o) :: " " :: pretty(p)
    }
  
  implicit def prettyExpr : PrettyAble[Expr] =
    PrettyAble { e => 
      parens(e match {
        case Lit(i)           => pretty(i)
        case Var(n)           => pretty(n)
        case Dot(v,f)         => pretty(v) :: "." :: pretty(f)
        case Op(f,es)         => prettyOp(f,es)
        case ExprVector(l)    => brackets(sepBy(comma :: " ", l map pretty[Expr]))
        case Sum(e,i,c,t)     => "sum " :: pretty(e) :: " for " :: pretty(i) :: 
                                 " in " :: pretty(c) :: " if " :: pretty(t)
        case TypeOf(cn)       => "type" :: parens(pretty(cn))
      })
    }
  
  def prettyOp(f:Name,es:List[Expr]) =
    (f,es) match {
      case (Name("_:_:_",0), x::y::z::Nil) =>
        parens(pretty(x) :: ":" :: pretty(y) :: ":" :: pretty(z))
      case (Name(op,0),x::y::Nil) if Parser.lexical.delimiters contains op => 
        parens(pretty(x) :: " " :: op :: " " :: pretty(y))
      case _ => 
        pretty(f) :: parens(sepBy(comma :: " ", es map pretty[Expr]))
    }
  
  implicit def prettyName : PrettyAble[Name] = 
    PrettyAble { n => n.x :: "'" * n.primes }

  implicit def prettyClassName : PrettyAble[ClassName] =
    PrettyAble { n => DocText(n.x) }
  
  /* pretty printing for ground values */
  implicit def prettyGroundValue : PrettyAble[GroundValue] =
    PrettyAble {
      case GInt(i)    => i.toString
      case GDouble(x) => x.toString
      case GBool(b)   => b.toString
      case GStr(s)    => dquotes(s)
    }
  
  /* pretty printing of interpreter's internals */

  implicit def prettyValue[A] : PrettyAble[Value[A]] =
    PrettyAble {
      case VLit(i)         => pretty(i)
      case VList(l)        => sepBy("::", l map pretty[Value[A]]) :: "::nil"
      case VVector(l)      => brackets(sepBy(comma :: " ", l map pretty[Value[A]]))
      case VObjId(Some(a)) => "#" :: a.toString
      case VObjId(None)    => "#none"
      case VClassName(n)   => pretty(n)
      case VStepType(t)    => pretty(t)
    }
  

  // better not make it implicit : this is a type alias
  def prettyObject(o : Map[Name, Value[_]]) = block(prettyEnv(o))
 
  // better not make it implicit : this is a type alias
  def prettyEnv(e:Map[Name, Value[_]]) : Document = {
    val it = e map { case (x,v) => pretty(x) :: " = " :: pretty(v) }
    breakWith(comma, it.toList)
  }
  
  // better not make it implicit : this is a type alias
  def prettyStore(s:CStore) : Document = {
    val it = s map { case (i,o) => 
      "#" + i.toString :: " " :: prettyObject(o) 
    }
    breaks(it.toList)
  }
  
  implicit def prettyStepType : PrettyAble[StepType] =
    PrettyAble {
      case Discrete()   => "@Discrete"
      case Continuous() => "@Continuous"
    }
}

object JSon {

  /* we don't want complicated rendering algorithms, 
     so we redefine our document type. */

  case class DocText(txt: String) extends Document
  case object DocNil extends Document
  case class DocCons(hd: Document, tl: Document) extends Document

  implicit def convert(s:String) : Document = DocText(s)

  abstract class Document {
    def ::(hd: Document): Document = DocCons(hd, this)
    def :/:(hd: Document): Document = hd :: " " :: this

    def render(sb:StringBuilder) : Unit = this match {
      case DocNil       => ()
      case DocText(s)   => sb append s
      case DocCons(l,r) => { l.render(sb) ; r.render(sb) }
    }

    override def toString = {
      val sb = new StringBuilder()
      render(sb)
      sb.toString
    }
  }

  /* combinators */
  def comma = DocText(",")
  def braces(d:Document) = "{" :: d :: "}"
  def brackets(d:Document) = "[" :: d :: "]"
  def quotes(d:Document) = "\"" :: d :: "\""

  def sepBy(sep:Document, ds:List[Document]) : Document =
    intersperse(sep)(ds).foldRight(DocNil:Document)(_ :: _)
 
  def obj(m:Iterable[(String,Document)]) = 
    braces(sepBy(comma :: " ", (m map {case (k,v) => quotes(k) :: ":" :: v }).toList ))

  def showVal(ty:String, v:Document) =
    obj(List(("type",  quotes(ty)), 
             ("value", v)))

  def showList(l:List[Document]) =
    brackets(sepBy(comma :: " ", l))

  /* translation to JSON */

  def toJSON(t:StepType) = 
    quotes(t match {
      case Discrete()   => "Discrete"
      case Continuous() => "Continuous" 
    })

  def toJSON(v:Value[_]) : Document = v match {
    case VLit(i)       => toJSON(i)
    case VList(l)      => showVal("list", showList(l map toJSON))
    case VVector(l)    => showVal("vector", showList(l map toJSON))
    case VObjId(id)    => showVal("objId", id.toString)
    case VClassName(n) => showVal("className", quotes(n.x))
    case VStepType(t)  => showVal("stepType", toJSON(t))
  }

  def toJSON(gv:GroundValue) = gv match {
    case GInt(i)    => showVal("int",    i.toString)
    case GDouble(x) => showVal("double", x.toString)
    case GBool(b)   => showVal("bool",   b.toString)
    case GStr(s)    => showVal("string", quotes(s.toString))
  }

  def toJSONObj(e:Map[Name, Value[_]]) : Document = {
    val it = e map { case (x,v) => (x.x, toJSON(v)) }
    obj(it)
  }

  def toJSON(s:CStore) : Document = {
    val it = s map { case (i,o) => (i.toString, toJSONObj(o)) }
    obj(it)
  }
}
