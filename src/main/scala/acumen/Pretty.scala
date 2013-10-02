package acumen

import scala.text._
import collection.immutable.{HashMap, StringOps}
import util.Stores._
import util.Collections._
import scala.util.parsing.json._
import com.sun.corba.se.impl.corba.CORBAObjectImpl
import acumen.Errors.{FromJSONError, ShouldNeverHappen}

class Pretty {

  var filterStore = false

  var withType = false
  var exprWithType = false

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

  def pprintOneLine(d:Document) : String = {
    val w = new java.io.StringWriter()
    d.format(Int.MaxValue,w)
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
  
  // FIXME: Evil global!
  var _types : Option[scala.collection.Map[Name, TypeLike]] = None

  implicit def prettyClassDef : PrettyAble[ClassDef] =
    PrettyAble { d =>
      if (withType && d._types != null)
        _types = Some(d._types)
      DocNest(2, 
        "class " :: pretty(d.name) :: 
        args(d.fields map prettyNameWithType) :/:
        DocGroup(DocNest(2, 
          "private" :/: breakWith(";", d.priv map pretty[Init])) :/: "end") :/:
        pretty(d.body)) :/: "end"
    }

  implicit def prettyInit : PrettyAble[Init] =
    PrettyAble { 
      case Init(x,rhs) =>
        prettyNameWithType(x)  :: ":= " :: (
          rhs match {
            case NewRhs(cn,es) => "create " :: pretty(cn) :: args(es map pretty[Expr])
            case ExprRhs(e) => pretty(e)
          })
    }

  def prettyNameWithType(x: Name) : Document = {
    pretty(x) ::
    (_types match {
      case Some(typs) => ":" + typs(x).toString
      case None => ""
    })
  }

  implicit def prettyActions : PrettyAble[List[Action]] =
    PrettyAble { as => breakWith(semi, as map pretty[Action]) }
 
  implicit def prettyClauses : PrettyAble[List[Clause]] =
    PrettyAble { cls => breaks(cls map pretty[Clause]) }

  implicit def prettyClause : PrettyAble[Clause] =
    PrettyAble {
      case Clause(lhs,assertion:Expr,rhs) => 
        DocNest(2, "case " :: pretty(lhs) :: " claim " :: pretty(assertion) :/: pretty(rhs)) 
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
      case Claim(e) => "claim " :: pretty(e)
    }
  
  implicit def prettyContinuousAction : PrettyAble[ContinuousAction] =
    PrettyAble {
      case Equation(lhs,rhs)  => pretty(lhs) :: " = " :: pretty(rhs) 
      case EquationI(lhs,rhs) => pretty(lhs) :: " =[i] " :: pretty(rhs) 
      case EquationT(lhs,rhs) => pretty(lhs) :: " =[t] " :: pretty(rhs) 
    }

  implicit def prettyDiscreteAction : PrettyAble[DiscreteAction] =
    PrettyAble {
      case Assign(lhs,rhs) => pretty(lhs) :: " := " :: pretty(rhs)
      case Create(lhs,c,as) =>
        (lhs match { case Some(e) => pretty(e) :: " := " case None => DocNil }) ::
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
      }) :: (if (exprWithType && e._type != null) ":" + e._type.toString else "")
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
      case GDouble(x) => "%f".format(x)
      case GBool(b)   => b.toString
      case GStr(s)    => dquotes(s)
      case _          => "??"
    }
  
  /* pretty printing of interpreter's internals */

  implicit def prettyValue[A] : PrettyAble[Value[A]] =
    PrettyAble {
      case VLit(i)         => pretty(i)
      case VList(l)        => sepBy("::", l map pretty[Value[A]]) :: "::nil"
      case VVector(l)      => brackets(sepBy(comma :: " ", l map pretty[Value[A]]))
      case VObjId(Some(a)) => "#" :: a.cid.toString
      case VObjId(None)    => "#none"
      case VClassName(n)   => pretty(n)
      case VResultType(t)  => pretty(t)
    }
  

  // better not make it implicit : this is a type alias
  def prettyObject(o : GObject) = block(prettyEnv(o))
 
  // better not make it implicit : this is a type alias
  def prettyEnv(e:GObject) : Document = {
    val sorted = e.toList.sortWith { (a,b) => a._1 < b._1 } 
    val filtered = if (filterStore) {
      val VClassName(ClassName(name)) = e.find{_._1 == Name("className",0)}.get._2
      if (name == "Simulator")
        sorted.filter { case (x,v) => x.x == "className" || x.x == "time"}
      else
        sorted.filter { case (x,_) => x.x == "className" || interpreters.Common.specialFields.indexOf(x.x) == -1 }
    } else {
      sorted
    }
    val it = filtered.map { case (x,v) => pretty(x) :: " = " :: pretty(v) }
    breakWith(comma, it)
  }
  
  // better not make it implicit : this is a type alias
  def prettyStore(s:GStore) : Document = {
    val it = s.toList.sortWith { (a,b) => a._1 < b._1 } map { case (i,o) => 
      "#" + i.toString :: " " :: prettyObject(o) 
    }
    breaks(it.toList)
  }
  
  implicit def prettyStepType : PrettyAble[ResultType] =
    PrettyAble {
      case Discrete   => "@Discrete"
      case FixedPoint => "@FixedPoint"
      case Continuous => "@Continuous"
    }
}

object Pretty extends Pretty

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

  // Handle the special cases in which a double value is not a number:
  //   PositiveInfinity
  //   NegativeInfinity
  //   NaN
  def showDouble(d:Double) : String = {
    d match {
      case Double.PositiveInfinity => '"' + Double.PositiveInfinity.toString() + '"'
      case Double.NegativeInfinity => '"' + Double.NegativeInfinity.toString() + '"'
      case Double.NaN => '"' + Double.NaN.toString()
      case _ => d.toString()
    }
  }

  def showOption(o:Option[_]) : String = o match {
	  case None => "\"\""
	  case Some(s) => '"' + s.toString + '"'
  }

  /* translation to JSON */

  def toJSON(t:ResultType) = 
    quotes(t match {
      case Discrete   => "Discrete"
      case FixedPoint => "FixedPoint"
      case Continuous => "Continuous" 
    })

  def toJSON(v:Value[_]) : Document = v match {
    case VLit(i)       => toJSON(i)
    case VList(l)      => showVal("list", showList(l map toJSON))
    case VVector(l)    => showVal("vector", showList(l map toJSON))
    case VObjId(id)    => showVal("objId", showOption(id))
    case VClassName(n) => showVal("className", quotes(n.x))
    case VResultType(t)=> showVal("resultType", toJSON(t))
  }

  def toJSON(gv:GroundValue) = gv match {
    case GInt(i)    => showVal("int",    i.toString)
    case GDouble(x) => showVal("double", showDouble(x))
    case GBool(b)   => showVal("bool",   b.toString)
    case GStr(s)    => showVal("string", quotes(s.toString))
  }

  def toJSONObj(e:Map[Name, Value[_]]) : Document = {
    val it = e map { case (x,v) => (x.x + "/" + x.primes, toJSON(v)) }
    obj(it)
  }

  def toJSON(s:CStore) : Document = {
    val it = s map { case (i,o) => (i.toString, toJSONObj(o)) }
    obj(it)
  }

  def fromJSON(s:String) : CStore = {
    val p = JSON.parseFull(s)
    var cs = new HashMap[CId, CObject]
    try {
      p match {
        case Some(m: Map[_,_]) => m collect {
          case (k: String, v: Map[String,Map[String,_]]) => {
            cs += (strToCId(k) -> fromJSON(v))
          }
        }
      }
    } catch {
      case ex:Exception => throw FromJSONError(s)
    }
    cs
  }

  // m to be transformed into a HashMap[Name, CObject]
  def fromJSON(m: Map[String,Map[String,_]]) : CObject = {
    var co = new HashMap[Name, CValue]
    for ((k,v) <- m) {
      val (name,primes) = getNameAndPrimes(k)
      co += (Name(name, primes) -> fromJSONCVal(v))
    }
    co
  }

  def getNameAndPrimes(s: String) : (String, Int) = {
    s.split("/") match {
      case Array(name, primes) => (name, primes.toInt)
      case _ => throw ShouldNeverHappen()
    }
  }

  def fromJSONCVal(m: Map[String,_]) : CValue = {
    m("type") match {
      case "int"    => getInt(m("value"))
      case "double" => getDouble(m("value"))
      case "bool"   => getBool(m("value"))
      case "string" => getStr(m("value"))

      case "list"   => getList(m("value"))
      case "vector" => getVector(m("value"))
      case "objId"  => getObjId(m("value"))
      case "className" => getClassName(m("value"))
      case "resultType" => getResultType(m("value"))

      case _ => throw ShouldNeverHappen()
    }
  }

  def getInt(v: Any) = {
    v match {
      case s: String => VLit(GInt(s.toInt))
      case d: Double => VLit(GInt(d.toInt))
      case _ => throw ShouldNeverHappen()
    }
  }

  // A double value is not necessary serialized to a number.
  // When its value is non-numeric, it's treated as a string.
  def getDouble(v: Any) = {
    v match {
      case d: Double => VLit(GDouble(d))
      case "-Infinity" => VLit(GDouble(Double.NegativeInfinity))
      case "Infinity" => VLit(GDouble(Double.PositiveInfinity))
      case "NaN" => VLit(GDouble(Double.NaN))
      case _ => throw ShouldNeverHappen()
    }
  }

  def getBool(v: Any) = {
    v match {
      case s: String => VLit(GBool(s.toBoolean))
      case b: Boolean => VLit(GBool(b))
      case _ => throw ShouldNeverHappen()
    }
  }

  def getStr(v: Any) = {
    v match {
      case s: String => VLit(GStr(s))
      case _ => throw ShouldNeverHappen()
    }
  }

  def getObjId(v: Any) = {
    v match {
      case s: String if (s.length() == 0) => VObjId(None)
      case s: String => VObjId(Some(strToCId(s)))
      case _ => throw ShouldNeverHappen()
    }
  }

  def getClassName(v: Any) = {
    v match {
      case s: String => VClassName(ClassName(s))
      case _ => throw ShouldNeverHappen()
    }
  }

  def getResultType(v: Any) = {
    v match {
      case "Discrete"   => VResultType(Discrete)
      case "FixedPoint" => VResultType(FixedPoint)
      case "Continuous" => VResultType(Continuous)
      case _ => throw ShouldNeverHappen()
    }
  }

  def getVector(v: Any) = {
    v match {
      case list: List[Map[String,_]] => VVector(list.map { e => fromJSONCVal(e) })
      case List() => VVector(List())
      case _ => throw ShouldNeverHappen()
    }
  }

  def getList(v: Any) = {
    v match {
      case list: List[Map[String,_]] => VList(list.map { e => fromJSONCVal(e) })
      case List() => VList(List())
      case _ => throw ShouldNeverHappen()
    }
  }

  // Converts a string in the form of "0.0" in a CId object (CId(0) in this case)
  // There is a special case that the root object has string representation of "0".
  def strToCId(s:String) : CId = {
    // Convert from string to a list of integers
    // and remove the 0. prefix (the first element)
    val x = s.split('.').toList.map(_.toInt).tail

    x.size match {
      case 0 => CId()
      case 1 => CId(x.head)
      case _ => new CId(x.reverse)
    }
  }
}

