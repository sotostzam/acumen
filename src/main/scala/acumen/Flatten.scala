package acumen

import acumen.Pretty._
import acumen.interpreters.Common.classDef

import scala.collection.mutable.ListBuffer

// Flatten takes an model with multiple objects and attempts to
// flatten them into a single Main object.

// Right now it only handles simple cases and does not do that much
// error checking.

object Flatten {
  case class UnhandledSyntax[T](syntax: T)(implicit prettyAble:PrettyAble[T]) extends Errors.AcumenError {
    override def getMessage = 
      "Flatten: Unhandled Syntax: " + pprint(syntax)
  }
  case class OtherUnsupported(msg: String) extends Errors.AcumenError {
    override def getMessage = "Flatten: " + msg
  }
  
  case class FlattenedClassDef(priv: List[Init], body: List[Action])
}

class Flatten(val prog: Prog) {
  import Flatten._
  class Fixup(prefix: String, special: Option[String]) extends ASTMap {
    override def mapExpr(e0: Expr) = super.mapExpr(e0) match {
      case     Dot(self@Var(Name("self",0)), name) => 
        Dot(self, addPrefix(name))
      case Dot(Dot(self@Var(Name("self",0)), Name(o, 0)), Name(x, primes)) if !special.exists(_ == o) => 
        Dot(self, Name(o + "$" + x, primes))
      case e => e
    }
    override def mapDiscreteAction(a: DiscreteAction) = a match {
      case Assign(lhs, rhs) => Assign(mapExpr(lhs), mapExpr(rhs))
      case _ => throw new UnhandledSyntax(a)
    }
    def addPrefix(x: Name) = Name(prefix + x.x, x.primes)
  }

  def flatten(prefix: String, obj: ClassDef, args: Seq[Expr]) : FlattenedClassDef  = {
    assert(obj.name.x == "Main" || obj.fields.size == args.size)
    
    val special = if (obj.name.x == "Main") Some(obj.fields(0).x) else None
    val fixup = new Fixup(prefix, special)

    val priv = ListBuffer.empty[Init]
    val bodyExtra = ListBuffer.empty[Action]
    if (obj.name.x != "Main")
      priv ++= obj.fields.indices.map{i => Init(fixup.addPrefix(obj.fields(i)),ExprRhs(args(i)))}
    obj.priv.foreach{case Init(name, rhs) => rhs match {
      case NewRhs(cn, args) => 
        val o = flatten(prefix + name.x + "$", classDef(cn,prog), args.map{fixup.mapExpr(_)})
        priv ++= o.priv
        bodyExtra ++= o.body
      case ExprRhs(expr) => 
        priv += Init(fixup.addPrefix(name), ExprRhs(fixup.mapExpr(expr)))
    }}
  
    val body = fixup.mapActions(obj.body) ++ bodyExtra.toList

    FlattenedClassDef(priv.toList, body)
  }

  val mainClass = classDef(ClassName("Main"), prog)
  val flatMain = flatten("", mainClass, Nil)
  val res = Prog(List(ClassDef(ClassName("Main"), mainClass.fields, flatMain.priv, flatMain.body)))
}
