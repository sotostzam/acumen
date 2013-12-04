package acumen

import acumen.Pretty._
import acumen.interpreters.Common.classDef

import scala.collection.mutable.ListBuffer

// Flatten takes an model with multiple objects and attempts to
// flatten them into a single Main object.

// Right now it only handles simple cases and does not do that much
// error checking.

// This version of flatten expects all creates to be in the priv
// (i.e. init) section

object FlattenSimple {
  // Entry point
  def run(prog: Prog) = flattenMain(prog)


  case class UnhandledSyntax[T](syntax: T)(implicit prettyAble:PrettyAble[T]) extends Errors.AcumenError {
    override def getMessage = 
      "Flatten: Unhandled Syntax: " + pprint(syntax)
  }
  case class OtherUnsupported(msg: String) extends Errors.AcumenError {
    override def getMessage = "Flatten: " + msg
  }
  
  case class FlattenedClassDef(priv: List[Init], body: List[Action])

  def flattenMain(prog: Prog) : Prog  = {
    val mainClass = classDef(ClassName("Main"), prog)
    val priv = ListBuffer.empty[Init]
    val body = ListBuffer(mainClass.body:_*)
    flattenTopLevel(mainClass.priv, priv, body, prog)
    Prog(List(ClassDef(ClassName("Main"), mainClass.fields, priv.toList, body.toList)))
  }

  def flattenTopLevel(toProc: Seq[Init], priv: ListBuffer[Init], body: ListBuffer[Action], env: Prog) : Unit = {
    toProc.foreach{case Init(name, rhs) => rhs match {
      case NewRhs(Var(n), args) => 
        val o = flatten(name.x + "$", classDef(ClassName(n.x),env), args, env)
        priv ++= o.priv
        body ++= o.body
      case ExprRhs(_) => 
        priv += Init(name,rhs)
    }}
  }

  def flatten(prefix: String, obj: ClassDef, args: Seq[Expr], env: Prog) : FlattenedClassDef  = {
    assert(obj.fields.size == args.size)
    
    val fixup = new Fixup(prefix)

    val priv = ListBuffer.empty[Init]
    val bodyExtra = ListBuffer.empty[Action]
    priv ++= obj.fields.indices.map{i => Init(fixup.addPrefix(obj.fields(i)),ExprRhs(args(i)))}
    obj.priv.foreach{case Init(name, rhs) => rhs match {
      case NewRhs(Var(n), args) => 
        val o = flatten(prefix + name.x + "$", classDef(ClassName(n.x),env), args.map{fixup.mapExpr(_)}, env)
        priv ++= o.priv
        bodyExtra ++= o.body
      case ExprRhs(expr) => 
        priv += Init(fixup.addPrefix(name), ExprRhs(fixup.mapExpr(expr)))
    }}
  
    val body = fixup.mapActions(obj.body) ++ bodyExtra.toList

    FlattenedClassDef(priv.toList, body)
  }

  class Fixup(prefix: String) extends ASTMap {
    override def mapExpr(e0: Expr) = super.mapExpr(e0) match {
      case     Dot(self@Var(Name("self",0)), name) => 
        Dot(self, addPrefix(name))
      case Dot(Dot(self@Var(Name("self",0)), Name(o, 0)), Name(x, primes)) =>
        Dot(self, Name(o + "$" + x, primes))
      case e => e
    }
    override def mapDiscreteAction(a: DiscreteAction) = a match {
      case Assign(lhs, rhs) => Assign(mapExpr(lhs), mapExpr(rhs))
      case _ => throw new UnhandledSyntax(a)
    }
    def addPrefix(x: Name) = Name(prefix + x.x, x.primes)
  }

}

