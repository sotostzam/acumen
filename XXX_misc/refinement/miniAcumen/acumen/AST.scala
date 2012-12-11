package miniAcumen.acumen

class AST {} // bogus class to prevent recompilation

/*
== Values Language ==

Value ::= Num | Bool

== Desugared Language (without when) ==

Value ::= Num | Bool
Expr  ::= Value | Expr + Expr | Expr == Expr
Instr ::= Nop | Var := Expr | if Expr then Prog else Prog end
Prog  ::= Instr (; Instr)*

== Full Language ==

Value ::= Num | Bool
Expr  ::= Value | Expr + Expr | Expr == Expr
Instr ::= Nop | Var := Expr | if Expr then Prog else Prog end | when Expr do Prog end 
Prog  ::= Instr (; Instr)* 
*/

class Full
class Desugared extends Full
class Values    extends Desugared

case class Name(x:String)

abstract class Expr[+T >: Values <: Full]
case class Num(i:Int)                                         extends Expr[Values]
case class Bool(b:Boolean)                                    extends Expr[Values]
case class Var[T >: Desugared <: Full](n:Name)                extends Expr[T]
case class Plus[T >: Desugared <: Full](x:Expr[T], y:Expr[T]) extends Expr[T]
case class Eq[T >: Desugared <: Full](x:Expr[T], y:Expr[T])   extends Expr[T]

abstract class Instr[+T >: Desugared <: Full]
case class Nop[T >: Desugared <: Full]()                                       extends Instr[T]
case class Assign[T >: Desugared <: Full](x:Name, e:Expr[T])                   extends Instr[T]
case class IfThenElse[T >: Desugared <: Full](c:Expr[T], t:Prog[T], e:Prog[T]) extends Instr[T]
case class When[T >: Desugared <: Full](c:Expr[T], t:Prog[T])                  extends Instr[T]

abstract class Prog[+T >: Desugared <: Full]
case class Seq[T >: Desugared <: Full](is:List[Instr[T]]) extends Prog[T]

// this one is for the interpreter output
case class Store(m:Map[Name, Expr[Values]])