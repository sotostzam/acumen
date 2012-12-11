package miniAcumen.principle

trait Test1 {

// does it solve the expression problem ?
// does the expression problem solve this problem ?
// is it equivalent to polymorphic variants ?
// is it know that scala can do refinement types ?

class Big extends Small
class Small extends Empty
class Empty

abstract class Expression[-T <: Empty]
// Expression is contravariant in T because (A subset B) <=> B <: A 

case class Plus[T <: Big](x:Expression[T], y:Expression[T])  extends Expression[T]
case class BigOne() extends Expression[Big]
case class Minus[T <: Small](x:Expression[T], y:Expression[T]) extends Expression[T]
case class SmallOne() extends Expression[Small]

object dummy {
	val e1 : Expression[Big] = (Minus(SmallOne(), SmallOne()) : Expression[Small])
	val e2 : Expression[Small] = (Minus(SmallOne(), SmallOne()) : Expression[Small])

 	val e1_1 : Expression[Big] = (Minus(BigOne(), BigOne()) : Expression[Big])
	// val e2_2 : Expression[Small] = (Minus(BigOne(), BigOne()) : Expression[Big])

 
    val e3 : Expression[Big] = Plus(BigOne(),BigOne())
    // val e4 : Expression[Small] = Plus(BigOne(),BigOne()) 
    //val e5 : Expression[Small] = Plus(SmallOne(),SmallOne())
        
    def f(x:Expression[Big]) : Expression[Small] =
      x match {
        case Plus(x,y) => Minus(f(x),f(y))
        case Minus(x,y) => Minus(f(x),f(y))
      }
}

}