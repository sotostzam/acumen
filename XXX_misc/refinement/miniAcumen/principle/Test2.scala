package miniAcumen.principle

trait Test2 {

// test1 the other way around

  class Small    extends Big        // language Small contains only -
  class Big      extends Universe   // language Big contains - and +
  class Universe 

  abstract class Expression[+T <: Universe]
  // Expression is contravariant in T because (A subset B) <=> B <: A 

  case class Plus[T >: Big <: Universe](x:Expression[T], y:Expression[T])  extends Expression[T]
  case class BigOne() extends Expression[Big]
  case class Minus[T >: Small <: Universe](x:Expression[T], y:Expression[T]) extends Expression[T]
  case class SmallOne() extends Expression[Small]

  class Leq[S, T >: S]()

  object dummy {
	val e1 : Expression[Big]   = Minus(SmallOne(), SmallOne())
	val e2 : Expression[Small] = Minus(SmallOne(), SmallOne())

 	val e1_1 : Expression[Big] = Minus(BigOne(), BigOne())
	//val e2_2 : Expression[Small] = Minus(BigOne(), BigOne())

 
    val e3 : Expression[Big] = Plus(BigOne(),BigOne())
    //val e4 : Expression[Small] = Plus(BigOne(),BigOne())
    //val e5 : Expression[Small] = Plus(SmallOne(),SmallOne())
    
    def f(x:Expression[Big]) : Expression[Small] =
      x match {
        case Plus(x,y) => Minus(f(x),f(y))
        case Minus(x,y) => Minus(f(x),f(y))
      }
  }
  
}

