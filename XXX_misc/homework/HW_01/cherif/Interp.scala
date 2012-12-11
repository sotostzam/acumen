
package repeat

abstract class Exp
case class Const(n: Int) extends Exp
case class Var(x: String) extends Exp
case class Plus(e1: Exp, e2: Exp) extends Exp
case class Minus(e1: Exp, e2: Exp) extends Exp
case class Repeat(x1: String, n1: Int, n2: Int, x2: String, n3: Int, e: Exp) extends Exp

object Interp {
  
  private type Env = String => Int 
  
  private def EmptyEnv(x: String): Int = throw new Exception("Not Found")
  
  private def eval(e: Exp, env: Env): Int = e match {
    case Const(n) => n
  	case Var(x) => env(x) 
  	case Plus(e1,e2) => eval(e1,env) + eval(e2,env) 
  	case Minus(e1,e2) => eval(e1,env) - eval(e2,env) 
  	case Repeat(x1,n1,n2,x2,n3,e) => 
  	  if (n1 > n2) n3
	  else {
	    val n4 = n1 + 1
        val pairs = new Array[Tuple2[String,Int]](2)
        pairs(0) = (x1,n1)
        pairs(1) = (x2,n3)
	    val env2 = ext(env, pairs)
	    val n5 = eval(e,env2) 
	    eval(Repeat(x1,n4,n2,x2,n5,e),env2)
      }
  }
  
  private def ext (env: Env, pairs: Array[Tuple2[String,Int]]): Env =
    { case x => 
      	var found = false
        var n = 0
      	pairs.foreach((pair: Tuple2[String,Int]) => 
      	  if (x == pair._1) {
      	    found = true
      	    n = pair._2
      	  })
	   if (found) n else env(x)
    }  

  def eval(e: Exp) :Int =
    eval(e,EmptyEnv)
      
}


