package miniAcumen.acumen

import scala.io.Source

object Main {

  val example = 
    """
    x := 3;
	y := x;
    if true then z := 1 else z := 2 end;
    when x == z+2 do z := 4 end;
    test := (x+1 == z)
    """
  
  def main(args : Array[String]) : Unit = {
    try {
    	val p  = Parser.parse(example)
        println("---  after parsing ---\n")
        println(Pretty.render(Pretty.pretty(p)))
     
        val dp = Desugarer.desugar(p)
        println("\n---  after desugaring ---\n")
    	println(Pretty.render(Pretty.pretty(dp)))
     
        val s = Interpreter.eval(dp)
        println("\n---  after evaluation ---\n")
        println(Pretty.render(Pretty.pretty(s)))
    } catch { 
      case e => print(e) 
    }
  }
}
