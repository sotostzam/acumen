package acumen
package interpreters
package imperative
package sequential

import Pretty._

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Suite

import Interpreter._

import java.io.FileInputStream
import java.io.InputStreamReader

class InterpreterTest extends parallel.InterpreterTest {
  override def suiteName = "Sequential InterpreterTest"
  
  val instance = new Interpreter

  override def runInterpreter(p: Prog) = instance.run(p)

  override def run(in: InputStreamReader) = {    
    val ast = Parser.run(Parser.prog, in)
    val des = Desugarer.run(ast)
    for (_ <- (instance.run(des).ctrace)) ()
  }
}
