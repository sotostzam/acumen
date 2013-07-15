package acumen
package interpreters
package imperative

import Pretty._

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Suite

import Common._

import java.io.FileInputStream
import java.io.InputStreamReader

class ImperativeInterpreterTest extends ParallelInterpreterTest {
  override def interpreter : CStoreInterpreter = ImperativeInterpreter

  override def run(in: InputStreamReader) = {    
    val ast = Parser.run(Parser.prog, in)
    val des = Desugarer.run(ast)
    for (_ <- (ImperativeInterpreter.run(des).ctrace)) ()
  }
}
