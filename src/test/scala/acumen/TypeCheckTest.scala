package acumen
package tests

import Errors._
import util.Filters._
import util.Names._
import util.Canonical._
import scala.math._
import java.io.InputStreamReader
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FunSuite
import java.io.File
import util.Transform
import interpreters.reference2013.Interpreter

// FIXME: Fixup test so it works again
//        Test should run the type checker on all the examples
//        and make sure it runs without error.

// class TypeCheckTest extends InterpreterTestBase with ShouldMatchers {

//   def run(in: InputStreamReader) : Unit = {
//     val ast = Parser.run(Parser.prog, in)
//     val desugared = Desugarer().run(ast)
//     val res = new TypeCheck(desugared).run()
//     // FIXME: There should be a better way to indicate that the test failed
//     if (res >= TypeCheck.EL_ERROR) throw new Exception("Type Check Failed")
//   }

//   testExamples
// }
