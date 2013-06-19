package acumen
package interpreters

import Errors._
import util.Filters._
import util.Names._
import util.Canonical._
import scala.math._
import java.io.FileInputStream
import java.io.InputStreamReader
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FunSuite
import java.io.File
import util.Transform

abstract class InterpreterTestBase extends FunSuite with ShouldMatchers {
  def run(in: InputStreamReader) : Unit

  def run(filename:String) : Unit = {
    val in  = 
      new InputStreamReader(this.getClass.getResourceAsStream("/acumen/"+filename))
    run(in)
  }
    
  def run(f: File) : Unit = {
    run(new InputStreamReader(new FileInputStream(f)))
  }

  def runWithInterpreter(in: InputStreamReader, i: Interpreter) = {
    val ast = Parser.run(Parser.prog, in)
    val tr = Transform.transform(ast)
    i run tr
  }

  def testExampleDir(d: File) : Unit = {
    def filter = new java.io.FilenameFilter {
      def accept(d: File, fn: String) = {
        fn.substring(0,3) 		 != "XXX" && // Ignore internal files
        d.getName.substring(0,3) != "XXX" && // Ignore internal directories
        d.getName 				 != "01_Enclosures" && //FIXME Support enclosure sim. params in CStore interpreters 
        d.getName 				 != "02_Robust_Simulation" //FIXME Support enclosure sim. params in CStore interpreters 
      }
    }
    for (f <- d.listFiles(filter)) {
      def fn = f.getName
      if (f.isDirectory) testExampleDir(f)
      else if (fn.endsWith(".acm")) test("example " + d + File.separator + fn) { run(f) should be () }
    }
  }

  def testExamples = {
    testExampleDir(new File("examples"))
  }

  def testShouldRun = {
    var toTest = List("shouldRun1.acm", "shouldRun2.acm", "shouldRun3.acm",
                      "shouldRun4.acm", "shouldRun5.acm", "shouldRun6.acm")
    for (fn <- toTest) {
      test(fn) {run("data/ShouldRun/" + fn) should be ()}
    }
  }
}

