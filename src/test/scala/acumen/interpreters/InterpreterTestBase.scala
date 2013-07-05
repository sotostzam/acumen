package acumen
package interpreters

import Errors._
import util.Filters._
import util.Names._
import util.Canonical._
import scala.math._
import java.io.{FileInputStream, InputStreamReader, BufferedReader}
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FunSuite
import java.io.File
import util.Transform

abstract class InterpreterTestBase extends FunSuite with ShouldMatchers {
  def runInterpreter(p: Prog) : CStoreRes 

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

  def testExamples(skip: Seq[String] = Nil) = {
    // Note: 
    //  - To add result file for newly created examples use: sbt "run . examples"
    //  - To update existing result file remove the file and then use the above command
    Examples.cstoreExamplesAction{(dn, f) =>
      //info(f.toString)
      val testName = "example " + f
      val resFile = Examples.resultFile(Examples.expectLoc, dn, f)
      if (skip.contains(f.toString)) {
        ignore(testName) {}
      } else if (resFile.exists)
        test(testName) { 
          Examples.writeExampleResult(Examples.gotLoc, dn, f, runInterpreter(_))
          class Result(loc: String) {
            val file = Examples.resultFile(loc, dn, f)
            val reader = new BufferedReader(new InputStreamReader(new FileInputStream(file)))
            var curLine : String = null
            def adv = curLine = reader.readLine
          }
          val expect = new Result(Examples.expectLoc)
          val got = new Result(Examples.gotLoc)
          do {
            expect.adv; got.adv
            if (expect.curLine != got.curLine)
              fail("regression failure, to see difference run: diff -u " + expect.file + " " + got.file)
          } while (expect.curLine != null && got.curLine != null)
      } else {
        info("Can't find result file for " + f + ", skipping.")
        ignore(testName) {}
      }
    }
  }

  def testShouldRun = {
    var toTest = List("shouldRun1.acm", "shouldRun2.acm", "shouldRun3.acm",
                      "shouldRun4.acm", "shouldRun5.acm", "shouldRun6.acm")
    for (fn <- toTest) {
      test(fn) {run("data/ShouldRun/" + fn) should be ()}
    }
  }
}

