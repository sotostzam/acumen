package acumen.interpreters.semanticstest

import java.io.{BufferedReader,File,FileInputStream,InputStreamReader}
import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import acumen._

abstract class SemanticsTestBase extends FunSuite with ShouldMatchers {
  def semantics : SemanticsImpl.CStore

  def run(in: InputStreamReader) = {    
    val ast = semantics.parse(in)
    val des = semantics.applyRequiredPasses(ast)
    val i = semantics.interpreter()
    for (_ <- (i.lazyRun(des).ctrace)) ()
  }

  def run(filename:String) : Unit = {
    val in  = 
      new InputStreamReader(SemanticsTestBase.this.getClass.getResourceAsStream("/acumen/"+filename))
    run(in)
  }
    
  def run(f: File) : Unit = {
    run(new InputStreamReader(new FileInputStream(f)))
  }

  def runWithInterpreter(in: InputStreamReader, si: SemanticsImpl.CStore) = {
    val ast = si.parse(in)
    val tr = si.applyPasses(ast, Nil)
    val i = si.interpreter()
    i run tr
  }

  def examples : Examples
  def examplesSkip (path: String) = false
  final def testExamples = {
    // Note: 
    //  - To add result file for newly created examples use: sbt "run record-reference-outputs"
    //  - To update existing result file remove the file and then use the above command
    val ex = examples
    val skip = examplesSkip(_)
    ex.cstoreExamplesAction{(dn, f) =>
      //info(f.toString)
      val testName = "example " + f
      val resFile = ex.resultFile(ex.expectLoc, dn, f)
      lazy val hasMain = resFile.exists && {
        val reader = new BufferedReader(new InputStreamReader(new FileInputStream(resFile)))
        val firstLine = reader.readLine
        firstLine != "NO MAIN"
      }
      if (skip(f.toString)) {
        ignore(testName) {}
      } else if (hasMain)
        test(testName) { 
          val gotLoc = ex.gotLoc + "/" + semantics.id.mkString("-")
          ex.writeExampleResult(gotLoc, dn, f, semantics)
          class Result(loc: String) {
            val file = ex.resultFile(loc, dn, f)
            val reader = new BufferedReader(new InputStreamReader(new FileInputStream(file)))
            var curLine : String = null
            def adv = curLine = reader.readLine
          }
          val expect = new Result(ex.expectLoc)
          val got = new Result(gotLoc)
          do {
            expect.adv; got.adv
            if (expect.curLine != got.curLine)
              fail("regression failure, to see difference run: diff -u " + expect.file + " " + got.file)
          } while (expect.curLine != null && got.curLine != null)
      } else if (!resFile.exists) {
        info("Can't find result file for " + f + ", skipping.")
        ignore(testName) {}
      }
    }
  }
  
  def shouldRun = List("shouldRun1.acm", "shouldRun2.acm", "shouldRun3.acm",
                       "shouldRun4.acm", "shouldRun5.acm", "shouldRun6.acm",
                       "classNameFirstClass1.acm", "let1.acm", "sumWithoutFilter.acm",
                       "vectorIdx.acm")
  final def testShouldRun = {
    for (fn <- shouldRun) {
      test(fn) {run("data/ShouldRun/" + fn) should be ()}
    }
  }

  def testShouldFail;

  testShouldRun
  testShouldFail
  testExamples

}

