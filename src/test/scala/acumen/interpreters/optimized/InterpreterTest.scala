package acumen
package interpreters
package optimized

import Errors._
import util.Filters._
import util.Names._
import util.Canonical._
import scala.math._
import java.io.InputStreamReader
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FunSuite
import java.io.File

import Pretty._

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Suite

import java.io.FileInputStream
import java.io.InputStreamReader

import Common.ObjId

class InterpreterTest extends InterpreterTestBase {
  override def semantics = SemanticsImpl.Optimized()

  override def suiteName = "Optimized InterpreterTest"

  test("StoreConversions1") {
    import Interpreter._
    val txt = """
      #0 { className = Main, parent = none, 
					 nextChild = 0, seed1 = 0, seed2 = 1 }
    """
    val cst = Parser.run(Parser.store, txt)
    val st = fromCStore(cst,CId())
    cst should be (repr(st))
  }
  
  test("StoreConversions2") {
    import Interpreter._
    val txt = """
      #0   { className = Main, parent = none, 
						 nextChild = 3, seed1 = 0, seed2 = 1 }
      #0.1 { className = A, parent = #0, o = #0.2, 
						 nextChild = 0, seed1 = 2, seed2 = 3 }
      #0.2 { className = A, parent = #0, o = #0.1, 
						 nextChild = 0, seed1 = 4, seed2 = 5 }
      """
    val cst = Parser.run(Parser.store, txt)
    val st = fromCStore(cst,CId())
    cst should be (repr(st))
  }

  test("StoreConversions3") {
    import Interpreter._
    val txt = """
#0.1 {
  parent = #0.2,
  time = 7.596000000000177,
  className = Simulator,
  resultType = @Discrete,
  endTime = 10.0,
  timeStep = 0.0030,
  nextChild = 0,
	seed1 = 0, 
	seed2 = 1
}
#0.2 {
  parent = none,
  mode = "Persist",
  className = Main,
  simulation = #0.1,
  nextChild = 0,
	seed1 = 2, 
	seed2 = 3
}
#0.4 {
  x' = 14.112000000000132,
  parent = #0.2,
  x'' = 9.8,
  x = 89.72769879999998,
  mode = "Fly",
  className = Ball,
  nextChild = 0,
	seed1 = 4, 
	seed2 = 5
}
#0.5 {
  x' = 14.112000000000132,
  parent = #0.2,
  x'' = 9.8,
  x = 89.72769879999998,
  mode = "Fly",
  className = Ball,
  nextChild = 0,
	seed1 = 6, 
	seed2 = 7
}
"""
    val cst = Parser.run(Parser.store, txt)
    val st = fromCStore(cst,CId(2))
    cst should be (repr(st))
  }

  def eqstreams(s1:Seq[CStore], s2:Seq[CStore]) : Boolean= {
    var t1 = s1
    var t2 = s2 
    var break = false
    while (t1.nonEmpty && t2.nonEmpty && !break) {
      val h1 = t1.head
      val h2 = t2.head
      t1 = t1.tail
      t2 = t2.tail
      if (h1 != h2) {
        println("*" * 30)
        println(pprint(prettyStore(h1)))
        println("!=" * 10)
        println(pprint(prettyStore(h2)))
        println("*" * 30)
        break = true
      }
    }
    if (break) false
    else t1.isEmpty && t2.isEmpty
  }

  testExamples(Examples2013)
  testShouldRun

  def getError(file:String) : Option[PositionalAcumenError] = {
    try { run(file) ; None }
    catch { case e:PositionalAcumenError => Some(e) }
  }

  def testForError(file:String, err: PositionalAcumenError, pos: String) = {
    val res = getError(file) 
    res should be (Some(err))
    res.get.pos.toString should equal (pos)
  }

  test("Error1") {
    val err = ClassNotDefined(cmain)
    getError("data/ShouldCrash/Error1.acm") should be (Some(err))
  }
  test("Error2") {
    testForError("data/ShouldCrash/Error2.acm", VariableNotDeclared(name("y")), "3.12")
  }
  test("Error3") {
    testForError("data/ShouldCrash/Error3.acm", VariableNotDeclared(name("x")), "2.27")
  }
  test("Error4") {
    testForError("data/ShouldCrash/Error4.acm", UnknownOperator("f"), "3.45")
  }
  test("Error5") {
    testForError("data/ShouldCrash/Error5.acm", NotAnObject(VLit(GInt(1))), "3.24")
  }
  test("Error6") {
    testForError("data/ShouldCrash/Error6.acm", NotAnObject(VLit(GInt(1))), "2.38")
  }
  test("Error7") {
    val err = evaluating {run("data/ShouldCrash/Error7.acm")} should produce [AccessDenied[ObjId]]
    err.pos.toString should be ("8.3")
  }
  test("Error8 ") {
    val err = evaluating {run("data/ShouldCrash/Error8.acm")} should produce [AccessDenied[ObjId]]
    err.pos.toString should be ("27.15")
  }
  test("Error9") {
    val err = evaluating {run("data/ShouldCrash/Error9.acm")} should produce [NotAChildOf[ObjId]]
    err.pos.toString should be ("33.12")
  }
  test("Error10 ") {
    testForError("data/ShouldCrash/Error10.acm", ClassNotDefined(ClassName("B")), "15.25")
  }
  test("Error11 ") {
    val err = ClassDefinedTwice(ClassName("A"))
    getError("data/ShouldCrash/Error11.acm") should be (Some(err))
    // No line number
  }
  test("ACUMEN-348") {
    testForError("data/ShouldCrash/ACUMEN-348.acm", DuplicateAssingmentUnspecified(Name("period",0)), "14.5")
  }

}

