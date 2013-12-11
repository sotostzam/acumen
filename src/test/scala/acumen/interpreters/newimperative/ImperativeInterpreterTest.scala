package acumen
package interpreters
package newimperative

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

import Pretty._

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Suite

import java.io.FileInputStream
import java.io.InputStreamReader

import Common.ObjId

class ImperativeInterpreterTest extends InterpreterTestBase {
  override def interpreter : CStoreInterpreter = ImperativeInterpreter

  override def suiteName = "New ImperativeInterpreterTest"

  override def run(in: InputStreamReader) = {    
    val ast = Parser.run(Parser.prog, in)
    val des = Desugarer().run(ast)
    for (_ <- (ImperativeInterpreter.run(des).ctrace)) ()
  }

  test("StoreConversions1") {
    import ImperativeInterpreter._
    val txt = """
      #0 { className = Main, parent = none, 
					 nextChild = 0, seed1 = 0, seed2 = 1 }
    """
    val cst = Parser.run(Parser.store, txt)
    val st = fromCStore(cst,CId())
    cst should be (repr(st))
  }
  
  test("StoreConversions2") {
    import ImperativeInterpreter._
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
    import ImperativeInterpreter._
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

  testExamples()
  testShouldRun

  def getError(file:String) : Option[AcumenError] = {
    try { run(file) ; None }
    catch { case e:AcumenError => Some(e) }
  }

  test("Error1") {
    val err = ClassNotDefined(cmain)
    getError("data/ShouldCrash/Error1.acm") should be (Some(err))
  }
  test("Error2") {
    val err = VariableNotDeclared(name("y"))
    getError("data/ShouldCrash/Error2.acm") should be (Some(err))
  }
  test("Error3") {
    val err = VariableNotDeclared(name("x"))
    getError("data/ShouldCrash/Error3.acm") should be (Some(err))
  }
  test("Error4") {
    val err = UnknownOperator("f")
    getError("data/ShouldCrash/Error4.acm") should be (Some(err))
  }
  test("Error5") {
    val err = NotAnObject(VLit(GInt(1)))
    getError("data/ShouldCrash/Error5.acm") should be (Some(err))
  }
  test("Error6") {
    val err = NotAnObject(VLit(GInt(1)))
    getError("data/ShouldCrash/Error6.acm") should be (Some(err))
  }
  test("Error7") {
    evaluating {run("data/ShouldCrash/Error7.acm")} should produce [AccessDenied[ObjId]]
  }
  test("Error8 ") {
    evaluating {run("data/ShouldCrash/Error8.acm")} should produce [AccessDenied[ObjId]]
  }
  test("Error9") {
    evaluating {run("data/ShouldCrash/Error9.acm")} should produce [NotAChildOf[ObjId]]
  }
  test("Error10 ") {
    val err = ClassNotDefined(ClassName("B"))
    getError("data/ShouldCrash/Error10.acm") should be (Some(err))
  }
  test("Error11 ") {
    val err = ClassDefinedTwice(ClassName("A"))
    getError("data/ShouldCrash/Error11.acm") should be (Some(err))
  }
  test("ACUMEN-348") {
    val err = DuplicateAssingmentUnspecified(Name("period",0))
    getError("data/ShouldCrash/ACUMEN-348.acm") should be (Some(err))
  }

}

