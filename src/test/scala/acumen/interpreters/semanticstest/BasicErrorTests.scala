package acumen.interpreters.semanticstest

import acumen._
import acumen.Errors._
import acumen.util.Canonical.cmain
import acumen.util.Names._

trait BasicErrorTests extends SemanticsTestBase {

  def getError(file:String) : Option[PositionalAcumenError] = {
    try { run(file) ; None }
    catch { case e:PositionalAcumenError => Some(e) }
  }

  def testForError(file:String, err: PositionalAcumenError, pos: String) = {
    val res = getError(file) 
    res should be (Some(err))
    res.get.pos.toString should equal (pos)
  }

  override def testShouldFail = {
    test("Error1") {
      val err = ClassNotDefined(cmain)
      getError("data/ShouldCrash/Error1.acm") should be (Some(err))
    }
    test("Error2") {
      testForError("data/ShouldCrash/Error2.acm", VariableNotDeclared(name("y")), "3.31")
    }
    test("Error3") {
      testForError("data/ShouldCrash/Error3.acm", VariableNotDeclared(name("x")), "2.47")
    }
    test("Error4") {
      testForError("data/ShouldCrash/Error4.acm", UnknownOperator("f"), "3.65")
    }
    test("Error5") {
      testForError("data/ShouldCrash/Error5.acm", NotAnObject(VLit(GInt(1))), "3.44")
    }
    test("Error6") {
      testForError("data/ShouldCrash/Error6.acm", NotAnObject(VLit(GInt(1))), "2.58")
    }
    test("Error7") {
      val err = evaluating {run("data/ShouldCrash/Error7.acm")} should produce [AccessDenied[_]]
      err.pos.toString should be ("12.1")
    }
    test("Error8") {
      val err = evaluating {run("data/ShouldCrash/Error8.acm")} should produce [AccessDenied[_]]
      err.pos.toString should be ("26.15")
    }
    test("Error9") {
      val err = evaluating {run("data/ShouldCrash/Error9.acm")} should produce [NotAChildOf[_]]
      err.pos.toString should be ("33.12")
    }
    test("Error10") {
      testForError("data/ShouldCrash/Error10.acm", ClassNotDefined(ClassName("B")), "15.25")
    }
    test("Error11") {
      val err = ClassDefinedTwice(ClassName("A"))
      getError("data/ShouldCrash/Error11.acm") should be (Some(err))
      // No line number
    }
    
  }

}
