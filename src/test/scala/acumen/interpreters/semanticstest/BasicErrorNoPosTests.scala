package acumen.interpreters.semanticstest

import acumen._
import acumen.Errors._
import acumen.util.Canonical.cmain
import acumen.util.Names._

trait BasicErrorNoPosTests extends SemanticsTestBase {

  def getError(file:String) : Option[AcumenError] = {
    try { run(file) ; None }
    catch { case e:AcumenError => Some(e) }
  }

  override def testShouldFail = {
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
      val err = AccessDenied(CId(), CId(1), Nil)
      getError("data/ShouldCrash/Error7.acm") should be (Some(err))
    }
    test("Error8") {
      val err = AccessDenied(CId(0,0,1), CId(1), List(CId(1,1),CId(0,1)))
      getError("data/ShouldCrash/Error8.acm") should be (Some(err))
    }
    test("Error9") {
      val err = NotAChildOf(CId(0,0,1), CId(0,1))
      getError("data/ShouldCrash/Error9.acm") should be (Some(err))
    }
    test("Error10") {
      val err = ClassNotDefined(ClassName("B"))
      getError("data/ShouldCrash/Error10.acm") should be (Some(err))
    }
    test("Error11") {
      val err = ClassDefinedTwice(ClassName("A"))
      getError("data/ShouldCrash/Error11.acm") should be (Some(err))
    }
  }

}

