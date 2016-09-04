package acumen.interpreters.semanticstest

import acumen._
import acumen.Errors._
import acumen.testutil.TestUtil.assertEqualTrace

trait Traditional2015Tests extends Traditional2014Tests {

  override def examples : Examples = Examples2015
  
  override def testShouldFail = {
    super.testShouldFail
    
    test("IndexOutBounds") {
    val err = evaluating {run("data/ShouldCrash/IndexOutBound.acm")} should produce [IndexOutOfBounds]
    err.pos.toString should be ("6.7")
  }
  
  test("NotVector") {
    val err = evaluating {run("data/ShouldCrash/NotVector.acm")} should produce [CantIndex]
    err.pos.toString should be ("5.7")
  }
  
  test("NotEnoughArguments") {
    val err = evaluating {run("data/ShouldCrash/NotEnoughArguments.acm")} should produce [ConstructorArity]
    err.pos.toString should be ("5.16")
  }
  test("ExpectInteger") {
    val err = evaluating {run("data/ShouldCrash/ExpectInteger.acm")} should produce [ExpectedInteger]
    err.pos.toString should be ("5.7")
  }
  test("AccessDeny") {
    val err = evaluating {run("data/ShouldCrash/AccessDeny.acm")} should produce [AccessDenied[_]]
    err.pos.toString should be ("11.9")
  }
   test("NoMatch") {
    val err = evaluating {run("data/ShouldCrash/NoMatch.acm")} should produce [NoMatch]
    err.pos.toString should be ("6.9")
  }
   test("NoVariable") {
    val err = evaluating {run("data/ShouldCrash/NoVariable.acm")} should produce [VariableNotDeclared]
    err.pos.toString should be ("6.7")
  }
  }
  
  
}
