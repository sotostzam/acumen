package acumen.interpreters.reference2013

import acumen._
import acumen.Errors._
import acumen.interpreters.semanticstest._

class Reference2013SemanticsTest extends SemanticsTestBase with BasicErrorNoPosTests {

  override def semantics = SemanticsImpl.Ref2013
  override def examples = Examples2013

  test("ACUMEN-348") {
    val err = DuplicateDiscreteAssingment(Name("period",0))
    getError("data/ShouldCrash/ACUMEN-348.acm") should be (Some(err))
  }

}

