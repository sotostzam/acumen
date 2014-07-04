package acumen.interpreters.reference2012

import acumen._
import acumen.interpreters.semanticstest._

class Reference2012SemanticsTest extends SemanticsTestBase with BasicErrorNoPosTests {

  override def semantics = SemanticsImpl.Ref2012
  override def examples = Examples2012

}
