package acumen.interpreters.imperative2012

import acumen._
import acumen.interpreters.semanticstest._

class Imperative2012SemanticsTest extends SemanticsTestBase {

  override def semantics = SemanticsImpl.Imperative2012
  override def examples = Examples2012
  override def testShouldFail = {}

}
