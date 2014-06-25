package acumen
package interpreters
package optimized

class InterpreterTest2013 extends InterpreterTest {
  override def semantics = SemanticsImpl.Opt2013

  override def suiteName = "2013 Optimized InterpreterTest"

  override def testExamples() = testExamples(Examples2013)
}

