package acumen.interpreters.optimized

import acumen._
import acumen.Errors._
import acumen.interpreters.semanticstest._
import java.io.File

class Optimized2013SemanticsTest extends SemanticsTestBase with BasicErrorTests {

  override def semantics = SemanticsImpl.Opt2013
  override def examples = Examples2013

  test("ACUMEN-348") {
    testForError("data" + File.separator + "ShouldCrash" + File.separator + "ACUMEN-348.acm", DuplicateAssingmentUnspecified(Name("period",0)), "14.5")
  }

}
