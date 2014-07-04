package acumen.interpreters.semanticstest

import acumen._
import acumen.Errors._
import acumen.testutil.TestUtil.assertEqualTrace

trait Traditional2014Tests extends SemanticsTestBase with BasicErrorTests {

  override def examples = Examples2014
  override def examplesSkip(f: String) = {
    // The following models need closer investigation
    // to make sure they still have the correct output.
    // Once this is done the reference outputs in
    // src/test/resources/acumen/data/examples-2014-res
    // should be updated.
    f.endsWith("/Quantization - Linear.acm") ||
    f.endsWith("/01_Converting_Accelerations.acm") ||
    f.startsWith("examples/XXX_internal/0_Demos/") || 
    // The ping-pong models need to be fixed.
    f.startsWith("examples/XXX_internal/test/ping-pong/")
  }

  test("continuous assignments are independent") {
    val head = "class Main(simulator)\n  private x := 0; t := 0; t' := 1 end "
    val cond = "if t < simulator.timeStep x = 1 else x = -1 end;"
    val time = "t' = 1;"
    val tail = "end"
    val timeFirst = Parser.run(Parser.prog, head ++ time ++ cond ++ tail)
    val condFirst = Parser.run(Parser.prog, head ++ cond ++ time ++ tail)
    assertEqualTrace(timeFirst, condFirst, semantics)
  }

  test("ACUMEN-348") {
    val err = evaluating {run("data/ShouldCrash/ACUMEN-348.acm")} should produce [DuplicateAssingment]
    err.x should be (Name("period",0))
    err.pos.toString should be ("14.5")
  }

}
