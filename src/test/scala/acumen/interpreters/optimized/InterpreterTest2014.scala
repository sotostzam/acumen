package acumen
package interpreters
package optimized

class InterpreterTest2014 extends InterpreterTest {
  override def semantics = SemanticsImpl.Opt2014

  override def suiteName = "2014 Optimized InterpreterTest"

  override def testExamples() =
    testExamples(Examples2014, 
               {f => // The following models need closer investigation
                     // to make sure they still have the correct output.
                     // Once this is done the reference outputs in
                     // src/test/resources/acumen/data/examples-2014-res
                     // should be updated.
                     f.endsWith("/Quantization - Linear.acm") ||
                     f.endsWith("/01_Converting_Accelerations.acm") ||
                     f.startsWith("examples/XXX_internal/0_Demos") || 
                     // This one seems okay, but the number of iterations is
                     // different
                     f.endsWith("/hodgkin-huxley.acm") ||
                     // The ping-pong models need to be fixed.
                     f.startsWith("examples/XXX_internal/test/ping-pong")})

}

