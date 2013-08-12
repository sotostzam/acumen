package acumen

import org.scalacheck._
import Gen._
import Shrink._
import Arbitrary.arbitrary
import org.scalacheck.Properties
import org.scalacheck.Prop._
import acumen.Pretty.pprint
import testutil.ProgGenerator.{
  arbProg
}
import java.io.ByteArrayOutputStream
import java.io.PrintStream

object ExtractTest extends Properties("Extract") {

  property("semantics preserving") = forAll { (p: Prog) => extractPreservesSemanticsOf(p) }

  /**
   * Given a Prog p, computes its desugared version d. Checks that the simulation trace
   * of d is the same as that obtained by first applying Extract to d and then simulating.
   */
  def extractPreservesSemanticsOf(p: Prog) = {
    var same = false
    lazy val desugared = Desugarer.run(p)
    val e = new Extract(desugared)
    val extracted = e.res
    try {
      // set up the reference interpreter
      val i = interpreters.reference.Interpreter
      // run desugared program before transformation
      val dtrace = i.run(desugared)
      // run desugared program after transformation
      val etrace = i.run(extracted)
      // compare pretty-printed traces
      val dps = new PrintStream(new ByteArrayOutputStream)
      val eps = new PrintStream(new ByteArrayOutputStream)
      val dsample = dtrace.dumpSample(dps)
      val esample = etrace.dumpSample(eps)
      same = dps.toString == eps.toString
      same
    } catch {
      case e =>
        e.printStackTrace
        throw e
    } finally {
      if (!same) {
        System.err.println("\ndesugared: \n")
        System.err.println(pprint(desugared))
        System.err.println("\n\nextracted: \n")
        System.err.println(pprint(extracted))
      }
    }
  }

} 