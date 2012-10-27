package acumen

import org.scalacheck.Properties
import org.scalacheck.Prop._
import org.scalacheck.Gen
import org.scalacheck.Gen._
import org.scalacheck.Arbitrary.arbitrary
import java.io.InputStreamReader
import scala.util.control.Exception
import acumen.interpreters.enclosure.Rounding
import acumen.interpreters.enclosure.Extract
import acumen.interpreters.enclosure.Types._
import acumen.interpreters.enclosure.solver.Systems._
import acumen.interpreters.enclosure.solver.HybridSystem

object TransformTest extends Properties("Transform") with Extract {

  import acumen.interpreters.enclosure.TestingContext._

  property("hybrid system parsing sanity") =
    forAll(oneOf(List(
      (parseFile("bouncingBall.acm"), BB(10, 0.5)),
      (parseFile("twoTanks.acm"), TT(3, 5, 10))))) {
      case (p: Option[Prog], h: HybridSystem) =>
        p match {
          case Some(p: Prog) => extract(p.defs(0)) == h
          case None => false
        }
    }

  /* Utilities */

  def parseFile(fileName: String): Option[Prog] = {
    try {
      val r = new InputStreamReader(
        this.getClass.getResourceAsStream(
          "/acumen/interpreters/enclosure/transform/" + fileName))
      try { Some(Parser.run(Parser.prog, r)) }
      finally { r.close }
    } catch {
      case e: Exception => { println("\n\n\n\n\n" + e.getMessage + "\n\n\n\n\n\n"); None }
    }
  }

}