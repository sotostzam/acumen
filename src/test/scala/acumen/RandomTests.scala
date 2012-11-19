package acumen
package tests

import Pretty._
import Generators._
import org.scalacheck.Properties
import org.scalacheck.Prop._
import acumen._

object RandomTests extends Properties("acumen") {

  property("parse-pretty consistency") =
    forAll { (x:Prog) =>
      val px = pprint(x)
      try { 
        (Parser.run(Parser.prog, px) == x) :| ("pretty:\n" + px)
      } catch {
        case e => false :| ("res:\n" + e +"\npretty:\n" + px)
      }
    }
}
