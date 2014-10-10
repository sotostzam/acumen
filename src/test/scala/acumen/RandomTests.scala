package acumen

import Pretty._
import testutil.Generators._
import org.scalacheck.Properties
import org.scalacheck.Prop._
import acumen._

object RandomTests extends Properties("Random") {



  property("parse-pretty consistency") = {
	import acumen.testutil.Generators.arbProg
    forAll { (x:Prog) =>
      val px = pprint(x)
      try { 
        (Parser.run(Parser.prog, px) == x) :| ("pretty:\n" + px)
      } catch {
        case e => false :| ("res:\n" + e +"\npretty:\n" + px)
      }
    }
  }
  /*
  property("parse-json consistency") =
    forAll { (c:CStore) =>
      val json = JSon.toJSON(c).toString
      try {
        (JSon.fromJSON(json) == c) :| ("json:\n" + json)
      } catch {
        case e => false :| ("res:\n" + e +"\njson:\n" + json)
      }
    }*/

}
