package acumen.interpreters.enclosure

import org.scalacheck.Properties
import org.scalacheck.Prop._
import org.scalacheck.Gen._

import Generators._
import Util._

object UtilTest extends Properties("Util") {

  property("zipDefault") =
    forAllNoShrink(posNum[Int]) { n =>
      forAllNoShrink(listOfN(n, posNum[Int]), listOfN(n, posNum[Int])) { (a, b) =>
        val names = listOfNames(n)
        val aMap = names.zip(a).toMap
        val bMap = names.zip(b).toMap
        forAllNoShrink(oneOf(aMap.keySet.union(bMap.keySet).toList)) { k =>
          val zd = zipDefault(aMap, bMap, -1)
          val (l, r) = zd(k)
          (l == aMap(k) || l == -1) && (r == bMap(k) || r == -1)
        }
      }
    }

}