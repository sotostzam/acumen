package acumen.interpreters.enclosure

import org.scalacheck.Properties
import org.scalacheck.Prop._
import org.scalacheck.Gen._
import Util._

object UtilTest extends Properties("Util") {

  property("zipDefault") =
    forAll(posNum[Int]) { n =>
      forAll(listOfN(n, posNum[Int]), listOfN(n, posNum[Int])) { (a, b) =>
        val aMap = (for { v <- a } yield ("t_" + v, v)).toMap
        val bMap = (for { v <- b } yield ("t_" + v, v)).toMap
        forAll(oneOf(aMap.keys.toList), oneOf(bMap.keys.toList)) { (ak, bk) =>
          val zd = zipDefault(aMap, bMap, -1)
          val (al,ar) = zd(ak) 
          val (bl,br) = zd(ak)
          (al == aMap(ak) || al == -1) && (br == bMap(bk) || br == -1)
        }
      }
    }

}