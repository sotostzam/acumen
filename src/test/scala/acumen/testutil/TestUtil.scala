package acumen
package testutil

import Pretty.{
  pprint, prettyStore
}

object TestUtil {

  def notEqual(left: CStore, right: CStore): Boolean = {
    val leftKeys = left.keySet
    val rightKeys = right.keySet
    val commonKeys = leftKeys intersect rightKeys
    val onlyLeftKeys = leftKeys -- commonKeys
    val onlyRightKeys = rightKeys -- commonKeys
    if (onlyLeftKeys.nonEmpty || onlyRightKeys.nonEmpty) {
      println(onlyLeftKeys.toList.sortWith(_ <= _) + "\n!=\n" + onlyRightKeys.toList.sortWith(_ <= _))
      true
    } else left.keys.exists(id => {
      val l = left(id)
      val r = right(id)
      if (l != r) { println(">>>> " + id + "" + left(id)); true } else false
    })
  }

  /** Checks that running p1 and p2 using i produces identical traces. */
  def assertEqualTrace(p1: Prog, p2: Prog, si: SemanticsImpl.CStore) = {
    val d1 = si.applyRequiredPasses(p1)
    val d2 = si.applyRequiredPasses(p2)
    val i = si.interpreter()
    val t1 = i.run(d1).ctrace
    val t2 = i.run(d2).ctrace
    (t1 zip t2) foreach {
      case (v1, v2) =>
        val pp1 = pprint(prettyStore(v1))
        val pp2 = pprint(prettyStore(v2))
        assert(pp1 == pp2, "CStores differ!\n\n" + sideBySideDiff(pp1, pp2)) 
    }
    val l1 = t1.length
    val l2 = t2.length
    assert(l1 == l2, "Trace lengths " + l1 + " and " + l2 + " differ!")
  }

  /** 
   * Returns a string juxtaposing expected and actual side by side, with indicators 
   * before each line where they differ.
   */
  def sideBySideDiff(expected: String, actual: String): String = {
    val s1l = expected.split("\n")
    val s2l = actual.split("\n")
    val width1 = s1l.map(_.length).max
    "Expected".padTo(width1 + 2, " ").mkString + "Actual\n\n" +
    (s1l zip s2l).map{ case(l,r) => 
      (if (l != r) "> " else "  ") + l.padTo(width1 + 2, " ").mkString + r 
    }.mkString("\n")
  }
    
  
}
