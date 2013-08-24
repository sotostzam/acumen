package acumen
package testutil

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck.Properties

import Generators.{
  genDistinctSetOfN, arbName
}

object ProgGeneratorTest extends Properties("ProgGenerator") {

  val pg = new ProgGenerator( maxConditionalsPerScope = 1
                            , maxSimulationTime       = 10)
  import pg._
  
  /** Checks that no scope in a program contains more than the preset maximum number of conditionals. */
  property("conditionals per scope") = {
    def maxConditionalsInScope(as: Seq[Action]): Int = as.map(_ match {
      case IfThenElse(_, t, e) =>
        Math.max(1, Math.max(maxConditionalsInScope(t), maxConditionalsInScope(e)))
      case Switch(_, as) =>
        as.foldRight(1)((a, m) => Math.max(m, maxConditionalsInScope(a.rhs)))
      case ForEach(_, _, as) =>
        Math.max(1, maxConditionalsInScope(as))
      case Continuously(_) =>
        0
      case Discretely(_) =>
        0
    }).sum
    forAllNoShrink( genProg ) {
      !_.defs.exists(d => maxConditionalsInScope(d.body) > pg.maxConditionalsPerScope)
    }
  }
  
  property("genCompleteNameSet") = 
    forAll(chooseNum[Int](0,50)) { (size: Int) =>
      forAll(genCompleteNameSet(size)) { (ns: Set[Name]) =>
        ns.forall(n => (0 to n.primes).forall(p => ns.contains(Name(n.x, p))))
      }
    }
    
  property("genDistinctSetOfN") =
    forAll(chooseNum[Int](0,10000)) { (size: Int) =>
      forAll(genDistinctSetOfN(size, choose(0,10000))) { (l: Set[Int]) =>
        l.size == l.toSet.size
      }
    }
  
}