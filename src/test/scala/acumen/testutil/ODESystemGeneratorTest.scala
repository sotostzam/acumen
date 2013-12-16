package acumen
package testutil

import scala.collection.mutable.{ Map => MutMap }
import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck.Properties
import org.scalacheck.Shrink._
import Gen._
import Shrink._
import Arbitrary.arbitrary
import org.scalacheck.Properties
import org.scalacheck.Prop._
import ODESystemGenerator._
import Generators._

object ODESystemGeneratorTest extends Properties("ODESystemGenerator") {

  property("The LHS of an equation does not occur in its RHS.") =
    forAllNoShrink(chooseNum[Int](2, 20)) { n =>
      forAllNoShrink(genDistinctSetOfN(n, arbitrary[Dot])) { ds =>
        forAllNoShrink(oneOf(ds.toList)) { lhs =>
          forAll(genLinearODE(ds)(lhs)) { equation =>
            !equation.rhs.toString.contains(equation.lhs.toString) 
          }
        }
      }
    }
  
} 
