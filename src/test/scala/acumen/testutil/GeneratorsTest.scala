package acumen
package testutil

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck.Properties

import Generators.{
  arbName, completeNames
}

object GeneratorsTest extends Properties("Generator") {
     
  property("completeNames") =
    forAll { (l: Set[Name]) =>
      val cn = completeNames(l)
      cn.forall(n => (0 to n.primes).forall(p => cn.contains(Name(n.x, p))))
    } 
  
}