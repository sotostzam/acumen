package acumen
package testutil

import org.scalacheck._
import Gen._
import Arbitrary.arbitrary

import org.scalacheck.Properties
import org.scalacheck.Prop._

import acumen.testutil.Generators.{ 
  arbName
} 

import scala.Array.{ ofDim }

/**
 * Class for generating systems of ODEs which are guaranteed to have (known) solutions.
 * 
 * This is useful when generating Acumen models which can be seen as encodings of piecewise 
 * systems of differential equations.
 * 
 * 1. Generate complete set of n variables (e.g. x, x', y, with n = 3).
 * 2. Generate an n x n matrix in the following way:
 *      a. Randomize n (nonzero?) numbers as diagonal elements.
 *      b. Set lower triangle to all 0s.
 *      c. Randomize upper triangle.
 * 3. This matrix corresponds to a homogeneous system of linear ordinary differential equations.
 * 4. This system will have a solution for any initial conditions chosen!
 */
object EquationSystemGenerator extends Properties("EquationSystemGenerator") {

  def genSmallDouble = chooseNum[Double](-1000.0,1000.0)
  
  /**
   * Generate upper triangular matrix with nonzero diagonal elements.
   * This matrix corresponds to a homogeneous system of linear ordinary differential equations.
   */
  def genUpperTriangularMatrix(dim: Int): Gen[Array[Array[Double]]] = 
    for {
      diagonal <- listOfN(dim, genSmallDouble suchThat (_ != 0))
      rows <- containerOfN[Array, Double](dim, genSmallDouble)
    } yield {
      val matrix = ofDim[Double](dim, dim)
      for { row <- 0 until dim; col <- 0 until dim }
        matrix(row)(col) =
          if (row < col) rows(row)
          else if (row == col) diagonal(row)
          else 0
      matrix
    }
    
  property("genUpperTriangularMatrix") =
    forAllNoShrink(chooseNum[Int](1, 50)) { (dim: Int) =>
      forAllNoShrink(genUpperTriangularMatrix(dim)) { (m: Array[Array[Double]]) =>
        var lowerPartAreZeros = true
        var diagonalIsNonZero = true
        for (row <- 0 until dim) {
          if (m(row)(row) == 0) diagonalIsNonZero = false
          for (col <- 0 until dim)
            if (row > col && m(row)(col) != 0)
              lowerPartAreZeros = false
        }
        lowerPartAreZeros && diagonalIsNonZero
      }
    }
    
  /**
   * Generate set of distinct variable Names that is complete, in the sense that if the set 
   * contains a Name(n,primes) it will also contain all Name(n,p), where 0 <= p < primes.
   */
  def genCompleteNameSet(minSize: Int): Gen[Set[Name]] =
    for { initial <- genDistinctListOfN(minSize, arbitrary[Name]) } yield completeNames(initial).toSet
  
  property("genCompleteNameSet") = 
    forAll(chooseNum[Int](0,50)) { (size: Int) =>
      forAll(genCompleteNameSet(size)) { (ns: Set[Name]) =>
        ns.forall(n => (0 to n.primes).forall(p => ns.contains(Name(n.x, p))))
      }
    }
    
  /** Generates a list of distinct E */
  def genDistinctListOfN[E](size: Int, gen: => Gen[E]): Gen[List[E]] =
    if (size == 0) Nil
    else for {
      init <- listOfN(size, gen)
      distinct = init.toSet.toList
      rest <- genDistinctListOfN(size - distinct.size, gen) suchThat (_.forall(!distinct.contains(_)))
    } yield distinct ++ rest
  
  property("genDistinctListOfN") =
    forAll(chooseNum[Int](0,10000)) { (size: Int) =>
      forAll(genDistinctListOfN(size, choose(0,10000))) { (l: List[Int]) =>
        l.size == l.toSet.size
      }
    }

  /**
   * Given a list of Names, completes the list by adding names with lower primes.
   * E.g. the input:
   *   List(Name(2,"x"), Name(0,"y")) 
   * yields
   *   List(Name(1,"x"), Name(1,"x"), Name(2,"x"), Name(0,"y"))  
   */
  def completeNames(l: List[Name]): List[Name] =
    l.flatMap(n => if(n.primes == 0) List(n) else for(p <- 0 to n.primes) yield Name(n.x, p))
     .toSet.toList
     
  property("completeNames") =
    forAll { (l: List[Name]) =>
      val cn = completeNames(l)
      cn.forall(n => (0 to n.primes).forall(p => cn.contains(Name(n.x, p))))
    }

  /**
   * Generates a list of n elements using genElem, each time passing a new 
   * element of the input list to genElem.
   */
  def listBasedOn[P, E](params: List[P], gen: P => Gen[E]): Gen[List[E]] =
    if (params.isEmpty) Nil
    else for {
      head <- gen(params.head)
      tail <- listBasedOn(params.tail, gen)
    } yield head +: tail
  
  def printMatrix[T](m: Array[Array[T]]) = {
    require(m.size > 0 && m(0).size > 0)
    for(row <- 0 until m.size) {
      for(col <- 0 until m(0).size)
        print(m(row)(col) + "\t")
      println()
    }
  }
    
}