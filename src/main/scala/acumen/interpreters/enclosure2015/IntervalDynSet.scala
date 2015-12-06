package acumen
package interpreters
package enclosure2015

import enclosure.Interval
import enclosure2015.Common._
import interpreters.Common._
import Errors._

abstract class IntervalDynSet extends RealVector
{  
  implicit val cValueIsReal: Real[CValue] = acumen.intervalCValueIsReal
  
  val zero = VLit(GConstantRealEnclosure(Interval.zero))
  val one  = VLit(GConstantRealEnclosure(Interval.one))
    
  val dim: Int
  
  def init(v: RealVector): IntervalDynSet
  
  def apply(i: Int): CValue
  
  def set(i: Int, v: CValue): IntervalDynSet
  
  def map(m: CValue => CValue)        : IntervalDynSet
  def map(m: (Int, CValue) => CValue) : IntervalDynSet
  
  def move(f: Mapping) : IntervalDynSet
  def move(f: C1Flow)  : (IntervalDynSet, IntervalDynSet)
  
  // TODO this is a stub to enabl development on a higher level
  def contains(that: IntervalDynSet): Boolean = (0 until dim).forall {
    i => val (VLit(GConstantRealEnclosure(i1)), VLit(GConstantRealEnclosure(i2))) = (this(i), that(i))
         i1 contains i2 }
  
  /* RealVector interface */
  val outerEnclosure: RealVector
  def repr = outerEnclosure.repr
  def activeIterator = outerEnclosure.activeIterator
  def activeKeysIterator = outerEnclosure.activeKeysIterator   
  def activeValuesIterator = outerEnclosure.activeValuesIterator 
  def activeSize = outerEnclosure.activeSize
  def length: Int = dim
  def update(i: Int,v: acumen.CValue) = throw ShouldNeverHappen()         
  def copy = outerEnclosure.copy
}

/* Box */

/** Implementation of the IntervalBox */
case class IntervalBox(v: RealVector) extends IntervalDynSet {
  
  val dim = v.size

  val outerEnclosure = v
  
  def init(v: RealVector) = IntervalBox(v: RealVector)
  
  def apply(i: Int) = v(i)
  
  def set(i: Int, c: CValue): IntervalBox = {
    val v1 = v.copy
    v1.update(i, c)
    IntervalBox(v1)
  }
  
  def map(m: CValue => CValue): IntervalBox = IntervalBox(v.copy.map(m))
    
  def map(m: (Int, CValue) => CValue): IntervalBox = {
    // TODO implement with UFunc
    def mapVector(v: RealVector) = breeze.linalg.Vector.tabulate[CValue](dim) { i      => m(i, v(i)) }
    IntervalBox(mapVector(v))
  }
    
  def move(f: Mapping): IntervalBox = IntervalBox(f(v))
          
  def move(f: C1Flow): (IntervalBox, IntervalBox) = (IntervalBox(f(v)), IntervalBox(f.range(v)))
}

/* Cuboid */

/** Constructing a Cuboid from a RealVector */
object Cuboid {
  def apply(v: RealVector)(implicit cValueIsReal: Real[CValue]): Cuboid = {
    val dim  = v.size
    val zero = cValueIsReal.zero
    val one  = cValueIsReal.one

    val imageMidpoint             : RealVector = midpointVector(v)
    val imageWidth                : RealVector = centeredVector(v)
    val imageLinearTransformation : RealMatrix = breeze.linalg.DenseMatrix.eye[CValue](dim)    
    val imageError                : RealVector = breeze.linalg.Vector.zeros[CValue](dim)
    
    Cuboid(imageMidpoint, imageLinearTransformation, imageWidth, imageError)
  }
}

/** Implementation of the Cuboid */
case class Cuboid
  ( midpoint             : RealVector
  , linearTransformation : RealMatrix
  , width                : RealVector
  , error                : RealVector )
    extends IntervalDynSet
  {
    // FIXME is there a nicer way to do this?
    assert(Set(midpoint.size, linearTransformation.rows, linearTransformation.cols, width.size, error.size).size == 1)
    
    val dim = midpoint.size
    
    lazy val outerEnclosure = midpoint + linearTransformation * width + error
    
    def init(v: RealVector) = Cuboid(v)
    
    def apply(i: Int) = outerEnclosure(i)
    
    // FIXME Can this be done more cleverly? (updating the all the terms individually to avoid wrapping)
    def set(i: Int, c: CValue): Cuboid = {
      val outerEnclosure1 = outerEnclosure.copy
      outerEnclosure1.update(i, c)
      Cuboid(outerEnclosure1)
    }
    
    def map(m: CValue => CValue): Cuboid = {
      val imageMidpoint             : RealVector = midpoint.copy.map(m)
      val imageWidth                : RealVector = width.copy.map(m)
      val imageLinearTransformation : RealMatrix = breeze.linalg.Matrix.tabulate(linearTransformation.rows, linearTransformation.cols){ 
                                                     case (r, c) => m(linearTransformation(r, c)) 
                                                   }    
      val imageError                : RealVector = error.copy.map(m)
      
      Cuboid(imageMidpoint, imageLinearTransformation, imageWidth, imageError)
    }
    
    def map(m: (Int, CValue) => CValue): Cuboid = {
      // TODO implement with UFunc
      def mapVector(v:   RealVector) = breeze.linalg.Vector.tabulate[CValue](dim)      { i      => m(i, v(i)) }
      def mapMatrix(mtx: RealMatrix) = breeze.linalg.Matrix.tabulate[CValue](dim, dim) { (r, c) => m(c, mtx(r, c)) } // Columns correspond to indices 
      val imageMidpoint             : RealVector = mapVector(midpoint)
      val imageWidth                : RealVector = mapVector(width)
      val imageLinearTransformation : RealMatrix = mapMatrix(linearTransformation)    
      val imageError                : RealVector = mapVector(error)
      
      Cuboid(imageMidpoint, imageLinearTransformation, imageWidth, imageError)
    }
    
    def move(f: Mapping): Cuboid = Cuboid(f(outerEnclosure))
          
    def move(f: C1Flow): (Cuboid, Cuboid) = {
      val coarseRangeEnclosure = f.range(outerEnclosure)
      
      val phi                  = f.phi(midpoint)
      val jacobian             = f.jacPhi(outerEnclosure)
      val remainder            = f.remainder(coarseRangeEnclosure)
      
      val phiPlusRemainder: RealVector = phi + remainder

      val imageMidpoint             : RealVector = midpointVector(phiPlusRemainder)
      val imageWidth                : RealVector = width
      val imageLinearTransformation : RealMatrix = jacobian * linearTransformation
      val imageError                : RealVector = jacobian * error + 
                                                   centeredVector(phiPlusRemainder)
      lazy val refinedRangeEnclosure = {
        val rangeMidpoint  : RealVector = f.range.phi(midpoint)
        val rangeJacobian  : RealMatrix = f.range.jacPhi(outerEnclosure)
        val rangeRemainder : RealVector = f.range.remainder(coarseRangeEnclosure)
        
        rangeMidpoint + rangeJacobian * (linearTransformation * width + error) + rangeRemainder 
      }
  
      ( Cuboid(refinedRangeEnclosure)
      , Cuboid(imageMidpoint, imageLinearTransformation, imageWidth, imageError) )
    }
}
