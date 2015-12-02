package acumen
package interpreters
package enclosure2015

import enclosure.Interval
import enclosure2015.Common._
import interpreters.Common._
import FAD.FDif
import TAD.TDifAsReal
import Pretty.pprint
import util._
import util.Canonical._
import util.Conversions.{
  extractDouble, extractDoubles, extractId, extractInterval, extractIntervals
}
import Errors._

abstract class IntervalDynSet
{  
  implicit val cValueIsReal: Real[CValue] = acumen.intervalCValueIsReal
  
  val zero = VLit(GConstantRealEnclosure(Interval.zero))
  val one  = VLit(GConstantRealEnclosure(Interval.one))
    
  val dim: Int
  
  val outerEnclosure: RealVector
  
  def map(m: CValue => CValue)        : IntervalDynSet
  def map(m: (Int, CValue) => CValue) : IntervalDynSet
  
  def move(f: Mapping) : IntervalDynSet
  def move(f: C1Flow)  : (IntervalDynSet, IntervalDynSet)
}

/* Box */

/** Implementation of the Box */
case class Box(v : RealVector) extends IntervalDynSet {
  
  val outerEnclosure = v
  
  val dim = v.size

    def map(m: CValue => CValue): Box = Box(v.copy.map(m))
    
    def map(m: (Int, CValue) => CValue): Box = {
      // TODO implement with UFunc
      def mapVector(v: RealVector) = breeze.linalg.Vector.tabulate[CValue](dim) { i      => m(i, v(i)) }
      Box(mapVector(v))
    }
    
    def move(f: Mapping): Box = Box(f(v))
          
    def move(f: C1Flow): (Box, Box) = (Box(f(v)), Box(f.range(v)))
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
    
    lazy val outerEnclosure = midpoint + linearTransformation * width + error
  
    val dim = midpoint.size

    def map(m: CValue => CValue): Cuboid = {
      val imageMidpoint             : RealVector = midpoint.copy.map(m)
      val imageWidth                : RealVector = width.copy.map(m)
      val imageLinearTransformation : RealMatrix = linearTransformation    
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
