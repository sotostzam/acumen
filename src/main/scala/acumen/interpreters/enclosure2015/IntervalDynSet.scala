package acumen
package interpreters
package enclosure2015

import enclosure.Interval
import enclosure2015.Common._
import interpreters.Common._
import Errors._

/** An IntervalDynSet represents a dim-dimensional RealVector.
 *  By choosing an appropriate representation, wrapping may be reduced.
 */ 
abstract class IntervalDynSet extends RealVector
{  
  implicit val cValueIsReal: Real[CValue] = acumen.intervalCValueIsReal
  
  val zero = VLit(GConstantRealEnclosure(Interval.zero))
  val one  = VLit(GConstantRealEnclosure(Interval.one))
  
  // An outer enclosure of the represented RealVector
  val outerEnclosure: RealVector
  
  // Initialize an IntervalDynSet representing vec
  def init(vec: RealVector): IntervalDynSet
  
  // Obtain an enclosure of the represented vectors i-th element
  def apply(i: Int): CValue = outerEnclosure(i)
  
  // Modify the represented vector
  def set(i: Int, c: CValue)          : IntervalDynSet = init(outerEnclosure.updated(i, c))
  def map(m: CValue => CValue)        : IntervalDynSet = init(outerEnclosure.map(m))
  def map(m: (Int, CValue) => CValue) : IntervalDynSet = init(outerEnclosure.mapIndexwise(m))
  
  // Map the represented vector
  def move(f: Mapping) : IntervalDynSet = init(f(outerEnclosure))
  
  // Move the represented vector by a flow: (range enclosure, end-time enclosure)
  def move(f: C1Flow)  : (IntervalDynSet, IntervalDynSet)
  
  // TODO this is a stub to enable development on a higher level
  def contains(that: IntervalDynSet): Boolean = (0 until outerEnclosure.length).forall {
    i => val (VLit(GConstantRealEnclosure(i1)), VLit(GConstantRealEnclosure(i2))) = (this(i), that(i))
         i1 contains i2 }
  
  /* RealVector interface */
  def repr                 = outerEnclosure.repr
  def activeIterator       = outerEnclosure.activeIterator
  def activeKeysIterator   = outerEnclosure.activeKeysIterator   
  def activeValuesIterator = outerEnclosure.activeValuesIterator 
  def activeSize           = outerEnclosure.activeSize
  def length: Int          = outerEnclosure.length
  def copy                 = outerEnclosure.copy
  def update(i: Int,v: acumen.CValue) = throw ShouldNeverHappen()         
}

/** The IntervalBox represents the vector as itself.
 */
case class IntervalBox(v: RealVector) extends IntervalDynSet
{
  val outerEnclosure = v
  
  def init(vec: RealVector) = IntervalBox(vec)
          
  def move(f: C1Flow): (IntervalBox, IntervalBox) = (IntervalBox(f(v)), IntervalBox(f.range(v)))
}

/** The Cuboid represents the vector as midpoint + linearTransformation * width + error, where
 *    midpoint, width, error \in R^n
 *    linearTransformation   \in R^{n x n}
 */
case class Cuboid
  ( midpoint             : RealVector
  , linearTransformation : RealMatrix
  , width                : RealVector
  , error                : RealVector )
    extends IntervalDynSet
    {
    assert( midpoint.size             == linearTransformation.rows 
         && linearTransformation.rows == linearTransformation.cols 
         && width.size                == error.size,
         "Creation of Cuboid failed: dimensions mismatch.")
    
    val outerEnclosure = midpoint + linearTransformation * width + error
    
    def init(vec: RealVector) = Cuboid(vec)
          
    /* flow(X(0), t) = phi(X(0), t) + remainder(X(0, t), t)
     * and phi(X(0), t) \in phi(x, t) + Dphi(X(0), t)(X(0) - x)
     */
    def move(f: C1Flow): (Cuboid, Cuboid) = {
      val coarseRangeEnclosure : RealVector = f.range(outerEnclosure)
      
      val phi                 : RealVector = f.phi(midpoint)
      val jacobian            : RealMatrix = f.jacPhi(outerEnclosure)
      val remainder           : RealVector = f.remainder(coarseRangeEnclosure)
      
      val phiPlusRemainder    : RealVector = phi + remainder

      val imageMidpoint             : RealVector = midpointVector(phiPlusRemainder)
      val imageWidth                : RealVector = width
      val imageLinearTransformation : RealMatrix = jacobian * linearTransformation
      val imageError                : RealVector = jacobian * error + 
                                                   centeredVector(phiPlusRemainder)

      lazy val refinedRangeEnclosure : RealVector = {
        val rangeMidpoint  : RealVector = f.range.phi(midpoint)
        val rangeJacobian  : RealMatrix = f.range.jacPhi(outerEnclosure)
        val rangeRemainder : RealVector = f.range.remainder(coarseRangeEnclosure)
        
        rangeMidpoint + rangeJacobian * (linearTransformation * width + error) + rangeRemainder 
      }
  
      ( Cuboid(refinedRangeEnclosure)
      , Cuboid(imageMidpoint, imageLinearTransformation, imageWidth, imageError) )
    }
}

/** Initializing a Cuboid from a RealVector.
 */
object Cuboid {
  def apply(v: RealVector)(implicit cValueIsReal: Real[CValue]): Cuboid = {
    val zero = cValueIsReal.zero
    val one  = cValueIsReal.one

    val newMidpoint             : RealVector = midpointVector(v)
    val newWidth                : RealVector = centeredVector(v)
    val newLinearTransformation : RealMatrix = breeze.linalg.DenseMatrix.eye[CValue](v.size)    
    val newError                : RealVector = breeze.linalg.Vector.zeros[CValue](v.size)
    
    Cuboid(newMidpoint, newLinearTransformation, newWidth, newError)
  }
}
