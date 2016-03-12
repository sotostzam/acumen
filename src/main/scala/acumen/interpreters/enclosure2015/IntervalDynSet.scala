package acumen
package interpreters
package enclosure2015

import enclosure.Interval
import enclosure2015.Common._
import interpreters.Common._
import Errors._
import enclosure2015.ValidatedLinAlg._

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
  def mapping(f: C1Mapping)(implicit parameters: Parameters): IntervalDynSet = init(f(outerEnclosure))
  
  // Move the represented vector by a flow: (range enclosure, end-time enclosure)
  def move(f: C1Flow)(implicit parameters: Parameters): (IntervalDynSet, IntervalDynSet)
  
  // Containment relation between IntervalDynSets
  def contains(that: IntervalDynSet): Boolean
  
  /* RealVector interface */
  def repr                 = outerEnclosure.repr
  def activeIterator       = outerEnclosure.activeIterator
  def activeKeysIterator   = outerEnclosure.activeKeysIterator   
  def activeValuesIterator = outerEnclosure.activeValuesIterator 
  def activeSize           = outerEnclosure.activeSize
  def length: Int          = outerEnclosure.length
  def copy                 = outerEnclosure.copy
  def update(i: Int, c: CValue) = throw ShouldNeverHappen()         
}

/** The IntervalBox represents the vector as itself.
 */
case class IntervalBox(v: RealVector) extends IntervalDynSet
{
  val outerEnclosure = v
  
  def init(vec: RealVector) = IntervalBox(vec)
          
  def move(f: C1Flow)(implicit parameters: Parameters): (IntervalBox, IntervalBox) = (IntervalBox(f(v)), IntervalBox(f.range(v)))
  
  def contains(that: IntervalDynSet): Boolean = isContained(that, v)
}

/** The Cuboid represents the vector as midpoint + linearTransform * width + error, where
 *    midpoint, width, error \in R^n
 *    linearTransform        \in R^{n x n} effectively orthogonal
 *    linearTransformT       \in R^{n x n}
 *    errorLTTLT             \in R^{n x n}
 *    
 *    such that linearTransformT * linearTransform \subseteq Id + errorLTTLT.
 */
case class Cuboid
  ( midpoint             : RealVector
  , linearTransform      : RealMatrix
  , linearTransformT     : RealMatrix
  , errorLTTLT           : RealMatrix
  , width                : RealVector
  , error                : RealVector )
    extends IntervalDynSet
    {
    assert( midpoint.size        == linearTransform.rows 
         && linearTransform.rows == linearTransform.cols 
         && width.size           == error.size,
         "Creation of Cuboid failed: dimensions mismatch.")
    
    val outerEnclosure = midpoint + linearTransform * width + error
    
    def init(vec: RealVector) = Cuboid(vec)
          
    /* flow(X(0), t) = phi(X(0), t) + remainder(X(0, t), t)
     * and phi(X(0), t) \in phi(x, t) + Dphi(X(0), t)(X(0) - x)
     */
    def move(f: C1Flow)(implicit parameters: Parameters): (Cuboid, Cuboid) = {
      val coarseRangeEnclosure : RealVector = f.range(outerEnclosure)
      
      val phi                 : RealVector = f.phi(midpoint)
      val jacobian            : RealMatrix = f.jacPhi(outerEnclosure)
      val remainder           : RealVector = f.remainder(coarseRangeEnclosure)
      
      val phiPlusRemainder    : RealVector = phi + remainder
      
      val resultOfQR : (RealMatrix, RealMatrix, RealMatrix, RealMatrix, RealMatrix) = validatedQR(jacobian * linearTransform)

      val imageMidpoint             : RealVector = midpointVector(phiPlusRemainder)
      val imageWidth                : RealVector = resultOfQR._2 * width
      val imageLinearTransform      : RealMatrix = resultOfQR._1
      val imageLinearTransformT     : RealMatrix = resultOfQR._3
      val imageErrorLTTLT           : RealMatrix = resultOfQR._5
      val imageError                : RealVector = resultOfQR._4 * width + jacobian * error + 
                                                   centeredVector(phiPlusRemainder)
      
      lazy val refinedRangeEnclosure : RealVector = {
        val rangeMidpoint  : RealVector = f.range.phi(midpoint)
        val rangeJacobian  : RealMatrix = f.range.jacPhi(outerEnclosure)
        val rangeRemainder : RealVector = f.range.remainder(coarseRangeEnclosure)
        
        rangeMidpoint + rangeJacobian * (linearTransform * width + error) + rangeRemainder 
      }
      
      // Log information about the jacobian and error
      val (eigenValReal, eigenValImag, eigenVecs) = ValidatedLinAlg.approximateED(jacobian)
      val eigenValNorm = breeze.linalg.DenseVector.tabulate[CValue](eigenValReal.length) {
        i => val (VLit(GConstantRealEnclosure(a)), VLit(GConstantRealEnclosure(b))) = (eigenValReal(i), eigenValImag(i))  
          VLit(GConstantRealEnclosure((a.square + b.square).sqrt))
      }
      Logger.trace(s"Cuboid.move: Jacobian eigenvalue norm: ${Pretty prettyRealVector eigenValNorm}, " + 
                                          s"real component: ${Pretty prettyRealVector eigenValReal}, " +
                                     s"imaginary component: ${Pretty prettyRealVector eigenValImag}.")
      Logger.trace(s"Cuboid.move: Maximum image error width: ${imageError.maxWidth}")
  
      /** If error width exceeds the image width in any dimension, then 
       *  re-initialize the Cuboid, moving the error to the width term. */
      def reorganizeIfNeeded( imageMidpoint         : RealVector
                            , imageLinearTransform  : RealMatrix 
                            , imageLinearTransformT : RealMatrix
                            , imageErrorLTTLT       : RealMatrix
                            , imageWidth            : RealVector
                            , imageError            : RealVector)
                            : Cuboid = {
        
        val factor = 1
        
        if (imageError.maxWidth > factor * imageWidth.maxWidth) {
          val newWidth                : RealVector = imageError + imageLinearTransform * imageWidth 
          val newLinearTransform      : RealMatrix = breeze.linalg.DenseMatrix.eye[CValue](this.size)
          val newLinearTransformT     : RealMatrix = newLinearTransform
          val newErrorLTTLT           : RealMatrix = breeze.linalg.DenseMatrix.zeros[CValue](this.size, this.size)
          val newError                : RealVector = breeze.linalg.Vector.zeros[CValue](this.size)
          
          Logger.trace(s"Cuboid.move: Reorganizing the data structure: ${imageError.maxWidth} > ${factor} * ${imageWidth.maxWidth}")
          
          Cuboid(imageMidpoint, newLinearTransform, newLinearTransformT, newErrorLTTLT, newWidth, newError)
        } else
          Cuboid(imageMidpoint, imageLinearTransform, imageLinearTransformT, imageErrorLTTLT, imageWidth, imageError)
      }
      
      ( Cuboid(refinedRangeEnclosure)
      , reorganizeIfNeeded(imageMidpoint, imageLinearTransform, imageLinearTransformT, imageErrorLTTLT, imageWidth, imageError) )
    }
    
    override def mapping(f: C1Mapping)(implicit parameters: Parameters): Cuboid = {
      
      val phi                 : RealVector = f.phi(midpoint)
      val jacobian            : RealMatrix = f.jacPhi(outerEnclosure)
      
      val resultOfQR : (RealMatrix, RealMatrix, RealMatrix, RealMatrix, RealMatrix) = validatedQR(jacobian * linearTransform)

      val imageMidpoint             : RealVector = midpointVector(phi)
      val imageWidth                : RealVector = resultOfQR._2 * width
      val imageLinearTransform      : RealMatrix = resultOfQR._1
      val imageLinearTransformT     : RealMatrix = resultOfQR._3
      val imageErrorLTTLT           : RealMatrix = resultOfQR._5
      val imageError                : RealVector = resultOfQR._4 * width + jacobian * error + 
                                                   centeredVector(phi)
      
      Cuboid(imageMidpoint, imageLinearTransform, imageLinearTransformT, imageErrorLTTLT, imageWidth, imageError)
    }
    
    def contains(that: IntervalDynSet): Boolean = that match {
        
        case IntervalBox(_) => this contains Cuboid(that)
    
        /* m1 + L1 * w1 + e1 \supseteq m2 + L2 * w2 + e2
         * 
         *  <=>                 (if L1 is non-singular)
         *  
         * L1^t( m1 + L1 * w1 + e1 ) \supseteq L1^t( m2 + L2 * w2 + e2 )
         * 
         *  is implied by       (if Id + eL1tL1 \supseteq L1^t L1)
         *  
         * w1 \supseteq L1^t( m2 - m1 + e2 - e1 ) + (L1^t * L2) * w2  - eL1tL1 * w1
         */
        case Cuboid(thatMidpoint: RealVector, thatLinearTransform: RealMatrix, _, _, thatWidth: RealVector, thatError: RealVector) =>
          // FIXME use proper implicits so vectors can be substracted from each other directly
          val minusId = breeze.linalg.DenseMatrix.tabulate[CValue](length, length) { 
            (i, j) => if (i == j) VLit(GConstantRealEnclosure(Interval(-1.0))) else zero }
            val rhs = linearTransformT * (thatMidpoint + minusId * midpoint + thatError + minusId * error) + (linearTransformT * thatLinearTransform) * thatWidth + minusId * (errorLTTLT * width)
            isContained(rhs, width)        
    }
    
  def toBeReorganized: Boolean =
   (0 until length).filter( i => (width(i), error(i)) match { 
     case (VLit(GConstantRealEnclosure(wi)), VLit(GConstantRealEnclosure(ei))) => ei.width.hiDouble > wi.width.hiDouble * 10.0 } ).isEmpty

  def reorganize: Cuboid = Cuboid(outerEnclosure)
}

/** Initializing a Cuboid from a RealVector.
 */
object Cuboid {
  def apply(v: RealVector)(implicit cValueIsReal: acumen.Real[CValue]): Cuboid = {
    val zero = cValueIsReal.zero
    val one  = cValueIsReal.one

    val newMidpoint             : RealVector = midpointVector(v)
    val newWidth                : RealVector = centeredVector(v)
    val newLinearTransform      : RealMatrix = breeze.linalg.DenseMatrix.eye[CValue](v.size)
    val newLinearTransformT     : RealMatrix = newLinearTransform
    val newErrorLTTLT           : RealMatrix = breeze.linalg.DenseMatrix.zeros[CValue](v.size, v.size)
    val newError                : RealVector = breeze.linalg.Vector.zeros[CValue](v.size)
    
    Cuboid(newMidpoint, newLinearTransform, newLinearTransformT, newErrorLTTLT, newWidth, newError)
  }
}
