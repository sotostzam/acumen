package acumen
package interpreters
package enclosure2015

import interpreters.Common._
import enclosure.Interval
import enclosure2015.Common._
import enclosure2015.Interpreter._
import Errors._

/** Validated Linear Algebra routines.  
 */
object ValidatedLinAlg {
  
  /** Containment relation between RealVectors
   *    v1 \subseteq v2
   */
  def isContained(v1: RealVector, v2: RealVector): Boolean = 
    (v1.length == v2.length) && (0 until v1.length).forall {
      i => val (VLit(GConstantRealEnclosure(i1)), VLit(GConstantRealEnclosure(i2))) = (v1(i), v2(i))
         i2 contains i1 }
  
  /** Computes an interval 'e' such that 
   *  i1 \subseteq i2 + e
   */
  def computeDifferenceInterval(i1: Interval, i2: Interval): Interval = 
    Interval( (i1.low - i2.low).loDouble, (i1.high - i2.high).hiDouble )

  /** Computes a validated QR decomposition for an input matrix M. 
   *  The result is 5 matrices Q, R, Q^t, eQR, eQtQ such that
   *  
   *  m \subseteq QR + eQR
   *  Q^t Q \subseteq Id + eQtQ
   */
  def validatedQR(M : RealMatrix): (RealMatrix, RealMatrix, RealMatrix, RealMatrix, RealMatrix) = {
    
    implicit val cValueIsReal: Real[CValue] = acumen.intervalCValueIsReal
    
    val zero = VLit(GConstantRealEnclosure(Interval.zero))
    val one  = VLit(GConstantRealEnclosure(Interval.one))
    
    assert( M.rows == M.cols, 
         "validated QR-decomposition failed: a square matrix is required.")
    
    val midOfM = breeze.linalg.DenseMatrix.tabulate[Double](M.rows, M.cols) { 
      (i, j) => M(i, j) match { 
        case VLit(GConstantRealEnclosure(i)) => i.midpoint.doubleValue() 
        case _                               => throw ShouldNeverHappen() 
      } }
      
    val doubleQR = breeze.linalg.qr(midOfM) match { case breeze.linalg.qr.QR(q,r) => (q, r) }
    
    val Q = breeze.linalg.DenseMatrix.tabulate[CValue](M.rows, M.cols) { 
      (i, j) => VLit(GConstantRealEnclosure(Interval(doubleQR._1(i, j)))) }
    
    val R = breeze.linalg.DenseMatrix.tabulate[CValue](M.rows, M.cols) { 
      (i, j) => VLit(GConstantRealEnclosure(Interval(doubleQR._2(i, j)))) }
    
    val QR = Q * R
    
    val Qt = Q.t
    
    val QtQ = Qt * Q
        
    // m \subseteq QR + errorQR
    val errorQR = breeze.linalg.DenseMatrix.tabulate[CValue](M.rows, M.cols) { 
      (i, j) => 
        val (VLit(GConstantRealEnclosure(int1)), VLit(GConstantRealEnclosure(int2))) = (M(i, j), QR(i, j)) 
        VLit(GConstantRealEnclosure(computeDifferenceInterval(int1, int2))) }
    
    // Qt * Q \subseteq I + errorQtQ
    val errorQtQ = breeze.linalg.DenseMatrix.tabulate[CValue](M.rows, M.cols) { 
      (i, j) => 
        val (VLit(GConstantRealEnclosure(int1)), VLit(GConstantRealEnclosure(int2))) = (QtQ(i, j), if (i == j) one else zero) 
        VLit(GConstantRealEnclosure(computeDifferenceInterval(int1, int2))) }

    (Q, R, Qt, errorQR, errorQtQ)
  }
}