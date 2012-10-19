package acumen
package ui

import java.awt.geom.{AffineTransform,Point2D,Rectangle2D}

package object plot {

  def applyTrP(tr: AffineTransform, p: Point2D) : Point2D = {
    val res = new Point2D.Double
    tr.transform(p,res)
    res 
  }

  def applyTrR(tr: AffineTransform, r: Rectangle2D) : Rectangle2D = {
    val p1 = applyTrP(tr, new Point2D.Double(r.getMinX, r.getMinY))
    val p2 = applyTrP(tr, new Point2D.Double(r.getMaxX, r.getMaxY))
    val res = new Rectangle2D.Double()
    res.setFrameFromDiagonal(p1, p2)
    res
  }
  
  def unapplyTrP(tr: AffineTransform, p: Point2D) : Point2D = {
    val res = new Point2D.Double
    tr.inverseTransform(p,res)
    res
  }

  def unapplyTrR(tr:AffineTransform, r: Rectangle2D) : Rectangle2D = {
    val p1 = unapplyTrP(tr, new Point2D.Double(r.getMinX, r.getMinY))
    val p2 = unapplyTrP(tr, new Point2D.Double(r.getMaxX, r.getMaxY))
    val res = new Rectangle2D.Double()
    res.setFrameFromDiagonal(p1, p2)
    res
  }

  /* translate then scale according to the viewport */
  def computeTransform(width:Double, height:Double, view:Rectangle2D) = {
    val tr = AffineTransform.getScaleInstance(
      width / view.getWidth,
      height / view.getHeight)
    tr.translate(-view.getMinX, -view.getMinY)
    tr
  }

}
