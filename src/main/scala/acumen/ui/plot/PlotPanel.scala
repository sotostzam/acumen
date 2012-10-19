package acumen
package ui
package plot

import java.awt.{BasicStroke, Color}
import java.awt.RenderingHints
import java.awt.Transparency
import java.awt.geom.{Ellipse2D, Line2D}
import java.awt.geom.{Point2D, Rectangle2D}
import java.awt.geom.AffineTransform
import java.awt.image.BufferedImage
import java.io.File
import java.util.TimerTask
import javax.imageio.ImageIO
import scala.collection.JavaConversions._
import scala.collection.mutable.{ArrayBuffer, Stack}
import scala.swing._
import scala.swing.event._

import util.Canonical._
import util.Conversions._
import interpreter._

case class PointedAtEvent(time:Double, name:String, value:String) extends Event

class PlotPanel(tb:TraceModel, pub:Publisher) 
  extends Panel 
{
  background = Color.gray
  peer.setDoubleBuffered(true)

  private var pp = PlotParms()
  private var pd = new PlotData(PlotParms(),null)
 
  private val selectionBorderColor = new Color(68, 127, 231)
  private val selectionFillColor   = new Color(165, 189, 231, 150)
 
  private var buffer : BufferedImage = null
  private var hoveredBox : Option[Int] = None
  private var dotX : Double = 0.0d
  private var dotY : Option[Double] = None
  private var selection : Option[(Point2D, Point2D)] = None
  private var drag : Option[(Point2D,Point2D)] = None

  private var viewPort : Rectangle2D = new Rectangle2D.Double(0,0,0,0)
  private val undoStack = new Stack[Rectangle2D] 
  private var viewChanged = true
  private var transform = new AffineTransform()

  private var plotStyle : PlotStyle = Lines()

  def clear = {
    pd = new PlotData(PlotParms(),null)
    buffer = null
    hoveredBox = None
    dotX = 0.0d
    dotY = None
    selection = None
    drag = None
    pd.boundingBox = (0.0, 0.0)
    viewPort = new Rectangle2D.Double(0,0,0,0)
    undoStack.clear 
    viewChanged = true
    transform = new AffineTransform()
    repaint
  }

  private def pushCurrentViewport = {
    undoStack.push(viewPort.clone.asInstanceOf[Rectangle2D])
  }

  def undo = {
    if (undoStack.nonEmpty) {
      viewPort = undoStack.pop
      viewChanged = true
      repaint
    }
  }

  def mkBuffer =  {
    peer.getGraphicsConfiguration.createCompatibleImage(
      size.width, size.height, Transparency.OPAQUE
    )
  }
  
  def applyTr(p:Point2D) : Point2D = applyTrP(transform, p)
  def applyTr(r:Rectangle2D) : Rectangle2D = applyTrR(transform, r)
  def unapplyTr(p:Point2D) : Point2D = unapplyTrP(transform, p)
  def unapplyTr(r:Rectangle2D) : Rectangle2D = unapplyTrR(transform, r)

  def fit : Unit = {
    if (buffer != null) {
      undoStack.clear
      val bb = new Rectangle2D.Double(0, 0, pd.boundingBox._1, pd.boundingBox._2)
      val tr = computeTransform(buffer.getWidth, buffer.getHeight, bb)
      val trbb = applyTrR(tr, bb)
      trbb.setRect(trbb.getX-10, trbb.getY-10, trbb.getWidth+20, trbb.getHeight+20)
      viewPort = unapplyTrR(tr, trbb)
      viewChanged = true
      repaint
    }
  }

  private def selectionRectangle = 
    for ((p1,p2) <- selection)
      yield new Rectangle2D.Double(math.min(p1.getX, p2.getX),
                                   math.min(p1.getY, p2.getY),
                                   math.abs(p1.getX - p2.getX),
                                   math.abs(p1.getY - p2.getY))

  def paintLater = 
    new TimerTask {
      def run = {
        viewChanged = true
        repaint
      }
    }

  /* translate then scale according to the viewport */
  private def computeTransform(width:Double, height:Double, view:Rectangle2D) = {
    val tr = AffineTransform.getScaleInstance(
      width / view.getWidth,
      height / view.getHeight)
    tr.translate(-view.getMinX, -view.getMinY)
    tr
  }

  private def paintBackground(tr:AffineTransform, buf:BufferedImage) = {
    val bg = buf.getGraphics.asInstanceOf[Graphics2D]
    bg.setRenderingHint(
      RenderingHints.KEY_ANTIALIASING,
      RenderingHints.VALUE_ANTIALIAS_ON)

    /* paint background */
    bg.setBackground(Color.lightGray)
    bg.clearRect(0, 0, buf.getWidth, buf.getHeight)

    /* clip the area to the size of the buffer */
    bg.setClip(new Rectangle2D.Double(0, 0, buf.getWidth, buf.getHeight))

    /* actual drawing */
    for (((p,a),b) <- pd.polys zip pd.axes zip pd.boxes) {

      // fill bounding box
      bg.setPaint(Color.white)
      bg.fill(applyTrR(tr, b))

      // draw y=0 axis
      bg.setPaint(Color.blue)
      bg.setStroke(new BasicStroke(1.0f))
      a.draw(bg, tr)

      // draw curve
      bg.setPaint(Color.red)
      bg.setStroke(new BasicStroke(1.0f))
      plotStyle match {
        case Dots() => 
          p.drawDots(bg, tr) 
        case Lines() => 
          p.draw(bg, tr)
        case Both() =>
          p.draw(bg, tr)
          p.drawDots(bg, tr) 
      }
    }

    buf.flush
  }

  override def paintComponent(g: Graphics2D) {

    g.setRenderingHint(
      RenderingHints.KEY_ANTIALIASING,
      RenderingHints.VALUE_ANTIALIAS_ON)
    super.paintComponent(g)

    if (viewChanged) {
      println("View Changed Man!!")
      /* create a new buffer for the background */
      buffer = mkBuffer
      /* compute the current transformation */
      transform = computeTransform(buffer.getWidth, buffer.getHeight, viewPort)
      /* draw the background */
      paintBackground(transform, buffer) 
      viewChanged = false
    } 
    
    // clip the area to what the user can see
    g.setClip(new Rectangle2D.Double(0, 0, size.getWidth, size.getHeight))
    
    // blit buffer
    drag match {
      case Some((_,v)) => 
        g.drawImage(buffer, null, v.getX.round.toInt, v.getY.round.toInt)
      case None => 
        g.drawImage(buffer, null, 0, 0)
    }

    // draw selected box
    g.setPaint(Color.gray)
    g.setStroke(new BasicStroke(2.0f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND))
    hoveredBox match {
      case Some(hb) =>
        val rec = applyTr(pd.boxes(hb))
        val recPlusOne = 
          new Rectangle2D.Double(
            rec.getX-2, rec.getY-2, 
            rec.getWidth+4, rec.getHeight+4)
        g.draw(recPlusOne)
      case None => ()
    }

    // draw mouse line and mouse dot (if any)
    g.setStroke(new BasicStroke(1.0f))

    g.draw(new Line2D.Double(dotX, 0, dotX, size.getHeight))

    dotY match {
      case Some(y) =>
        g.setPaint(Color.green)
        g.fill(new Ellipse2D.Double(dotX-2, y-2, 5, 5))
        g.setPaint(Color.gray)
        g.setStroke(new BasicStroke(0.5f))
        g.draw(new Ellipse2D.Double(dotX-2, y-2, 5, 5))
      case None => ()
    }

    // draw selection rectangle (if any)
    selectionRectangle match {
      case Some(r) =>
        g.setPaint(selectionFillColor)
        g.fill(r)
        g.setPaint(selectionBorderColor)
        g.setStroke(new BasicStroke(1.0f))
        g.draw(r)
      case None => ()
    }
  }

  /* mouse handling */

  // binary search
  private def findTimeIndex(t:Double) : (Double, Int) = {
    def findIn(min:Int, max:Int) : (Double, Int) = {
      if (max - min <= 1) {
        val tmax = pd.traceViewTime(max)
        val tmin = pd.traceViewTime(min)
        if (math.abs(tmax-t) < math.abs(tmin-t)) 
          (tmax, max)
        else (tmin, min)
      }
      else {
        val middle = (max+min)/2
        if (pd.traceViewTime(middle) < t) findIn(middle, max)
        else findIn(min, middle)
      }
    }
    if (pd.traceViewTime.isEmpty) (0,0)
    else findIn(0, pd.traceViewTime.size-1)
  }

  listenTo(mouse.moves)
  listenTo(mouse.clicks)
  //listenTo(mouse.wheel)
  listenTo(this)

  reactions += {
    case MouseMoved(_, p, _) =>
      if (pd.columnIndices.size > 0) {
        // p in the original coordinate system
        val op = unapplyTr(p)
        // quantized time and row in the trace model corresponding to p.x
        var (qt, row) = findTimeIndex(op.getX)
        // update the vertical line (and dot) x coordinate
        dotX = applyTr(new Point2D.Double(qt, 0)).getX

        // index of the curve pointed by the mouse
        val hb = (op.getY / 1.2).toInt
        if (hb < pd.columnIndices.length&&hb>=0) {
          // corresponding column in the trace model
          val column = pd.columnIndices(hb)
          // name of the column
          val name = tb.getColumnName(column)
          // value of the column as a string
          var value = tb.getValueAt(row, column).asInstanceOf[String]
          // FIXME: This is an incredible ugly hack to check if we area
          //   displaying enclosures instead of a real trace view.
          if (tb.getColumnName(0) == "time" && value(0) == '[') {
            if (row == 0)
              value = "(-,%s)".format(value)
            else if (row == tb.getRowCount() - 1)
              value = "(%s,-)".format(value)
            else {
              if (row % 2 == 0) row -= 1
              value = "(%s,%s)".format(tb.getValueAt(row, column).asInstanceOf[String],
                                       tb.getValueAt(row + 1, column).asInstanceOf[String])
            }
          }
          // publish a pointed at event
          pub.publish(PointedAtEvent(qt, name, value))
          // update the green dot Y coordinate
          val (scale, shift) = pd.yTransformations(hb)
          dotY = for (y <- tb.getDouble(row, column))
                   yield applyTr(new Point2D.Double(0, y*scale + shift)).getY
          // updating the hovered box number
          hoveredBox = Some(hb)
        } else {
          // the mouse is pointing at no box
          hoveredBox = None
        }
        repaint
      }
    /*
    case e@MouseClicked(_,_,_,_,_) => 
      if (e.peer.getButton == 1) {
        viewPort = boxes(hoveredBox).clone.asInstanceOf[Rectangle2D]
        val dx = viewPort.getWidth / 100
        val dy = viewPort.getHeight / 100
        pushCurrentViewport
        viewPort.setFrameFromDiagonal(viewPort.getMinX-dx, viewPort.getMinY-dy, 
                                      viewPort.getMaxX+dx, viewPort.getMaxY+dy)
        viewChanged = true
        repaint
      }
      */
    case e@MousePressed(_,p,_,_,_) =>
      if (e.peer.getButton == 3)
        drag = Some((p, new Point2D.Double(0,0)))
      else if (e.peer.getButton == 1)
        selection = Some((p,p))
    case MouseDragged(_,p,_) =>
      selection match {
        case Some((o,_)) => selection = Some((o,p))
        case None        => ()
      }
      drag match {
        case Some((o,_)) => 
          drag = Some((o,new Point2D.Double(p.getX-o.getX, p.getY-o.getY)))
        case None => ()
      }
      repaint
    case e@MouseReleased(_,p,_,_,_) =>
      if (e.peer.getButton == 3) {
        val v = drag.get._2
        pushCurrentViewport
        viewPort.setRect(
          viewPort.getX - v.getX / transform.getScaleX, 
          viewPort.getY - v.getY / transform.getScaleY,
          viewPort.getWidth, 
          viewPort.getHeight)
        drag = None
        viewChanged = true
        repaint
      } else if (e.peer.getButton == 1) {
          val selected = selectionRectangle.get
          if (math.abs(selected.getWidth * selected.getHeight) > 1) {
          // there is no way to transform a rectangle and get a rectangle
          // so we have to do it ourselves...
          val p1 = unapplyTr(new Point2D.Double(selected.getMinX, selected.getMinY))
          val p2 = unapplyTr(new Point2D.Double(selected.getMaxX, selected.getMaxY))
          pushCurrentViewport
          viewPort.setFrameFromDiagonal(p1,p2)
          selection = None
          viewChanged = true
          repaint
        }
      }
      /*
    case MouseWheelMoved(_,p,_,i) =>
      val op = unapplyTr(p)
      val (cx,cy) = (op.getX, op.getY) 
      val factor = math.pow(1.5,i)
      zoomAround(cx,cy,factor)
    */
    case ComponentResized(_) =>
      viewChanged = true
      repaint
  }

  def zoomAround(cx:Double, cy:Double, factor:Double) =  {
    val f = 1/factor
    val (p1x, p1y) = ((viewPort.getMinX-cx) * f + cx, 
                      (viewPort.getMinY-cy) * f + cy)
    val (p2x, p2y) = ((viewPort.getMaxX-cx) * f + cx, 
                      (viewPort.getMaxY-cy) * f + cy)
    pushCurrentViewport
    viewPort.setFrameFromDiagonal(p1x, p1y, p2x, p2y)
    viewChanged = true
    repaint
  }

  def zoom(factor:Double) = 
    zoomAround(viewPort.getCenterX, viewPort.getCenterY, factor)

  def redraw = {
    println("Redraw man!")
    pd = new PlotData(pp,tb)
    viewChanged = true
    repaint
  }

  def toggleSimulator(b:Boolean) = { 
    pp = pp.copy(plotSimulator = b)
    hoveredBox = None
    redraw
    fit
  }
  def toggleNextChild(b:Boolean) = {
    pp = pp.copy(plotNextChild = b)
    hoveredBox = None
    redraw
    fit
  }
  def toggleSeeds(b:Boolean) = {
    pp = pp.copy(plotSeeds = b)
    hoveredBox = None
    redraw
    fit
  }
  def setPlotStyle(ps:PlotStyle) = {
    plotStyle = ps
    viewChanged = true
    repaint
  }

  def render(file:File, w:Int, h:Int) = {
    val tr = computeTransform(w,h,viewPort)
    val buf = new BufferedImage(w, h, BufferedImage.TYPE_INT_ARGB)
    paintBackground(tr,buf)
    ImageIO.write(buf, "PNG", file)
  }
}

