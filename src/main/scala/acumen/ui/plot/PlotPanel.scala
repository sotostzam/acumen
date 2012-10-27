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

class PlotPanel(pub:Publisher) extends Panel 
{
  background = Color.gray
  peer.setDoubleBuffered(true)

  /*private*/ @volatile var pp = PlotParms()
  /*private*/ var pd : PlotData = new PlotData(PlotParms(),null)
  var pi : PlotImage = null
  /*private*/ var model : PlotModel = null
 
  private val selectionBorderColor = new Color(68, 127, 231)
  private val selectionFillColor   = new Color(165, 189, 231, 150)
 
  private var hoveredBox : Option[Int] = None
  private var dotX : Double = 0.0d
  private var dotY : Option[Double] = None
  private var selection : Option[(Point2D, Point2D)] = None
  private var drag : Option[(Point2D,Point2D)] = None

  private val undoStack = new Stack[Rectangle2D] 

  /*private*/ @volatile var plotStyle : PlotStyle = Lines()

  def clear = {
    pd = new PlotData(PlotParms(),null)
    pi = null
    hoveredBox = None
    dotX = 0.0d
    dotY = None
    selection = None
    drag = None
    pd.boundingBox = (0.0, 0.0)
    resetViewPort(new Rectangle2D.Double(0,0,0,0))
    repaint
  }

  private def pushCurrentViewport = {
    undoStack.push(viewPort.clone.asInstanceOf[Rectangle2D])
  }

  def undo = {
    if (undoStack.nonEmpty) {
      viewPort = undoStack.pop
      plotter ! Repaint(viewPort)
    }
  }

  def mkBuffer =  {
    peer.getGraphicsConfiguration.createCompatibleImage(
      size.width, size.height, Transparency.OPAQUE
    )
  }
  
  def applyTr(p:Point2D) : Point2D = applyTrP(pi.transform, p)
  def applyTr(r:Rectangle2D) : Rectangle2D = applyTrR(pi.transform, r)
  def unapplyTr(p:Point2D) : Point2D = unapplyTrP(pi.transform, p)
  def unapplyTr(r:Rectangle2D) : Rectangle2D = unapplyTrR(pi.transform, r)

  private var viewPort : Rectangle2D = null
  def resetViewPort(vp: Rectangle2D) = {
    println("Restting view port")
    undoStack.clear
    viewPort = vp
  }

  private def selectionRectangle = 
    for ((p1,p2) <- selection)
      yield new Rectangle2D.Double(math.min(p1.getX, p2.getX),
                                   math.min(p1.getY, p2.getY),
                                   math.abs(p1.getX - p2.getX),
                                   math.abs(p1.getY - p2.getY))

  /* translate then scale according to the viewport */
  private def computeTransform(width:Double, height:Double, view:Rectangle2D) = {
    val tr = AffineTransform.getScaleInstance(
      width / view.getWidth,
      height / view.getHeight)
    tr.translate(-view.getMinX, -view.getMinY)
    tr
  }

  override def paintComponent(g: Graphics2D) {

    g.setRenderingHint(
      RenderingHints.KEY_ANTIALIASING,
      RenderingHints.VALUE_ANTIALIAS_ON)
    super.paintComponent(g)

    // clip the area to what the user can see
    g.setClip(new Rectangle2D.Double(0, 0, size.getWidth, size.getHeight))
    
    // blit buffer
    if (pi != null)
    drag match {
      case Some((_,v)) => 
        g.drawImage(pi.buf, null, v.getX.round.toInt, v.getY.round.toInt)
      case None => 
        g.drawImage(pi.buf, null, 0, 0)
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
          val name = model.getColumnName(column)
          // value of the column as a string
          var value = model.getValueAt(row, column).asInstanceOf[String]
          // FIXME: This is an incredible ugly hack to check if we area
          //   displaying enclosures instead of a real trace view.
          if (model.getColumnName(0) == "time" && value(0) == '[') {
            if (row == 0)
              value = "(-,%s)".format(value)
            else if (row == model.getRowCount() - 1)
              value = "(%s,-)".format(value)
            else {
              if (row % 2 == 0) row -= 1
              value = "(%s,%s)".format(model.getValueAt(row, column).asInstanceOf[String],
                                       model.getValueAt(row + 1, column).asInstanceOf[String])
            }
          }
          // publish a pointed at event
          pub.publish(PointedAtEvent(qt, name, value))
          // update the green dot Y coordinate
          val (scale, shift) = pd.yTransformations(hb)
          dotY = for (y <- model.getDouble(row, column))
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
          viewPort.getX - v.getX / pi.transform.getScaleX, 
          viewPort.getY - v.getY / pi.transform.getScaleY,
          viewPort.getWidth, 
          viewPort.getHeight)
        drag = None
        plotter ! Repaint(viewPort)
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
          plotter ! Repaint(viewPort)
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
      plotter ! Repaint
  }

  def zoomAround(cx:Double, cy:Double, factor:Double) =  {
    val f = 1/factor
    val (p1x, p1y) = ((viewPort.getMinX-cx) * f + cx, 
                      (viewPort.getMinY-cy) * f + cy)
    val (p2x, p2y) = ((viewPort.getMaxX-cx) * f + cx, 
                      (viewPort.getMaxY-cy) * f + cy)
    pushCurrentViewport
    viewPort.setFrameFromDiagonal(p1x, p1y, p2x, p2y)
    plotter ! Repaint(viewPort)
  }

  def zoom(factor:Double) =
    zoomAround(viewPort.getCenterX, viewPort.getCenterY, factor)
    
  val plotter = new PlotActor(this)
  plotter.start()
  
  def toggleSimulator(b:Boolean) = {
    pp = pp.copy(plotSimulator = b)
    hoveredBox = None
    plotter ! Replot
  }
  def toggleNextChild(b:Boolean) = {
    pp = pp.copy(plotNextChild = b)
    hoveredBox = None
    plotter ! Replot
  }
  def toggleSeeds(b:Boolean) = {
    pp = pp.copy(plotSeeds = b)
    hoveredBox = None
    plotter ! Replot
  }
  def setPlotStyle(ps:PlotStyle) = {
    plotStyle = ps
    plotter ! Repaint(viewPort)
  }

  def render(file:File, w:Int, h:Int) = {
    // FIXME: This should't be done on the EDT...
    val buf = new BufferedImage(w, h, BufferedImage.TYPE_INT_ARGB)
    new PlotImage(pd, buf, plotStyle, viewPort)
    ImageIO.write(buf, "PNG", file)
  }
}

