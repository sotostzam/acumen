package acumen
package ui

import scala.io._
import collection.mutable.ArrayBuffer
import collection.mutable.Stack
import collection.JavaConversions._
import swing._
import swing.event._
import java.lang.Thread
import java.util.TimerTask
import java.util.Timer
import java.io.File
import java.awt.Color
import java.awt.Transparency
import java.awt.BasicStroke
import java.awt.RenderingHints
import java.awt.GraphicsEnvironment
import java.awt.Shape
import java.awt.AlphaComposite
import java.awt.geom.Area
import java.awt.geom.Point2D
import java.awt.geom.Line2D
import java.awt.geom.Rectangle2D
import java.awt.geom.Ellipse2D
import java.awt.geom.Path2D
import java.awt.geom.AffineTransform
import java.awt.image.BufferedImage
import javax.swing.event.TableModelEvent
import javax.swing.event.TableModelListener
import javax.swing.JSpinner
import javax.swing.JLabel
import javax.imageio.ImageIO
import Errors._
import util.Canonical._
import util.Conversions._


sealed abstract class PlotStyle
case class Lines() extends PlotStyle
case class Dots() extends PlotStyle
case class Both() extends PlotStyle

class TraceView(plotSimulator:Boolean, plotNextChild:Boolean, 
	        plotSeeds:Boolean, tmodel: TraceModel) 
  extends BorderPanel with TableModelListener 
{
  /* for some reason forwarding events causes stack overflows, so we pass
     this to Plotter, which will ask this to publish events ... */
  private val plotter = 
    new PlotterPanel(plotSimulator, plotNextChild, plotSeeds, tmodel, this)

  val resetZoom = new Action("Reset Zoom") {
    icon = Icons.home
    def apply = fit
    toolTip = "Reset View"
  }
  val zoomIn = new Action("Zoom In") {
    icon = Icons.zoomIn
    def apply = plotter.zoom(1.5)
    toolTip = "Zoom In"
  }
  val zoomOut = new Action("Zoom Out") {
    icon = Icons.zoomOut
    def apply = plotter.zoom(1/1.5) 
    toolTip = "Zoom Out"
  }
  val undo = new Action("Undo") {
    icon = Icons.undo
    def apply = plotter.undo
    toolTip = "Undo Previous Action"
  }
  val saveAs = new Action("Save As") {

    icon = Icons.save
    tooltip = "Save As"
    private var currentDir = new File(".")
    private def fresh = Files.getFreshFile(currentDir, "png")
    private var currentWidth = 640
    private var currentHeight = 480

    def apply : Unit = {
      val cp = new SaveAsDailog
      cp.pack
      cp.open
    }

    class SaveAsDailog extends Dialog(null) {
      modal = true
      val widthSpin = new JSpinner() { setValue(currentWidth) }
      val heightSpin = new JSpinner() { setValue(currentHeight) }
      val inputField = new TextField(fresh.getCanonicalPath,20)
      val openButton = Button("Browse") {
        val fc = new FileChooser(currentDir) {
          selectedFile = new File(inputField.text)
        }
        val returnVal = fc.showOpenDialog(TraceView.this)
        if (returnVal == FileChooser.Result.Approve) {
          val file = fc.selectedFile
          inputField.text = file.getAbsolutePath
        }
      }
      val upperPane = new GridPanel(2,2) {
        border = javax.swing.BorderFactory.createEmptyBorder(5,5,5,5)
        vGap = 5
        peer.add(new JLabel("Width"))
        peer.add(widthSpin)
        peer.add(new JLabel("Height"))
        peer.add(heightSpin)
      }
      val lowerPane =
        new FlowPanel(FlowPanel.Alignment.Leading)(inputField,openButton)
      val options = new BoxPanel(Orientation.Vertical) {
        contents += (upperPane, lowerPane)
      }
      val cancel = Button("Cancel")(dispose)
      val save = Button("Save") {
        val f = new File(inputField.text)
        currentDir = f.getParentFile
        currentHeight  = heightSpin.getValue.asInstanceOf[Int]
        currentWidth = widthSpin.getValue.asInstanceOf[Int]
        plotter.render(f, currentWidth, currentHeight)
        dispose
      }
      val buttons = new FlowPanel(FlowPanel.Alignment.Trailing)(cancel,save)
      contents = new BorderPanel {
        add(options, BorderPanel.Position.Center)
        add(buttons, BorderPanel.Position.South)
      }
    }
  }

  private val b1 = new Button(resetZoom) { peer.setHideActionText(true) }
  private val b2 = new Button(undo) { peer.setHideActionText(true) }
  private val b3 = new Button(zoomIn) { peer.setHideActionText(true) }
  private val b4 = new Button(zoomOut) { peer.setHideActionText(true) }
  private val b5 = new Button(saveAs) { peer.setHideActionText(true) }
  private val hint = new Label("Hint: Right click on image & drag to move")
  private val check = new CheckBox("") { 
    action = Action("Draw") { 
      if (selected) redraw 
      else plotter.clear
    }
  }
  private val rightBottomButtons =
    new FlowPanel(FlowPanel.Alignment.Leading)(check, b5, b1, b2, b3, b4, hint)

  add(plotter, BorderPanel.Position.Center)
  add(rightBottomButtons, BorderPanel.Position.South)
  check.selected = true
  tmodel.addTableModelListener(this)

  def redraw : Unit = { plotter.redraw; fit }
  private def fit = plotter.fit
  override def tableChanged(e: TableModelEvent) = 
    if (check.selected) redraw

  def toggleSimulator(b:Boolean) = plotter.toggleSimulator(b)
  def toggleNextChild(b:Boolean) = plotter.toggleNextChild(b)
  def toggleSeeds(b:Boolean) = plotter.toggleSeeds(b)
  def setPlotStyle(ps:PlotStyle) = plotter.setPlotStyle(ps)
}

case class PointedAtEvent(time:Double, name:String, value:String) extends Event

class PlotterPanel(_plotSimulator: Boolean, _plotNextChild:Boolean, 
	           _plotSeeds:Boolean, tb:TraceModel, pub:Publisher) 
  extends Panel 
{
  background = Color.gray
  peer.setDoubleBuffered(true)

  private var pd = new PlotData(false,false,false,null)
 
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

  private var plotNextChild = _plotNextChild
  private var plotSimulator = _plotSimulator
  private var plotSeeds = _plotSeeds
  private var plotStyle : PlotStyle = Lines()

  def clear = {
    pd = new PlotData(false,false,false,null)
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

  def applyTr(tr: AffineTransform, p: Point2D) : Point2D = {
    val res = new Point2D.Double
    tr.transform(p,res)
    res 
  }

  def applyTr(p:Point2D) : Point2D = applyTr(transform, p)

  def applyTr(tr: AffineTransform, r: Rectangle2D) : Rectangle2D = {
    val p1 = applyTr(tr, new Point2D.Double(r.getMinX, r.getMinY))
    val p2 = applyTr(tr, new Point2D.Double(r.getMaxX, r.getMaxY))
    val res = new Rectangle2D.Double()
    res.setFrameFromDiagonal(p1, p2)
    res
  }
  
  def applyTr(r:Rectangle2D) : Rectangle2D = applyTr(transform, r)

  def unapplyTr(tr: AffineTransform, p: Point2D) : Point2D = {
    val res = new Point2D.Double
    tr.inverseTransform(p,res)
    res
  }

  def unapplyTr(p:Point2D) : Point2D = unapplyTr(transform, p)

  def unapplyTr(tr:AffineTransform, r: Rectangle2D) : Rectangle2D = {
    val p1 = unapplyTr(tr, new Point2D.Double(r.getMinX, r.getMinY))
    val p2 = unapplyTr(tr, new Point2D.Double(r.getMaxX, r.getMaxY))
    val res = new Rectangle2D.Double()
    res.setFrameFromDiagonal(p1, p2)
    res
  }

  def unapplyTr(r:Rectangle2D) : Rectangle2D = unapplyTr(transform, r)

  def fit : Unit = {
    if (buffer != null) {
      undoStack.clear
      val bb = new Rectangle2D.Double(0, 0, pd.boundingBox._1, pd.boundingBox._2)
      val tr = computeTransform(buffer.getWidth, buffer.getHeight, bb)
      val trbb = applyTr(tr, bb)
      trbb.setRect(trbb.getX-10, trbb.getY-10, trbb.getWidth+20, trbb.getHeight+20)
      viewPort = unapplyTr(tr, trbb)
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
      bg.fill(applyTr(tr, b))

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
    pd = new PlotData(plotSimulator,plotNextChild,plotSeeds,tb)
    viewChanged = true
    repaint
  }

  def toggleSimulator(b:Boolean) = { 
    plotSimulator = b
    hoveredBox = None
    redraw
    fit
  }
  def toggleNextChild(b:Boolean) = {
    plotNextChild = b
    hoveredBox = None
    redraw
    fit
  }
  def toggleSeeds(b:Boolean) = {
    plotSeeds = b
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

