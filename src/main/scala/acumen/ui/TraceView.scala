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
import java.awt.geom.Point2D
import java.awt.geom.Line2D
import java.awt.geom.Rectangle2D
import java.awt.geom.Ellipse2D
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

class TraceView(
	plotSimulator:Boolean, plotNextChild:Boolean, 
	plotSeeds:Boolean, tmodel: AbstractTraceModel) 
  extends BorderPanel with TableModelListener {
    /* for some reason forwarding events causes stack overflows, so we pass
       this to Plotter, which will ask this to publish events ... */
    private val plotter = 
			new Plotter(plotSimulator, plotNextChild, plotSeeds, tmodel, this)

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
				val cp = new ChoicePanel
        cp.pack
        cp.open
      }

      class ChoicePanel extends Dialog(null) {
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

class MyPath2D() {
  private var p1   : Point2D.Double = null
  private var p2   : Point2D.Double = null
  private var path : ArrayBuffer[Point2D.Double] = null

  def x1 = p1.getX
  def y1 = p1.getY
  def x2 = p2.getX
  def y2 = p2.getY
  def width = x2 - x1
  def height = y2 - y1

  private def update(x:Double, y:Double) = {
    val minX = math.min(x1, x)
    val maxX = math.max(x2, x)
    val minY = math.min(y1, y)
    val maxY = math.max(y2, y)
    p1 = new Point2D.Double(minX, minY)
    p2 = new Point2D.Double(maxX, maxY)
  }
  def startAt(x:Double, y:Double) = {
    p1 = new Point2D.Double(x,0)
    p2 = new Point2D.Double(x,0)
    update(x,y)
    path = ArrayBuffer(new Point2D.Double(x,y))
  }
  def goTo(x:Double, y:Double) = {
    path += new Point2D.Double(x,y)
    update(x,y)
  }
  def draw(g:Graphics2D, tr:AffineTransform) = {
    if (path.size >= 2) {
      for (i <- 0 until (path.size-1)) {
        val p0 = new Point2D.Double()
        val p1 = new Point2D.Double()
        tr.transform(path(i), p0)
        tr.transform(path(i+1), p1)
        g.draw(new Line2D.Double(p0, p1))
      }
    }
  }
  def drawDots(g:Graphics2D, tr:AffineTransform) = {
    if (path.size >= 2) {
      for (i <- 0 until path.size) {
        val p = new Point2D.Double()
        tr.transform(path(i), p)
        g.fillRect(p.getX.toInt, p.getY.toInt, 2, 2)
      }
    }
  }
  def transform(tr:AffineTransform) = {
    for (p <- path) tr.transform(p, p)
    tr.transform(p1, p1)
    tr.transform(p2, p2)
    val minX = math.min(x1, x2)
    val maxX = math.max(x1, x2)
    val minY = math.min(y1, y2)
    val maxY = math.max(y1, y2)
    p1 = new Point2D.Double(minX, minY)
    p2 = new Point2D.Double(maxX, maxY)
  }
}

case class PointedAtEvent(time:Double, name:String, value:String) extends Event

class Plotter(
  _plotSimulator: Boolean, _plotNextChild:Boolean, 
	_plotSeeds:Boolean, tb:AbstractTraceModel, pub:Publisher) extends Panel {

  background = Color.gray
  peer.setDoubleBuffered(true)
 
  private val selectionBorderColor = new Color(68, 127, 231)
  private val selectionFillColor   = new Color(165, 189, 231, 150)
 
  private var buffer : BufferedImage = null
  type Poly = Pair[MyPath2D, MyPath2D];
  private var polys = new ArrayBuffer[Poly]()
  private var axes  = new ArrayBuffer[MyPath2D]()
  private var boxes = new ArrayBuffer[Rectangle2D]()
  private var hoveredBox : Option[Int] = None
  private var dotX : Double = 0.0d
  private var dotY : Option[Double] = None
  private var selection : Option[(Point2D, Point2D)] = None
  private var drag : Option[(Point2D,Point2D)] = None
  private var boundingBox = (0.0, 0.0)

  private var viewPort : Rectangle2D = new Rectangle2D.Double(0,0,0,0)
  private val undoStack = new Stack[Rectangle2D] 
  private var viewChanged = true
  private var transform = new AffineTransform()
  private var time = new ArrayBuffer[Double]
  private var columnIndices = new ArrayBuffer[Int]
  private var yTransformations = new ArrayBuffer[(Double, Double)]

  private var plotNextChild = _plotNextChild
  private var plotSimulator = _plotSimulator
  private var plotSeeds = _plotSeeds
  private var plotStyle : PlotStyle = Lines()

  def clear = {
    buffer = null
    polys = new ArrayBuffer[Poly]()
    axes  = new ArrayBuffer[MyPath2D]()
    boxes = new ArrayBuffer[Rectangle2D]()
    hoveredBox = None
    dotX = 0.0d
    dotY = None
    selection = None
    drag = None
    boundingBox = (0.0, 0.0)
    viewPort = new Rectangle2D.Double(0,0,0,0)
    undoStack.clear 
    viewChanged = true
    transform = new AffineTransform()
    time = new ArrayBuffer[Double]
    columnIndices = new ArrayBuffer[Int]
    yTransformations = new ArrayBuffer[(Double, Double)]
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
      val bb = new Rectangle2D.Double(0, 0, boundingBox._1, boundingBox._2)
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
    for (((p,a),b) <- polys zip axes zip boxes) {

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
          p._1.drawDots(bg, tr) 
          if (p._2 != null)
            p._2.drawDots(bg, tr) 
        case Lines() => 
          p._1.draw(bg, tr)
          if (p._2 != null)
            p._2.draw(bg, tr) 
        case Both() =>
          p._1.draw(bg, tr)
          p._1.drawDots(bg, tr) 
          if (p._2 != null)
            p._2.draw(bg, tr) 
            p._2.drawDots(bg, tr) 
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
        val rec = applyTr(boxes(hb))
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

  private def plotit(p: Plottable) = {
    (plotSimulator || !p.simulator) && 
    (plotNextChild || p.fn != Name("nextChild",0)) &&
		(plotSeeds || (p.fn != Name("seed1",0) && p.fn !=  Name("seed2",0)))
  }

  def redraw = {
  
    polys = new ArrayBuffer[Poly]
    axes  = new ArrayBuffer[MyPath2D]
    boxes = new ArrayBuffer[Rectangle2D]
    yTransformations = new ArrayBuffer[(Double,Double)]
    
    if (tb.isEmpty) ()
    else {

      time = tb.getTimes()

      columnIndices = new ArrayBuffer[Int]
      for ((p, idx) <- (tb.getPlottables() zipWithIndex) if plotit(p)) {
        val s = p.startFrame
        val a = p.values
        columnIndices += idx
        var line : Poly = null;
        val ax = new MyPath2D()
        var firstPoint = true
        for (f <- 0 until a.size; 
             val frame = s+f) {
          ax startAt (time(frame), 0)
          ax goTo (time(math.min(s+a.size, time.size-1)), 0)
          axes += ax
          a(f) match {
            case VLit(x@(GDouble(_)|GInt(_))) =>
              if (firstPoint) {
                line = new Poly(new MyPath2D(), null)
                line._1 startAt (time(frame), extractDouble(x))
                firstPoint = false
              } else {
                line._1 goTo (time(frame), extractDouble(x))
              }
            case VLit(x@(GInterval(lo,hi))) =>
              if (firstPoint) {
                line = new Poly(new MyPath2D(), new MyPath2D())
                line._1 startAt (time(frame), hi)
                line._2 startAt (time(frame), lo)
                firstPoint = false
              } else {
                line._1 goTo (time(frame), hi)
                line._2 goTo (time(frame), lo) 
             }
            case _ => throw ShouldNeverHappen()
          }
        }
        polys += line
      }
  
      //normalize (scale)
      for (((p,a),i) <- polys zip axes zip Stream.from(0)) {
        var scale = 1.0
        if (p._1.height > 1e-4) {
          scale = -1.0 / p._1.height
          val tr1 = AffineTransform.getScaleInstance(1.0, scale)
          p._1.transform(tr1)
          if (p._2 != null)
            p._2.transform(tr1)
          a.transform(tr1)
        }
        val shift = i*1.2 - p._1.y1
        val tr2 = AffineTransform.getTranslateInstance(0.0, shift)
        p._1.transform(tr2)
        if (p._2 != null)
          p._2.transform(tr2)
        a.transform(tr2)

        yTransformations += ((scale, shift))
        boxes += new Rectangle2D.Double(p._1.x1, i*1.2, p._1.width, 1.0)
      }
      boundingBox = (time(time.size-1), 1.2*polys.size - 0.2)
    }
    viewChanged = true
    repaint
  }

  /* mouse handling */

  // binary search
  private def findTimeIndex(t:Double) : (Double, Int) = {
    def findIn(min:Int, max:Int) : (Double, Int) = {
      if (max - min <= 1) {
        val tmax = time(max)
        val tmin = time(min)
        if (math.abs(tmax-t) < math.abs(tmin-t)) 
          (tmax, max)
        else (tmin, min)
      }
      else {
        val middle = (max+min)/2
        if (time(middle) < t) findIn(middle, max)
        else findIn(min, middle)
      }
    }
    if (time.isEmpty) (0,0)
    else findIn(0, time.size-1)
  }

  listenTo(mouse.moves)
  listenTo(mouse.clicks)
  //listenTo(mouse.wheel)
  listenTo(this)

  reactions += {
    case MouseMoved(_, p, _) =>
      if (columnIndices.size > 0) {
        // p in the original coordinate system
        val op = unapplyTr(p)
        // quantized time and row in the trace model corresponding to p.x
        val (qt, row) = findTimeIndex(op.getX)
        // update the vertical line (and dot) x coordinate
        dotX = applyTr(new Point2D.Double(qt, 0)).getX

        // index of the curve pointed by the mouse
        val hb = (op.getY / 1.2).toInt
        if (hb < columnIndices.length) {
          // corresponding column in the trace model
          val column = columnIndices(hb)
          // name of the column
          val name = tb.getColumnName(column)
          // value of the column as a string
          val value = tb.getValueAt(row, column).asInstanceOf[String]
          // publish a pointed at event
          pub.publish(PointedAtEvent(qt, name, value))
          // update the green dot Y coordinate
          val (scale, shift) = yTransformations(hb)
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

