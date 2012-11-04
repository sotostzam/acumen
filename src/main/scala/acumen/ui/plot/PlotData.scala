package acumen
package ui
package plot

import Errors._
import interpreters._
import java.awt.{AlphaComposite, BasicStroke, Color, RenderingHints}
import java.awt.geom.{AffineTransform, Area}
import java.awt.geom.{Line2D, Path2D, Point2D, Rectangle2D}
import java.awt.image.BufferedImage
import scala.collection.JavaConversions._
import scala.collection.mutable.ArrayBuffer
import scala.swing._
import util.Canonical._
import util.Conversions._
import interpreter._

sealed trait PlotEntity { // FIXME: BetterName
  def x1 : Double
  def y1 : Double
  def x2 : Double
  def y2 : Double
  def width = x2 - x1
  def height = y2 - y1
  def draw(g:Graphics2D, tr:AffineTransform);
  def drawDots(g:Graphics2D, tr:AffineTransform);
  def transform(tr:AffineTransform);
}

class MyPath2D() extends PlotEntity {
  private var p1   : Point2D.Double = null
  private var p2   : Point2D.Double = null
  private var path : ArrayBuffer[Point2D.Double] = null

  def x1 = p1.getX
  def y1 = p1.getY
  def x2 = p2.getX
  def y2 = p2.getY

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

class EnclosurePath() extends PlotEntity {
  private var p1  = new Point2D.Double(0,0)
  private var p2  = new Point2D.Double(0,0)
  case class PolyPoints(a:Point2D.Double, b:Point2D.Double,
                        c:Point2D.Double, d:Point2D.Double) 
  {
    def transform(tr:AffineTransform, res: PolyPoints) = {
      tr.transform(a,res.a)
      tr.transform(b,res.b)
      tr.transform(c,res.c)
      tr.transform(d,res.d)
    }
  }
  private var polys = new ArrayBuffer[PolyPoints]

  def x1 = p1.getX
  def y1 = p1.getY
  def x2 = p2.getX
  def y2 = p2.getY

  private def update(x:Double, y:Double) = {
    val minX = math.min(x1, x)
    val maxX = math.max(x2, x)
    val minY = math.min(y1, y)
    val maxY = math.max(y2, y)
    p1 = new Point2D.Double(minX, minY)
    p2 = new Point2D.Double(maxX, maxY)
  }
  def add(x0: Double, x1:Double, ys: Enclosure) {
    polys += PolyPoints(new Point2D.Double(x0,ys.hiLeft),
                        new Point2D.Double(x1,ys.hiRight),
                        new Point2D.Double(x1,ys.loRight),
                        new Point2D.Double(x0,ys.loLeft))
    update(x0,ys.hiLeft);
    update(x1,ys.hiRight);
    update(x1,ys.loRight);
    update(x0,ys.loLeft);
  }
  def draw(g:Graphics2D, tr:AffineTransform) = {
    val area = new Area();
    val lines = new ArrayBuffer[Line2D.Double];
    g.setStroke(new BasicStroke(1))
    for (polyPoints <- polys) {
      val toDraw = PolyPoints(new Point2D.Double,
                              new Point2D.Double,
                              new Point2D.Double,
                              new Point2D.Double);
      polyPoints.transform(tr, toDraw)
      val polyPath = new Path2D.Double;
      polyPath.moveTo(toDraw.a.getX(),toDraw.a.getY())
      polyPath.lineTo(toDraw.b.getX(),toDraw.b.getY())
      polyPath.lineTo(toDraw.c.getX(),toDraw.c.getY())
      polyPath.lineTo(toDraw.d.getX(),toDraw.d.getY())
      polyPath.closePath()
      area.add(new Area(polyPath))
      // Draw exact solutions as a line
      if (toDraw.a == toDraw.d && toDraw.b == toDraw.c)
        g.draw(new Line2D.Double(toDraw.a.getX(),toDraw.a.getY(),toDraw.b.getX(),toDraw.b.getY()))
    }
    g.draw(area)
    val prevComposite = g.getComposite()
    g.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, 0.10f))
    g.fill(area)
    g.setComposite(prevComposite)
  }

  def drawDots(g:Graphics2D, tr:AffineTransform) = draw(g,tr)

  def transform(tr:AffineTransform) = {
    for (p <- polys) p.transform(tr,p)
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

case class PlotParms(plotSimulator:Boolean = false, 
		             plotNextChild:Boolean = false, 
		             plotSeeds:Boolean = false)
		             
class PlotData(parms: PlotParms = null, tb:PlotModel = null) 
{
  /*private*/ var polys = new ArrayBuffer[PlotEntity]
  /*private*/ var axes  = new ArrayBuffer[MyPath2D]
  var boxes = new ArrayBuffer[Rectangle2D]
  var boundingBox = (0.0, 0.0)

  var time : IndexedSeq[Double] = new ArrayBuffer[Double]
  var traceViewTime : IndexedSeq[Double]= new ArrayBuffer[Double]
  var columnIndices = new ArrayBuffer[Int]
  var yTransformations = new ArrayBuffer[(Double, Double)]

  private def plotit(p: Plottable) = {
    (parms.plotSimulator || !p.simulator) && 
    (parms.plotNextChild || p.fn != Name("nextChild",0)) &&
        	(parms.plotSeeds || (p.fn != Name("seed1",0) && p.fn !=  Name("seed2",0)))
  }

  if (tb == null || tb.isEmpty) ()
  else {

    time = tb.getTimes()
    traceViewTime = tb.getTraceViewTimes()

    columnIndices = new ArrayBuffer[Int]
    for (p <- tb.getPlottables() if plotit(p)) {
      var s = p.startFrame
      columnIndices += p.column

      val ax = new MyPath2D()
      ax startAt (time(s), 0)
      ax goTo (time(math.min(s+p.values.size, time.size-1)), 0)
      axes += ax

      p match {
        case p:PlotDoubles => {
          val line = new MyPath2D();
          line startAt (time(s), p.values(0))

          for (f <- 0 until p.values.size;
               val frame = s + f) {
            line goTo (time(frame), p.values(f))
          }

          polys += line
        }
        case p:PlotEnclosure => {
          val path = new EnclosurePath
          var prevTime = time(s)
          for (f <- 1 until p.values.size;
               val frame = s + f) {
            if (time(frame-1) != time(frame))
                prevTime = time(frame-1)
            path.add(prevTime, time(frame), p.values(f))
          }
          polys += path
        }
      }
    }
    //normalize (scale)
    for (((p,a),i) <- polys zip axes zip Stream.from(0)) {
      var scale = 1.0
      if (p.height > 1e-4) {
        scale = -1.0 / p.height
        val tr1 = AffineTransform.getScaleInstance(1.0, scale)
        p.transform(tr1)
        a.transform(tr1)
      }
      val shift = i*1.2 - p.y1
      val tr2 = AffineTransform.getTranslateInstance(0.0, shift)
      p.transform(tr2)
      a.transform(tr2)

      yTransformations += ((scale, shift))
      boxes += new Rectangle2D.Double(p.x1, i*1.2, p.width, 1.0)
    }
    boundingBox = (time(time.size-1), 1.2*polys.size - 0.2)
  }
}

class PlotImage(pd: PlotData, 
                val buf: BufferedImage, plotStyle: PlotStyle = null,
                var viewPort : Rectangle2D = null) 
{
  if (viewPort == null) {
    val bb = new Rectangle2D.Double(0, 0, pd.boundingBox._1, pd.boundingBox._2)
    val tr = computeTransform(buf.getWidth, buf.getHeight, bb)
    val trbb = applyTrR(tr, bb)
    trbb.setRect(trbb.getX-10, trbb.getY-10, trbb.getWidth+20, trbb.getHeight+20)
    viewPort = unapplyTrR(tr, trbb)
  }

  val transform = computeTransform(buf.getWidth, buf.getHeight, viewPort)

  {
    val tr = transform

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
}