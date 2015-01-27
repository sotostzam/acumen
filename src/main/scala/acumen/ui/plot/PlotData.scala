package acumen
package ui
package plot

import Errors._
import interpreters._
import java.awt.{AlphaComposite, BasicStroke, Color, RenderingHints}
import java.awt.geom.{AffineTransform, Area}
import java.awt.geom.{Line2D, Path2D, Point2D, Rectangle2D}
import java.awt.image.BufferedImage
import scala.collection.immutable.SortedSet
import scala.collection.JavaConversions._
import scala.collection.mutable.{ArrayBuffer,ArrayBuilder,Map=>MutMap}
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
  def draw(g:Graphics2D, tr:AffineTransform)
  def drawDots(g:Graphics2D, tr:AffineTransform)
  def transform(tr:AffineTransform)
  def palette: Palette
}

class MyPath2D(val palette: Palette) extends PlotEntity {
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
    for (i <- 0 until path.size) {
      val p = new Point2D.Double()
      tr.transform(path(i), p)
      g.fillRect(p.getX.toInt, p.getY.toInt, 2, 2)
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

class DiscretePathBuilder {
  case class Line(xs: Array[Double], y: String)
  
  private var lines = new ArrayBuffer[Line]
  private var curLines = MutMap.empty[String,ArrayBuilder[Double]]
  def add(x:Double, ys: Set[String]) = {
    ys.foreach {y =>
      curLines.get(y) match {
        case Some(line) => line += x
        case None       => curLines += ((y, (ArrayBuilder.make[Double]) += x))
        }
    }
    curLines.keys.foreach { y =>
      if (!ys.contains(y)) {
          lines += Line(curLines(y).result(), y)
        curLines -= y
      }
    }
  }
  
  def sortValues(sortWith: Option[(String,String) => Boolean] = None) : ArrayBuffer[Array[Point2D.Double]] = {
    curLines.foreach { case (y,line) =>
      lines += Line(line.result(), y)
    }
    curLines.clear()
    val yMap = MutMap.empty[String,Double]
    var counter = 0
    lines.foreach{case Line(xs, y) => 
                    if (!yMap.contains(y)) {yMap += ((y, counter)); counter += 1}}
    sortWith match {
      case Some(f) => yMap.keys.toList.sortWith(f).zipWithIndex.foreach{case (key, i) => yMap(key) = i}
      case None => /* nothing to do */
    }
    lines.map{case Line(xs, y) => xs.map{x => new Point2D.Double(x,yMap(y))}}
  }

  def result(sortWith: Option[(String,String) => Boolean] = None, palette: Palette) : DiscretePath = {
    new DiscretePath(sortValues(sortWith), palette)
  }
}

class DiscretePath(val lines: Seq[Array[Point2D.Double]], val palette: Palette) extends PlotEntity {
  var x1 = Double.MaxValue // min
  var x2 = Double.MinValue // max
  var y1 = Double.MaxValue // min
  var y2 = Double.MinValue // max
  collectBounds()
  def collectBounds() {
    lines.foreach{line => line.foreach{p => 
      x1 = x1.min(p.getX)
      x2 = x2.max(p.getX)
      y1 = y1.min(p.getY)
      y2 = y2.max(p.getY)
    }}
  }
  def draw(g:Graphics2D, tr:AffineTransform) = {
    lines.foreach{line =>
      val p0 = new Point2D.Double()
      val p1 = new Point2D.Double()
      tr.transform(line.minBy{p => p.getX}, p0)
      tr.transform(line.maxBy{p => p.getX}, p1)
      g.draw(new Line2D.Double(p0, p1))
    }
  }
  def drawDots(g:Graphics2D, tr:AffineTransform) = {
    lines.foreach{line =>
      line.foreach{p0 =>
        val p = new Point2D.Double()
        tr.transform(p0, p)
        g.fillRect(p.getX.toInt, p.getY.toInt, 2, 2)
      }
    }
  }
  def transform(tr:AffineTransform) {
    val p = new Point2D.Double()
    lines.foreach{line =>
      line.foreach{p =>
        tr.transform(p, p)
      }
    }
    collectBounds()
  }
}

class EnclosurePath(val palette: Palette) extends PlotEntity {
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
    val upper = new ArrayBuffer[Point2D.Double]
    val lower = new ArrayBuffer[Point2D.Double]
    case object FallBackToSlow extends Exception
    g.setStroke(new BasicStroke(1))
    var area = new Area()
    // set prev to special value to avoid special case
    var prev = new PolyPoints(null, polys(0).a, null, null)
    try {
      for (polyPoints <- polys) {
        if (prev == polyPoints) {/* skip */}
        else if (prev.b.getX != polyPoints.a.getX) throw FallBackToSlow
        else {
          prev = polyPoints
          val area = new Area();
          val toDraw = PolyPoints(new Point2D.Double,
                                  new Point2D.Double,
                                  new Point2D.Double,
                                  new Point2D.Double)
          polyPoints.transform(tr, toDraw)
          upper += toDraw.a
          upper += toDraw.b
          lower += toDraw.d
          lower += toDraw.c
        }
      }
      val polyPath = new Path2D.Double
      polyPath.moveTo(upper.head.getX, upper.head.getY)
      upper.tail.foreach { p => polyPath.lineTo(p.getX, p.getY) }
      lower.reverse.foreach { p => polyPath.lineTo(p.getX, p.getY) }
      polyPath.closePath()
      g.draw(polyPath)
      area = new Area(polyPath)
    } catch { 
      case FallBackToSlow =>
        println("Falling Back to Slow Plotting Method.")
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
          area.add(new Area(polyPath))
          // Draw exact solutions as a line
          if (toDraw.a == toDraw.d && toDraw.b == toDraw.c)
            g.draw(new Line2D.Double(toDraw.a.getX(),toDraw.a.getY(),toDraw.b.getX(),toDraw.b.getY()))
        }
        g.draw(area)
    }
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

class PlotData(parms: PlotParms = null, tb:PlotModel = null, val disableThreshold: Int = 24) 
{
  /*private*/ var polys = new ArrayBuffer[PlotEntity]
  /*private*/ var axes  = new ArrayBuffer[MyPath2D]
  var boxes = new ArrayBuffer[Rectangle2D]
  var boundingBox = (0.0, 0.0)

  var time : IndexedSeq[Double] = new ArrayBuffer[Double]
  var columnIndices = new ArrayBuffer[Int]
  var yTransformations = new ArrayBuffer[(Double, Double)]

  val plottables = if (tb == null) Nil else tb.getPlottables(parms)
  val disabled = plottables.size >= disableThreshold

  if (plottables.size == 0 || disabled) ()
  else {
    time = tb.getTimes()

    columnIndices = new ArrayBuffer[Int]
    for (p <- plottables) {
      var s = p.startFrame
      columnIndices += p.column

      val ax = new MyPath2D(p.palette)
      ax startAt (time(s), 0)
      ax goTo (time(math.min(s+p.values.size, time.size-1)), 0)
      axes += ax

      p match {
        case p:PlotDoubles => {
          val line = new MyPath2D(p.palette)
          line startAt (time(s), p.values(0))

          for (f <- 0 until p.values.size;
               val frame = s + f) {
            line goTo (time(frame), p.values(f))
          }

          polys += line
        }
        case p:PlotDiscrete => {

          val lines = new DiscretePathBuilder
                    
          for (f <- 0 until p.values.size;
               val frame = s + f) {
            p.values(f) match {
              case VLit(GStr(str)) => lines.add(time(frame),Set(str))
              case VLit(e:GDiscreteEnclosure[String]) => lines.add(time(frame),e.range)
              case VLit(GInt(i)) => lines.add(time(frame),Set(i.toString))
            }
          }

          polys += lines.result(Some((a,b) => a < b), p.palette)
       }
        case p:PlotEnclosure => {
          val path = new EnclosurePath(p.palette)
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
      bg.setPaint(p.palette.boundingBox)
      bg.fill(applyTrR(tr, b))

      // draw y=0 axis
      bg.setPaint(p.palette.yAxis)
      bg.setStroke(new BasicStroke(1.0f))
      a.draw(bg, tr)

      // draw curve
      bg.setPaint(p.palette.value)
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
