package acumen
package ui
package plot

import scala.collection.immutable.SortedSet
import scala.collection.JavaConversions._
import scala.collection.mutable.{ArrayBuffer,ArrayBuilder,Map=>MutMap}
import scala.swing._
import scala.collection.mutable
import java.awt.{AlphaComposite, BasicStroke, Color, RenderingHints}
import java.awt.geom.{AffineTransform, Area}
import java.awt.geom.{Line2D, Path2D, Point2D, Rectangle2D}
import java.awt.image.BufferedImage

import Errors._
import interpreters._
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
  private var p1   : Point2D.Double = null // Bottom left corner
  private var p2   : Point2D.Double = null // Top right corner
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

/**
 *  Collects <add> and sorts <sortValues> the data from
 *  a discrete plot (X: double, Y: sets of strings)
 *  and creates <result> a DiscretePath for the Plotter
 */
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

class EnclosurePath(val palette: Palette, val outline: Option[Color] = None) extends PlotEntity {
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
  /** Get the read-only polys list */
  def getPolys = polys.toArray
  
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
  
  def buildArea(g:Graphics2D, tr:AffineTransform) = {
    val polyPath = new Path2D.Double
    for (polyPoints <- polys) {
      val toDraw = PolyPoints(new Point2D.Double, new Point2D.Double,
                              new Point2D.Double, new Point2D.Double)
      polyPoints.transform(tr, toDraw)
      polyPath.moveTo(toDraw.a.getX, toDraw.a.getY)
      polyPath.lineTo(toDraw.b.getX, toDraw.b.getY)
      polyPath.lineTo(toDraw.c.getX, toDraw.c.getY)
      polyPath.lineTo(toDraw.d.getX, toDraw.d.getY)
      // Draw exact solutions as a line
      if (outline.nonEmpty && toDraw.a == toDraw.d && toDraw.b == toDraw.c){
        val prevColor = g.getColor
        g.setColor(outline.get)
        g.draw(new Line2D.Double(toDraw.a.getX, toDraw.a.getY,
          toDraw.b.getX, toDraw.b.getY))
        g.setColor(prevColor)
      }
    }
    new Area(polyPath)
  }

  def draw(g:Graphics2D, tr:AffineTransform) = {
    g.setStroke(new BasicStroke(1))
    val area = buildArea(g, tr)
    if(outline.isDefined) {
      val prevColor = g.getColor
      g.setColor(outline.get)
      g draw area
      g.setColor(prevColor)
    }
    val prevComposite = g.getComposite
    g setComposite AlphaComposite.getInstance(AlphaComposite.SRC_OVER, 0.10f)
    g fill area
    g setComposite prevComposite
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

class HollowEnclosurePath(palette: Palette) extends EnclosurePath(palette = palette, outline = Some(Palette.unapply(palette).get._1)) {
  override def draw(g: Graphics2D, tr: AffineTransform) = {
    g.setStroke(new BasicStroke(1))
    val area = buildArea(g, tr)
    g draw area
  }
}

class PlotData(parms: PlotParms = null, tb:PlotModel = null, val disableThreshold: Int = 24) 
{
  /*private*/ var polys = new ArrayBuffer[ArrayBuffer[PlotEntity]]
  /*private*/ var axes  = new ArrayBuffer[MyPath2D]
  var boxes = new ArrayBuffer[Rectangle2D]
  var boundingBox = (0.0, 0.0)

  var time = Map.empty[Tag, IndexedSeq[Double]]
  var yTransformations = new ArrayBuffer[(Double, Double)]
  var tags = Set.empty[Tag] //May contain tags comming from _plot() in the future
  var deadTags = if (tb == null) Set.empty[Tag] else tb.getDeadTags
  val plottables = if (tb == null) Nil else tb.getPlottables(parms, tags)
  val probaPlottables = if (tb == null) None else tb.getProba
  var valToTimeTr : (Double, Double) = (1, 0) //Parameters used in the affine transformation of the values in the probaPlot section
  val disabled = plottables.size >= disableThreshold
  var columnIndices = new ArrayBuffer[ArrayBuffer[(Tag, Int)]]

  if (plottables.isEmpty|| disabled) ()
  else {
    tags = tb.getTags //Because no tag have been sent to getPlottables, they are all present
    time = (tags map (t => t ->  tb.getTimes(t))).toMap

    //Groups of plottables to be plot on the same graph
    val plotGroups = plottables groupBy(x => (x.key.objId, x.key.fieldName.x, x.key.fieldName.primes, x.key.vectorIdx))
    //Lexicogrphic ordering of graphs ordering
    val ordered_keys = plotGroups.unzip._1.toList.sortWith {
      case ((cid1, n1, p1, _), (cid2, n2, p2, _)) =>
        cid1 < cid2 || cid1 == cid2 && (n1 < n2 || n1 == n2 && p1 < p2)
    }

    for(k <- ordered_keys) {
      val columnIndicesGp = new ArrayBuffer[(Tag, Int)]
      val pg = plotGroups(k)
      val plotGroup = new ArrayBuffer[PlotEntity]

      //Used only for the zero axe
      val smallestStartTime =  pg.foldLeft(time(pg.head.key.tag)(pg.head.startFrame)) {
        case (st, p) =>
          val st0 = time(p.key.tag)(p.startFrame)
          if (st < st0) st else st0
      }
      val biggestEndTime = pg.foldLeft(time(pg.head.key.tag)(pg.head.startFrame + pg.head.values.size - 1)) {
        case (et, p) =>
          val et0 = time(p.key.tag)(p.startFrame + p.values.size - 1)
          if (et > et0) et else et0
      }


      val ax = new MyPath2D(pg.head.palette)
      ax startAt (smallestStartTime, 0)
      ax goTo (biggestEndTime, 0)
      axes += ax

      var containsEnclosures = false
      var outlinePath = new EnclosurePath(Palette(Color.white))
      val pathoutline = new HollowEnclosurePath(Palette(Color.black))
      for(p <- pg) {
        val t = p.key.tag
        columnIndicesGp += t -> p.column
        p match {
          case p: PlotDoubles => {
            var invalid = false
            val line = new MyPath2D(p.palette)
            line startAt(time(t)(p.startFrame), p.values(0))
            for (f <- p.values.indices ; frame = p.startFrame + f ; value = p.values(f)) {
              invalid = invalid || value.isNaN || value.isInfinity
              line goTo(time(t)(frame), value)
            }
            if (!invalid) plotGroup += line
          }
          case p: PlotDiscrete => {
            val lines = new DiscretePathBuilder

            for (f <- p.values.indices; frame = p.startFrame + f) {
              p.values(f) match {
                case VLit(GStr(str)) => lines.add(time(t)(frame), Set(str))
                case VLit(e: GDiscreteEnclosure[_]) => lines.add(time(t)(frame), e.range.map(_.toString))
                case VLit(GInt(i)) => lines.add(time(t)(frame), Set(i.toString))
                case VLit(GBool(b)) => lines.add(time(t)(frame), Set(b.toString))
                case _ => throw ShouldNeverHappen
              }
            }

            plotGroup += lines.result(Some((a, b) => a < b), p.palette)
          }
          case p: PlotEnclosure => {
            containsEnclosures = true
            var i = 0
            val t = p.key.tag
            val path = new EnclosurePath(p.palette)
            var prevTime = time(t)(p.startFrame)
            for (f <- 1 until p.values.size;
                 frame = p.startFrame + f) {
              val curTime = time(t)(frame)
              if (time(t)(frame - 1) != curTime)
                prevTime = time(t)(frame - 1)
              path.add(prevTime, curTime, p.values(f))
              pathoutline.add(prevTime, curTime, p.values(f))
            }
            path.x1
            plotGroup += path
          }
        }
      }
      if(containsEnclosures && deadTags.isEmpty)
        plotGroup += pathoutline
      polys += plotGroup
      columnIndices += columnIndicesGp
    }

    //normalize (scale) ; only for temporal plots
    for (((pg,a),i) <- polys zip axes zipWithIndex ; if (pg.nonEmpty)) {
      //Vertical scaling in respect of the global height with arbitrary maximum
      val globalYBounds = pg.foldLeft((pg.head.y1, pg.head.y2))((y, p) => (Math.min(y._1, p.y1), Math.max(y._2, p.y2)))
      val scale =  - 1 / Math.max(1e-4, globalYBounds._2 - globalYBounds._1)
      val tr1 = AffineTransform.getScaleInstance(1.0, scale)
      pg foreach {_.transform(tr1)}
      a.transform(tr1)

      val shift = i*1.2 - pg.foldLeft(pg.head.y1)((y, p) => Math.min(y, p.y1))
      val tr2 = AffineTransform.getTranslateInstance(0.0, shift)
      pg foreach {_.transform(tr2)}
      a.transform(tr2)

      yTransformations += ((scale, shift))
      val globalx1 = pg.foldLeft(pg.head.x1)((x, p) => Math.min(x, p.x1))
      val globalWidth = pg.foldLeft(pg.head.x2)((x, p) => Math.max(x, p.x2)) - globalx1
      boxes += new Rectangle2D.Double(globalx1, i*1.2, globalWidth, 1.0)
    }
    val maxTime = tags.foldLeft(time.values.head.last)((t, tag) => Math.max(t, time(tag).last))

    //Add the probability graphs
    //The global time range of the plotted values is supposed to be [0, 10] for the explanation
    //Whatever the range of the values and of the probabilities, they are scaled to use the [1, 9] range for the values
    //and the vertical axis is always [0, 1] for the cdf and pdf.
    //The [0, 1] and [9, 10] ranges represent actually all the values respectively under and above the the min and
    //the max of the values in the cdf and the pdf.
    probaPlottables match {
      case Some(probaPlot) =>
        val probPlot = probaPlot
        val cdfEnclosure = new EnclosurePath(Palette(Color.red), outline = Some(Color.blue))
        var pdfBoxes = List.empty[EnclosurePath]
        //These to lines reopen the plots to show that they are not bounded and they fix the vertical range at [0, 1]
        val (leftLine1, rightLine1) = (new MyPath2D(Palette(Color.white)), new MyPath2D(Palette(Color.white)))
        val (leftLine2, rightLine2) = (new MyPath2D(Palette(Color.white)), new MyPath2D(Palette(Color.white)))

        val probaOut = probaPlot.probaOut
        val range = probaPlot.getValuesRange
        val a = 0.8 * maxTime / (range.hiDouble - range.loDouble)
        val b = - range.loDouble * a + 0.1 * maxTime
        valToTimeTr = (a, b)
        def affineTransform(x: Double) = a * x + b

        //The CDF is only one enclosurePath
        for((v, p) <- probaPlot.cdf){
          val enc = Enclosure(p.loDouble, p.hiDouble, p.loDouble, p.hiDouble)
          cdfEnclosure.add(affineTransform(v.loDouble), affineTransform(v.hiDouble), enc)
        }
        //Add the extremities (little extras to avoid numerical glitches)
        cdfEnclosure.add(0, 0.100000001 * maxTime,
          Enclosure(0, probaOut.hiDouble, 0, probaOut.hiDouble))
        cdfEnclosure.add(0.899999999 * maxTime, maxTime,
          Enclosure(1 - probaOut.hiDouble, 1, 1 - probaOut.hiDouble, 1))

        //The PDF is a list of enclosurePaths which will correspond to each box "p1 < P(a < x < b) < p2".
        for((v, p) <- probaPlot.pdf){
        val scale = 1 / v.width.loDouble
          val enc = Enclosure(p.loDouble*scale, p.hiDouble*scale, p.loDouble*scale, p.hiDouble*scale)
          val box = new EnclosurePath(Palette(Color.red), outline = Some(Color.blue))
          box.add(affineTransform(v.loDouble), affineTransform(v.hiDouble), enc)
          pdfBoxes = box :: pdfBoxes
        }
        //Add the extremities (always null mean densiy for now)
        //FIXME: Which width for inifnite values ?
        val firstBox = new EnclosurePath(Palette(Color.red), outline = Some(Color.blue))
          firstBox.add(0, 0.1 * maxTime, Enclosure(0, 0, 0, 0))
        val lastBox = new EnclosurePath(Palette(Color.red), outline = Some(Color.blue))
          lastBox.add(0.9 * maxTime, maxTime, Enclosure(0, 0, 0, 0))
        pdfBoxes = firstBox :: lastBox :: pdfBoxes

        //Build the plotGroups (left and right line at the end to be drawn on top of the rest)
        leftLine1.startAt(0, 0); leftLine1.goTo(0, 1)
        rightLine1.startAt(maxTime, 0); rightLine1.goTo(maxTime, 1)
        val pgCdf = new ArrayBuffer[PlotEntity]
        pgCdf += cdfEnclosure ; pgCdf += leftLine1 ; pgCdf += rightLine1

        leftLine2.startAt(0, pdfBoxes.minBy(_.y1).y1); leftLine2.goTo(0, pdfBoxes.maxBy(_.y2).y2)
        rightLine2.startAt(maxTime, leftLine2.y1); rightLine2.goTo(maxTime, leftLine2.y2)
        val pgPdf = new ArrayBuffer[PlotEntity]
        pdfBoxes foreach (pgPdf += _) ; pgPdf += leftLine2 ; pgPdf += rightLine2

        //scallings
        val globalYBounds1 = pgCdf.foldLeft((pgCdf.head.y1, pgCdf.head.y2))((y, p) => (Math.min(y._1, p.y1), Math.max(y._2, p.y2)))
        val scale1 =  - 1 / Math.max(1e-4, globalYBounds1._2 - globalYBounds1._1)
        val tr1 = AffineTransform.getScaleInstance(1.0, scale1)
        pgCdf foreach {_.transform(tr1)}
        val globalYBounds2 = pgPdf.foldLeft((pgPdf.head.y1, pgPdf.head.y2))((y, p) => (Math.min(y._1, p.y1), Math.max(y._2, p.y2)))
        val scale2 =  - 1 / Math.max(1e-4, globalYBounds2._2 - globalYBounds2._1)
        val tr2 = AffineTransform.getScaleInstance(1.0, scale2)
        pgPdf foreach {_.transform(tr2)}
        val shift1 = polys.size*1.2 - pgCdf.foldLeft(pgCdf.head.y1)((y, p) => Math.min(y, p.y1))
        val tr3 = AffineTransform.getTranslateInstance(0.0, shift1)
        pgCdf foreach {_.transform(tr3)}
        val shift2 = (polys.size + 1)*1.2 - pgPdf.foldLeft(pgPdf.head.y1)((y, p) => Math.min(y, p.y1))
        val tr4 = AffineTransform.getTranslateInstance(0.0, shift2)
        pgPdf foreach {_.transform(tr4)}

        //Add the shifted plot groups, boxes and null axes to keep the same size for polys, axes and boxes
        polys += pgCdf ; polys += pgPdf
        boxes += new Rectangle2D.Double(0, axes.size*1.2, maxTime, 1.0)
        boxes += new Rectangle2D.Double(0, (axes.size + 1)*1.2, maxTime, 1.0)
        axes += null; axes += null
        yTransformations += ((scale1, shift1), (scale2, shift2))
      case _ =>
    }

    boundingBox = (maxTime, 1.2*polys.size - 0.2)
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
    for (((pg,a),b) <- pd.polys zip pd.axes zip pd.boxes) {
      // fill bounding box
      bg.setPaint(pg.head.palette.boundingBox)
      bg.fill(applyTrR(tr, b))

      // draw y=0 axis
      bg.setPaint(pg.head.palette.yAxis)
      bg.setStroke(new BasicStroke(1.0f))
      if (a != null) a.draw(bg, tr)
      for(p <- pg) {
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
    }
    buf.flush
  }
}
