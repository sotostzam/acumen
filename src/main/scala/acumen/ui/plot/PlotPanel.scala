package acumen
package ui
package plot

import scala.collection.JavaConversions._
import scala.collection.mutable.{ArrayBuffer, Stack}
import scala.swing._
import scala.swing.event._

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

import net.java.jinterval.rational.ExtendedRational

import util.Canonical._
import util.Conversions._
import interpreter._
import interpreters.enclosure.{Interval, TagsGraph}

case class PointedAtEvent(time:Double, name:String, value:String) extends Event

class PlotPanel(pub:Publisher) extends Panel 
{
  import PlotPanel._

  background = Color.gray
  peer.setDoubleBuffered(true)

  private var pd : PlotData = null
  private var pi : PlotImage = null
  private var model : PlotModel = null
 
  private val selectionBorderColor = new Color(68, 127, 231)
  private val selectionFillColor   = new Color(165, 189, 231, 150)
 
  private var hoveredBox : Option[Int] = None
  private var dotX : (Double, Double) = (0.0d, 0.0d)
  private var dotY : Option[(Double, Double)] = None // Having two points allows to bound enclosures
  private var selection : Option[(Point2D, Point2D)] = None
  private var drag : Option[(Point2D,Point2D)] = None
  private var prbCursor : Option[(Double, Int)] = None

  private var viewPort : Rectangle2D = null
  private val undoStack = new Stack[Rectangle2D] 
  
  private var probaEnabled = false

  def getModel()  = {
    val m = App.ui.controller.model
    if (m != null)
      m.getPlotModel
    else
      null
  }
  val plotI = new PlotInput(getModel,mkBuffer)

  override def paint(g : Graphics2D) = {
    if (model == null) clear
    super.paint(g)
  }

  private def clear = {
    pd = new PlotData()
    pi = new PlotImage(pd, mkBuffer)
    hoveredBox = None
    dotX = (0.0d, 0.0d)
    dotY = None
    selection = None
    drag = None
    resetViewPort(new Rectangle2D.Double(0,0,0,0))
    enabled = false
    App.publish(Disabled)
  }

  def reset = {
    clear
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

  def mkBuffer() =  {
    peer.getGraphicsConfiguration.createCompatibleImage(
      size.width, size.height, Transparency.OPAQUE
    )
  }
  
  def applyTr(p:Point2D) : Point2D = applyTrP(pi.transform, p)
  def applyTr(r:Rectangle2D) : Rectangle2D = applyTrR(pi.transform, r)
  def unapplyTr(p:Point2D) : Point2D = unapplyTrP(pi.transform, p)
  def unapplyTr(r:Rectangle2D) : Rectangle2D = unapplyTrR(pi.transform, r)

  def resetViewPort(vp: Rectangle2D) = {
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

    g.draw(new Line2D.Double(dotX._1, 0, dotX._1, size.getHeight))
    g.draw(new Line2D.Double(dotX._2, 0, dotX._2, size.getHeight))

    dotY match {
      case Some((y1, y2)) =>
        g.draw(new Line2D.Double(0, y1, size.getWidth, y1))
        g.draw(new Line2D.Double(0, y2, size.getWidth, y2))
        g.draw(new Line2D.Double(dotX._2, 0, dotX._2, size.getHeight))
        g.setPaint(Color.green)
        g.fill(new Ellipse2D.Double(dotX._1-2, y1-2, 5, 5))
        g.fill(new Ellipse2D.Double(dotX._1-2, y2-2, 5, 5))
        g.fill(new Ellipse2D.Double(dotX._2-2, y1-2, 5, 5))
        g.fill(new Ellipse2D.Double(dotX._2-2, y2-2, 5, 5))
        g.setPaint(Color.gray)
        g.setStroke(new BasicStroke(0.5f))
        g.draw(new Ellipse2D.Double(dotX._1-2, y1-2, 5, 5))
        g.draw(new Ellipse2D.Double(dotX._1-2, y2-2, 5, 5))
        g.draw(new Ellipse2D.Double(dotX._2-2, y1-2, 5, 5))
        g.draw(new Ellipse2D.Double(dotX._2-2, y2-2, 5, 5))
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
    
    // draw probability cursor (if any)
    prbCursor match {
      case Some((x, hb)) =>
        val trX = applyTr(new Point2D.Double(x, 0)).getX
        val box = applyTr(pd.boxes(hb))
        val yBounds = (box.getMinY, box.getMaxY)
        g.setPaint(Color.blue)
        g.draw(new Line2D.Double(trX, yBounds._1, trX, yBounds._2))
      case None =>
    }
    
  }

  /* mouse handling */

  // binary search
  // forceSup can force to find the closest superior time. Gives bounding guarantees with enclosures.
  private def findTimeIndices(t:Double, forceSup: Boolean = false) : Map[Tag, Option[(Double, Int)]] = {
    def findIn(tag: Tag, min: Int, max: Int): Option[(Double, Int)] = {
      if (max - min <= 1) {
        val tmax = pd.time(tag)(max)
        val tmin = pd.time(tag)(min)
        //If the time range corresponding to the store tagged by tag doesn't contain t, return None
        if(tmax < t && tmin < t || tmax > t && tmin > t) None
        else if (forceSup || math.abs(tmax - t) < math.abs(tmin - t))
          Some(tmax, max)
        else Some(tmin, min)
      }
      else {
        val middle = (max + min) / 2
        if (pd.time(tag)(middle) < t) findIn(tag, middle, max)
        else findIn(tag, min, middle)
      }
    }
    (pd.tags filter (!pd.deadTags.contains(_)) map (tag =>
      if (pd.time(tag).isEmpty) tag -> None
      else tag -> findIn(tag, 0, pd.time(tag).size-1)
      )).toMap
  }

  listenTo(mouse.moves)
  listenTo(mouse.clicks)
  //listenTo(mouse.wheel)
  listenTo(this)
  listenTo(App.pub)

  var readyState = false
  var donePlotting = false

  def updateEnabled {
    val newEnabled = readyState && donePlotting
    if (newEnabled && !enabled) {
      enabled = true
      App.publish(Enabled)
    } else if (!newEnabled && enabled) {
      enabled = false
      App.publish(Disabled)
    }
  }

  reactions += {
    case App.Starting => 
      reset
    case _:App.Playing => 
      readyState = false
      updateEnabled
    case _:App.Ready => 
      readyState = true
    case m:PlotReady => 
      //println("Got PlotReady Message!")
      model = m.model
      pd = m.data
      pi = m.image
      if (m.newPlot)
        resetViewPort(pi.viewPort)
      hoveredBox = None
      repaint
      // Don't think updateEnabled is needed here -- kevina
      //updateEnabled
    
    case Plotter.Ready => 
      donePlotting = true
      updateEnabled

    case Plotter.Busy => 
      donePlotting = false
      updateEnabled

    case MouseMoved(_, p, _) =>
      if (pd != null && pd.columnIndices.nonEmpty) {
        // p in the original coordinate system
        val op = unapplyTr(p)
        // quantized times and rows in the trace model corresponding to p.x (the index value may differ)
        val qtAndRows = findTimeIndices(op.getX)
        // If several curves are plotted on the same graph, some may exist at dates where other don't
        val dotXs = qtAndRows flatMap {
          case (tag, Some((qt, r))) =>
            Some(tag -> applyTr(new Point2D.Double(qt, 0)).getX)
          case _ => None
        }
        val potTags = dotXs.keys

        // index of the curve group pointed by the mouse
        var hb = (op.getY / 1.2).toInt
        if (hb < pd.columnIndices.length && hb >= 0 && potTags.nonEmpty) {
          // corresponding columns in the trace model
          val columns = pd.columnIndices(hb).toMap
          // update the green dot Y coordinate
          //Same transformation for the whole group
          val (scale, shift) = pd.yTransformations(hb)
          val middleValues = potTags map (t => {
            val i = columns(t); t -> model.getDoubleAt(t, qtAndRows(t).get._2, i)
          })
          val boundingValues = potTags map (t => {
            val i = columns(t); t -> model.getBoundingAt(t, qtAndRows(t).get._2, i)
          })
          val middleCoordinates = for ((t, bv) <- middleValues if bv.isDefined) yield
            t -> applyTr(new Point2D.Double(0, bv.get * scale + shift)).getY
          // Ordinate of the closest point from the mouse
          val (closestTag, (closestIndex, _)) =
            middleCoordinates.foldLeft(middleCoordinates.head._1 -> (columns(middleCoordinates.head._1) -> middleCoordinates.head._2)) {
              case (prev@(t0, (i0, y0)), (t1, y1)) =>
                if (Math.abs(y1 - p.y) < Math.abs(y0 - p.y))
                  t1 -> (columns(t1) -> y1)
                else prev
            }
          dotY = if (middleValues.nonEmpty) {
            val boundingCoordinates = {
              val closestBounding = model.getBoundingAt(closestTag, qtAndRows(closestTag).get._2, columns(closestTag))
              (applyTr(new Point2D.Double(0, closestBounding.get._1 * scale + shift)).getY,
                applyTr(new Point2D.Double(0, closestBounding.get._2 * scale + shift)).getY)
            }
            Some(boundingCoordinates)
          } else None
          // names of the columns
          val name = model.getPlotTitle(closestTag, closestIndex)
          // values of the columns as a string
          val value = model.getValueAt(closestTag, qtAndRows(closestTag).get._2, closestIndex)
          // update the vertical line (and dot) x coordinate
          val x = applyTr(new Point2D.Double(qtAndRows(closestTag).get._1, 0)).getX
          dotX = (x, x) //No horizontal range here
          // updating the hovered box number
          hoveredBox = Some(hb)
          pub.publish(PointedAtEvent(qtAndRows(closestTag).get._1, name, value))
        } else if (hb == pd.columnIndices.length) {
          val pp = pd.probaPlottables.get
          // p in the original coordinate system
          val op = (unapplyTr(p).getX - pd.valToTimeTr._2) / pd.valToTimeTr._1
          val valRange = pp.getValuesRange
          var prb: Interval = null
          if(valRange.contains(op))
            prb = pp.getCdf.find { case (i, p) => i.contains(op) }.get._2
          else if (op <= valRange.loDouble)
              prb = pp.getProbaOut
          else prb = Interval(1) - pp.getProbaOut
          dotX = (p.getX, p.getX)// No horizontal range here
          val (scale, shift) = pd.yTransformations(hb)
          dotY = Some(applyTr(new Point2D.Double(0, prb.loDouble * scale + shift)).getY,
                      applyTr(new Point2D.Double(0, prb.hiDouble * scale + shift)).getY)
          hoveredBox = Some(hb)
          pub.publish(PointedAtEvent(pp.getTime, "CDF of " + pp.getName, s"P(<=$op) = " + prb.toString))
        } else if (hb == pd.columnIndices.length + 1) {
          val pp = pd.probaPlottables.get
          // p in the original coordinate system
          val op = (unapplyTr(p).getX - pd.valToTimeTr._2) / pd.valToTimeTr._1
          val valRange = pp.getValuesRange
          var value, prb: Interval = null
          if (valRange.contains(op))  {
            val res = pd.probaPlottables.get.getPdf.find { case (i, p) => i.contains(op) }.get
            value = res._1
            prb = res._2
          } else if (op <= valRange.loDouble) {
            value = Interval(ExtendedRational.NEGATIVE_INFINITY, pd.probaPlottables.get.getValuesRange.lo)
            prb = pp.getProbaOut
          } else {
            value = Interval(pd.probaPlottables.get.getValuesRange.hi, ExtendedRational.POSITIVE_INFINITY)
            prb = pp.getProbaOut
          }
          dotX = (applyTr(new Point2D.Double(value.loDouble * pd.valToTimeTr._1 + pd.valToTimeTr._2, 0)).getX,
                  applyTr(new Point2D.Double(value.hiDouble * pd.valToTimeTr._1 + pd.valToTimeTr._2, 0)).getX)
          val prbScaled = {
//            val meanUncertainty = pd.probaPlottables.get.getPdf.values.map(_.width.loDouble).sum / pd.probaPlottables.get.getPdf.size
//            val pHalfWidth = prb.width.loDouble / 2 / meanUncertainty / 10
//            val middleShifted = (prb.loDouble + prb.hiDouble) / 2 / (value.hiDouble - value.loDouble)
//            Interval(middleShifted - pHalfWidth, middleShifted + pHalfWidth)
            val scale = 1 / value.width.loDouble
            Interval(prb.loDouble * scale, prb.hiDouble * scale)
          }
          val (scale, shift) = pd.yTransformations(hb)
          dotY = Some(applyTr(new Point2D.Double(0, prbScaled.loDouble * scale + shift)).getY,
            applyTr(new Point2D.Double(0, prbScaled.hiDouble * scale + shift)).getY)
          hoveredBox = Some(hb)
          pub.publish(PointedAtEvent(pp.getTime, "PDF of " + pp.getName, "P(" + value + ") = " + prb.toString))
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
    // Left click on a graph handling
    // Here is computed the probabilities related to the clicked graph at the time clicked.
    // FIXME: It does not seems to be the best place to perform such computation but the fact that the different Stores are now asynchronous make it difficult to compute the probabilities in the producer.
    case e@MouseClicked(_, p, _, 1, _)  =>
      if(e.peer.getButton == 1) {
        // Similar to the MouseMouved event handling
        if (pd != null && pd.columnIndices.nonEmpty) {
          val op = unapplyTr(p)
          val qtAndRows = findTimeIndices(op.getX, forceSup = true)
          val tags = (qtAndRows filter { case (t, Some(_)) => true; case _ => false }).keys
          val hb = (op.getY / 1.2).toInt

          if (hb < pd.columnIndices.length && hb >= 0 && tags.nonEmpty && tags.forall(_.p.nonEmpty)) {
            prbCursor = Some((op.getX, hb))
            // FIXME: Find a better way to extract the column name without the tag
            val name = "(" + model.getPlotTitle(pd.columnIndices(hb).head._2).tail.takeWhile('(' !=)
            val columns = pd.columnIndices(hb).toMap
            val boundingValues = (tags map (t => {
              val i = columns(t)
              t -> model.getBoundingAt(t, qtAndRows(t).get._2, i)
            })).filter(_._2.nonEmpty).map(x => (x._1, Interval(x._2.get._1, x._2.get._2))).toMap
            val totalInterval = boundingValues.values reduce (_ /\ _)
            val totalProb = boundingValues.keys.foldLeft(Interval(0))(_ + _.p.get) \/ Interval(0, 1)
            // Probability for the value to be outside of total interval. The uncertainty for the low bound is maximum
            // because noone can tell what happen to the values which are not simulated.
            // The high bound is obviously 1 - min(P(being in totalInterval))
            val probaOutBounding = Interval(0) /\ (Interval(1) - totalProb)

            // Compute the probabilities distribution
            // Map: value bounds -> proba bounds
            var probaDist = Map.empty[Interval, Interval]
            var continue = true
            var graphs = List(TagsGraph(boundingValues))
            def tagsOutsideOfInterval(i: Interval) = tags filter (t => {
              val bounds = boundingValues(t)
              (bounds.hi le i.lo) || (bounds.lo ge i.hi)
            })
            def tagsInsideOfInterval(i: Interval) = tags filter (i contains boundingValues(_))
            while (continue) {
              val intervalsIn = (graphs flatMap (g => {
                if (!g.isEmpty) {
                  val i = g.getTags map (boundingValues(_)) reduce (_ /\ _)
                  Some(g.getTags -> i)
                }
                else None
              })).sortWith(_._2.lo le _._2.lo)
              // Extend the intervals to hide the little gaps between them
              var extendedIntervals =
                if(intervalsIn.tail.nonEmpty)
                  (intervalsIn zip intervalsIn.tail) map {case ((_, i1), (_, i2)) =>Interval(i1.lo, i2.lo)}
                else intervalsIn.unzip._2
              extendedIntervals = extendedIntervals.head /\ totalInterval.lo :: extendedIntervals.tail
              extendedIntervals = extendedIntervals.take(extendedIntervals.size - 1) ::: List (extendedIntervals.last /\ totalInterval.hi)

              probaDist = (extendedIntervals map (i => i ->
                tagsInsideOfInterval(i).foldLeft(Interval(0))(_ + _.p.get) /\
                (Interval(1) - tagsOutsideOfInterval(i).foldLeft(Interval(0))(_ + _.p.get)))).toMap
              // Refine the split by replacing the biggest graph by the result of its split
              // May be improved for other grouping strategies
              val toCut = graphs.indexOf(graphs.maxBy(_.size))
              if(graphs(toCut).size > 1) {
                val cuts = graphs(toCut).cut
                graphs = graphs.take(toCut) ::: graphs.drop(toCut + 1)
                if(!cuts._1.isEmpty) graphs = cuts._1 :: graphs
                if(!cuts._2.isEmpty) graphs = cuts._2 :: graphs
              } else
                continue = false
            }

            // CDF calculation
            // Get all the bounds, mark if it is a low or a high bound and attach the tag of the corresponding interval
            // If it is a low bound, add the probability to the high bound of the CDF,
            // if it is a high bound, add it to the Low bound
            val points = (boundingValues flatMap { case (t, i) =>
              // true for low bound, false for hi bound
              List((i.lo, t, true), (i.hi, t, false))
            }).toList.sortWith(_._1 le _._1)
            val cdfBounds = points.foldLeft(List.empty[(Interval.Real, Interval)]) { case (l, (x, t, low)) =>
              val p0 = if (l.nonEmpty) l.head._2 else probaOutBounding
              if (low) (x, Interval(p0.lo, (p0 + t.p.get).hi)) :: l
              else (x, Interval((p0 + t.p.get).lo, p0.hi)) :: l
            }.reverse
            // Build a list of enclosures from the cdfBounds at different times interval
            val cdfEnclosure =
            ((cdfBounds zip (cdfBounds unzip)._1.tail) map { case ((l, p), h) =>
              Interval(l, h) -> p
            }).toMap
            model.setProba(Some(new ProbaData(name, qtAndRows.find(_._2.nonEmpty).get._2.get._1, probaOutBounding, probaDist, cdfEnclosure)))
            probaEnabled = true
          }
        }
      } else if (e.peer.getButton == 2) {
        model.setProba(None)
        prbCursor = None
        probaEnabled = false
      }
      plotter ! Refresh
    case e@MousePressed(_,p,_,_,_) if (enabled) =>
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
    case e@MouseReleased(_,p,_,_,_) if (enabled) =>
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
      } else if (e.peer.getButton == 1 && selectionRectangle != null) {
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
    case UIElementResized(_) if (enabled) =>
      plotter ! Repaint
  }

  def zoomAround(cx:Double, cy:Double, factor:Double) =  if (enabled) {
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

  val tableI = new TableInput({() => App.ui.controller.model.getTraceModel})
    
  val plotter = new Plotter(tableI,plotI,App.ui.jPlotI)
  plotter.start()
  
  def toggleSimulator(b:Boolean) = {
    plotI.parms = plotI.parms.copy(plotSimulator = b)
    hoveredBox = None
    plotter ! Replot
  }
  def toggleNextChild(b:Boolean) = {
    plotI.parms = plotI.parms.copy(plotNextChild = b)
    hoveredBox = None
    plotter ! Replot
  }
  def toggleSeeds(b:Boolean) = {
    plotI.parms = plotI.parms.copy(plotSeeds = b)
    hoveredBox = None
    plotter ! Replot
  }
  def setPlotStyle(ps:PlotStyle) = {
    plotI.plotStyle = ps
    plotter ! Repaint(viewPort)
  }

  def render(file:File, w:Int, h:Int) = if (enabled) {
    // FIXME: This should't be done on the EDT...
    val buf = new BufferedImage(w, h, BufferedImage.TYPE_INT_ARGB)
    new PlotImage(pd, buf, plotI.plotStyle, viewPort)
    ImageIO.write(buf, "PNG", file)
  }
}

object PlotPanel {
  sealed abstract class State extends Event
  case object Disabled extends State
  case object Enabled extends State
}
