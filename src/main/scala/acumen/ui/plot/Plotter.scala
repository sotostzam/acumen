package acumen
package ui
package plot

import java.awt.geom.Rectangle2D
import java.awt.image.BufferedImage

import scala.actors._
import interpreter._

// Plotter actor primary resposable for plotting but also updating the trace table

abstract sealed class PlotterAction
case object Refresh extends PlotterAction // implies repaint
case object Replot extends PlotterAction // implies repaint
case class Repaint(viewPort : Rectangle2D) extends PlotterAction
object Repaint extends Repaint(null)

class TableInput(val model : () => TraceModel) 
{
  @volatile var enabled = false
}

class PlotInput(val model : () => PlotModel,
                val buffer : () => BufferedImage) 
{
  @volatile var enabled = true
  @volatile var parms = PlotParms()
  @volatile var plotStyle : PlotStyle = Lines()
}

abstract class JPlotInput {
  @volatile var enabled = false
  def newData(): Object;
  def addToPlot(d: Object): Unit;
}


case class TraceModelReady(model: TraceModel) 
     extends swing.event.Event

case class PlotReady(model: PlotModel,
                     data: PlotData,
                     image: PlotImage,
                     newPlot: Boolean) // as oppose to a new view on the existing plot
     extends swing.event.Event

class Plotter(tableI: TableInput, plotI: PlotInput, jPlotI: JPlotInput) 
  extends DaemonActor with scala.swing.Publisher 
{
  import Plotter._

  var lastMsg: PlotterAction = null

  def act() {
    Supervisor.watch(this, "Plotter", {restart})
    waitForMsg()
  }
  
  def waitForMsg() {
    App ! Ready
    react {
      case msg : PlotterAction => 
        mergeMsgs(msg)
      case msg => 
        println("Unknown Msg in Plotter:. " + msg)
    }
  }

  def mergeMsgs(msg: PlotterAction) {
    lastMsg match {
      case Refresh                      => lastMsg = Refresh
      case Replot if lastMsg != Refresh => lastMsg = Replot
      case null                         => lastMsg = msg
      case _                            =>
    }
    reactWithin (0) {
      case msg : PlotterAction => 
        println("SKIPPING UPDATE!")
        mergeMsgs(msg)
      case TIMEOUT => 
        App ! Busy
        msg match {
          case Refresh => refresh
          case Replot  => replot
          case Repaint(vp) => if (pm != null) repaint(vp)
        }
        lastMsg = null
        waitForMsg()
      case msg =>
        println("Unknown Msg in Plotter:: " + msg)
    }
  }

  def refresh {
    if (tableI.enabled) {
      val tm = tableI.model()
      App ! TraceModelReady(tm)
    }
    replot
    updateJPlot
  }

  var pm : PlotModel = null
  var pd : PlotData = null

  def replot = if (plotI.enabled) {
    pm = plotI.model()
    if (pm != null) {
      pd = new PlotData(plotI.parms,pm)
      repaint()
    }
  }

  def updateJPlot = if (jPlotI.enabled) {
    val data = jPlotI.newData()
    if (data != null) swing.Swing.onEDTWait {
      jPlotI.addToPlot(data)
    }
  }

  def repaint(vp: Rectangle2D = null) = {
    var buf = plotI.buffer()
    val pi = new PlotImage(pd, buf, plotI.plotStyle, vp)
    App ! PlotReady(pm,pd,pi,vp == null)
  }
}

object Plotter {
  import scala.swing.event.Event

  abstract sealed class State extends Event
  case object Ready extends State
  case object Busy  extends State
}
