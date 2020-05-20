package acumen
package ui
package plot

import java.awt.geom.Rectangle2D
import java.awt.image.BufferedImage

import scala.actors._
import interpreter._

// Plotter actor primary responsible for plotting but also updating the trace table

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
  @volatile var enabled = false
  @volatile var parms = PlotParms()
  @volatile var plotStyle : PlotStyle = Lines()
  val DISABLE_THRESHOLD = 25
  @volatile var disableThreshold = DISABLE_THRESHOLD
}

case class PlotReady(model: PlotModel,
                     data: PlotData,
                     image: PlotImage,
                     newPlot: Boolean) // as oppose to a new view on the existing plot
     extends swing.event.Event

class Plotter(tableI: TableInput, plotI: PlotInput)
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

  var updatesSkipped = 0

  def mergeMsgs(msg: PlotterAction) {
    lastMsg match {
      case Refresh                      => lastMsg = Refresh
      case Replot if lastMsg != Refresh => lastMsg = Replot
      case null                         => lastMsg = msg
      case _                            =>
    }
    reactWithin (0) {
      case msg : PlotterAction => 
        updatesSkipped += 1
        //println("SKIPPING UPDATE! : #" + updatesSkipped)
        mergeMsgs(msg)
      case TIMEOUT => 
        App ! Busy
        msg match {
          case Refresh => refresh
          case Replot  => replot
          case Repaint(vp) => if (pm != null) repaint(vp)
        }
        lastMsg = null
        updatesSkipped = 0
        waitForMsg()
      case msg =>
        println("Unknown Msg in Plotter:: " + msg)
    }
  }

  def refresh {
    if (App.ui.controller.model != null)
      App.ui.controller.model.flush
    if (tableI.enabled) {
      val tm = tableI.model()
      App.ui.serializeTable(tm)
    }
    replot
  }

  var pm : PlotModel = null
  var pd : PlotData = null

  def replot = if (plotI.enabled) {
    //val startTime = System.currentTimeMillis( )
    pm = plotI.model()
    pd = new PlotData(plotI.parms,pm,plotI.disableThreshold)
    repaint()
    //val endTime = System.currentTimeMillis( )
    //println("Plot time: " + (endTime-startTime) + "ms" + "  " + (endTime-startTime+0.0)/pd.time.size)
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
