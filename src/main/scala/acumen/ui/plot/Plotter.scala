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

class PlotInput(val model : () => PlotModel,
                val buffer : () => BufferedImage) 
{
  @volatile var enabled = true
  @volatile var parms = PlotParms()
  @volatile var plotStyle : PlotStyle = Lines()
}

case class TraceModelReady(model: TraceModel) 
     extends swing.event.Event

case class PlotReady(model: PlotModel,
                     data: PlotData,
                     image: PlotImage,
                     newPlot: Boolean) // as oppose to a new view on the existing plot
     extends swing.event.Event

class PlotActor(tableI: TableInput, plotI: PlotInput) 
  extends DaemonActor with scala.swing.Publisher 
{
  var lastMsg: PlotterAction = null

  def act() {
    Supervisor.watch(this, "Plotter")
    waitForMsg()
  }
  
  def waitForMsg() {
    Acumen.actor ! PlotStateChanged(Ready)
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
        println("SKIPPING PLOT!")
        mergeMsgs(msg)
      case TIMEOUT => 
        Acumen.actor ! PlotStateChanged(Busy)
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
    val tm = tableI.model()
    Acumen.ui.actor ! TraceModelReady(tm)
    replot
  }

  var pm : PlotModel = null
  var pd : PlotData = null

  def replot = if (plotI.enabled) {
    pm = plotI.model()
    println("pm = " + pm)
    pd = new PlotData(plotI.parms,pm)
    repaint()
  }

  def repaint(vp: Rectangle2D = null) = {
    var buf = plotI.buffer()
    val pi = new PlotImage(pd, buf, plotI.plotStyle, vp)
    Acumen.ui.actor ! PlotReady(pm,pd,pi,vp == null)
  }
}

