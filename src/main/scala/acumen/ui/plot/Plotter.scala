package acumen
package ui
package plot

import java.awt.geom.Rectangle2D
import java.awt.image.BufferedImage

import swing.Swing
import scala.actors._
import interpreter._

// Plotter actor primary resposable for plotting but also updating the trace table

abstract sealed case class PlotterAction
case object Refresh extends PlotterAction // implies repaint
case object Replot extends PlotterAction // implies repaint
case class Repaint(viewPort : Rectangle2D) extends PlotterAction
object Repaint extends Repaint(null)

class PlotActor(ppn: PlotPanel) extends DaemonActor {

  var lastMsg: PlotterAction = null

  def act() {
    Supervisor.watch(this, "Plotter")
    waitForMsg()
  }
  
  def waitForMsg() {
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

  var pnum = 1
  def refresh {
    val tm = Acumen.ui.controller.model.getTraceModel
    Swing.onEDTWait {
      Acumen.ui.traceTable.model = tm
      tm.fireTableStructureChanged()
    }
    replot
  }

  var pm : PlotModel = null
  var pd : PlotData = null

  def replot {
    if (Acumen.ui.traceView.check.selected) {
      println("=== NEW PLOT # " + pnum + "!!!")
      pm = Acumen.ui.controller.model.getPlotModel
      pd = new PlotData(ppn.pp,pm)
      repaint()
    }
  }

  def repaint(vp: Rectangle2D = null) {
    var buf : BufferedImage = null
    Swing.onEDTWait {
      buf = ppn.mkBuffer
    }
    val pi = new PlotImage(pd, buf, ppn.plotStyle, vp)
    //javax.imageio.ImageIO.write(buf, "PNG", new java.io.File("plot-" + pnum + ".png"))
    Swing.onEDTWait {
      println("Starting plot # " + pnum)
      ppn.model = pm
      ppn.pd = pd
      ppn.pi = pi
      if (vp == null)
        ppn.resetViewPort(pi.viewPort)
      ppn.repaint
    }
    pnum += 1
  }
}

