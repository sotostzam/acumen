package acumen
package ui
package plot

import java.awt.{BasicStroke, Color}
import scala.collection.JavaConversions._
import scala.collection.mutable.{Buffer,Map,HashMap,HashSet,ArrayBuffer}
import org.jfree.chart._
import org.jfree.chart.plot._
import org.jfree.chart.renderer.xy.XYDifferenceRenderer
import org.jfree.data.xy._
import org.jfree.ui.ApplicationFrame
import swing.Swing

import acumen.interpreters.enclosure._
import acumen.ui.interpreter.{PlotParms, Plottable,PlotDiscrete,PlotDoubles,PlotEnclosure,PlotModel}
import acumen.util.Canonical._
import acumen.util.Conversions._
import Errors._

case object TooManySubplots extends Exception

class CStorePlotter extends JFreePlotter {

  def renderer(color: Color) = {
    val ren = new org.jfree.chart.renderer.xy.XYLineAndShapeRenderer(true,false)
    ren.setPaint(color)
    ren
  }

  def addToPlot(d: Object) = try {
    //combinedPlot.setNotify(false)
    println("Adding data to plot")
    val model = d.asInstanceOf[PlotModel]
    try {
      for (toPlot <- model.getPlottables(PlotParms())) {
        addDataHelper(model, toPlot)
      }
      //combinedPlot.setNotify(true)
      for (ds <- dataSets.values) {
        ds.fireSeriesChanged()
      }
    } finally {
      lastFrame = model.getRowCount
    }
    //for (p <- subPlotsList) {
    //  p.notifyListeners(new org.jfree.chart.event.PlotChangeEvent(p))
    //}
    //combinedPlot.notifyListeners(new org.jfree.chart.event.PlotChangeEvent(combinedPlot))
  } catch {
    case TooManySubplots =>
  }

  val dataSets = new HashMap[Int,XYSeries]

  private def newSubPlot(legendLabel: String, idx: Int) = { // FIXME: rename
    if (!App.ui.jPlotI.forsedDisable && subPlotsList.size > 24) {
      println("Too Many Subplots, Disabling!")
      App.ui.jPlotI.enabled = false
      App.ui.jPlotI.tooSlow = true
      App.ui.jPlotI.forsedDisable = true
      App.ui.newPlotView.fixPlotState
      //App.ui.console.log("Too Many Subplots!  Disable New Plot Tab...")
      throw TooManySubplots
    }
    val p = initXYPlot(legendLabel)
    val s = new XYSeries(legendLabel,false,true)
    val sc = new XYSeriesCollection(s)
    p.setDataset(sc)
    p.setRenderer(renderer(Color.red))
    combinedPlot.add(p, 1)
    subPlotsList += p
    s
  }
  
  var lastFrame = 0
  val stringValues = new HashMap[String, Int]

  private def addDataHelper(model: PlotModel, toPlot: Plottable) = {
    val times = model.getTimes
    val series = dataSets.getOrElseUpdate(toPlot.column, 
                                          newSubPlot(model.getPlotTitle(toPlot.column), toPlot.column))
    
    val offset = toPlot.startFrame
    series.setNotify(false)

    toPlot match {
      case tP: PlotDiscrete =>
         for (i <- lastFrame until tP.values.size) {
          tP.values(i) match {
            case VLit(GStr(str)) => 
              series.add(times(offset + i),stringValues.getOrElseUpdate(str, - stringValues.size),true)
            case VLit(e:GDiscreteEnclosure[String]) => 
              throw NewPlotEnclosureError()
            case VLit(GInt(n)) => 
              series.add(times(offset + i),stringValues.getOrElseUpdate(n.toString, - stringValues.size),true)
          }
        }
      case tP: PlotDoubles =>
        for (i <- lastFrame until tP.values.size)
          series.add(times(offset + i),tP.values(i),true)
      case tP: PlotEnclosure =>
        throw NewPlotEnclosureError()
    }
    
    series.setNotify(true)
  }

  override def resetPlot = {
    super.resetPlot
    dataSets.clear
    lastFrame = 0
  }

}

