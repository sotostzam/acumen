package acumen
package ui
package plot

import java.awt.{BasicStroke, Color}
import scala.collection.JavaConversions._
import scala.collection.mutable.{Buffer, Map,HashMap,HashSet,ArrayBuffer}
import org.jfree.chart._
import org.jfree.chart.plot._
import org.jfree.chart.renderer.xy.XYDifferenceRenderer
import org.jfree.data.xy._
import org.jfree.ui.ApplicationFrame

import acumen.interpreters.enclosure._
import acumen.ui.interpreter.TraceData
import acumen.util.Canonical._
import acumen.util.Conversions._

class CStorePlotter extends JFreePlotter {

  private var ids = new HashSet[CId]
  private var indexes = new HashMap[(CId,Name,Option[Int]),Int]

  def renderer(color: Color) = {
    val ren = new org.jfree.chart.renderer.xy.XYLineAndShapeRenderer(true,false)
    ren.setPaint(color)
    ren
  }

  def addToPlot(d: Object) = {
    //combinedPlot.setNotify(false)
    val ds = d.asInstanceOf[Array[interpreter.TraceData]]
    for (sts <- ds)
      addDataHelper(sts)
    //combinedPlot.setNotify(true)
    for (ds <- dataSets) {
      ds.fireSeriesChanged()
    }
    //for (p <- subPlotsList) {
    //  p.notifyListeners(new org.jfree.chart.event.PlotChangeEvent(p))
    //}
    //combinedPlot.notifyListeners(new org.jfree.chart.event.PlotChangeEvent(combinedPlot))
  }

  private def plotit(v: CValue, cn: ClassName, fn: Name) = v match {
    case VLit(GDouble(_) | GInt(_)) =>
      cn != cmagic && 
        (fn != Name("nextChild",0)) && 
        (fn != Name("seed1",0) && fn !=  Name("seed2",0))
    case _ => false
  }
  
  val dataSets = ArrayBuffer[XYSeries]()
  var disabled = true

  private def addSubPlot(legendLabel: String) = {
    if (!disabled && subPlotsList.size >= 24) {
      super.resetPlot
      dataSets.clear
      disabled = true
      for (key <- indexes.keys)
        indexes(key) = -1
      App.ui.console.log("Too Many Subplots!  Disable New Plot Tab...")
      -1
    } else if (!disabled) {
      val p = initXYPlot(legendLabel)
      //p.setNotify(false)
      val s = new XYSeries(legendLabel,false,true)
      //s.setNotify(false)
      dataSets += s
      val sc = new XYSeriesCollection(s)
      p.setDataset(sc)
      p.setRenderer(renderer(Color.red))
      combinedPlot.add(p, 1)
      subPlotsList += p
      subPlotsList.size - 1
    } else {
      -1
    }
  }

  private def addDataHelper(sts:interpreter.TraceData) = {
    def compIds(ido1:(CId,_), ido2:(CId,_)) = ido1._1 < ido2._1
    def compFields(p1:(Name,CValue),p2:(Name,CValue)) = 
      Ordering[(String,Int)] lt ((p1._1.x, p1._1.primes),(p2._1.x, p2._1.primes))
    // First dig out the time value
    for (st <- sts) {
      // first dig out the time value
      var time = 0.0
      for ((id,o) <- st.asInstanceOf[CStore].toList) {
        for ((x,v) <- o.toList) 
          if (x == Name("time",0) && classOf(o) == cmagic) {
            time = extractDouble(v)
          }
      }
      // now we can plot the rest
      for ((id,o) <- st.asInstanceOf[CStore].toList sortWith(compIds)) {
        if (!ids.contains(id)) {
          for ((x,v) <- o.toList sortWith(compFields)) {
            v match {
              case VVector(u) =>
                for ((ui,i) <- u zipWithIndex) {
                  if (plotit(ui, classOf(o), x)) {
                    val idx = addSubPlot("(#" + id + " : " + classOf(o).x + ")." + x.x + "'" * x.primes + "["+i+"]")
                    indexes += (((id,x,Some(i)), idx))
                  } else {
                    indexes += (((id,x,Some(i)), -1))
                  }
                }
              case _ =>
                if (plotit(v, classOf(o), x)) {
                  val idx = addSubPlot("(#" + id + " : " + classOf(o).x + ")." + x.x + "'" * x.primes)
                  indexes += (((id,x,None), idx))
                } else {
                  indexes += (((id,x,None), -1))
                }
            } 
          }
          ids += id
        }
        for ((x,v) <- o.toList) addVal(id,x,time,v)
      }
    }
  }

  private def addVal(id:CId, x:Name, t:Double, v:CValue) = v match {
    case VVector(u) =>
      for ((ui,i) <- u zipWithIndex) {
        val idx = indexes((id,x,Some(i)))
        if (idx != -1)
          addPoint(idx, t, extractDoubleNoThrow(ui))
      }
    case _ =>
      val idx = indexes((id,x,None))
      if (idx != -1) 
        addPoint(idx, t, extractDoubleNoThrow(v))
  }

  private def addPoint(idx: Int, t: Double, v: Double) {
    dataSets(idx).add(t, v, false)
  }

  override def resetPlot = {
    super.resetPlot
    ids.clear
    indexes.clear
    dataSets.clear
    disabled = false
  }

}

