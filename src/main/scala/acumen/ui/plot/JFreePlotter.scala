package acumen
package ui
package plot

import java.awt.Color
import java.awt.event.{ActionEvent, ActionListener}

import javax.swing.{BoxLayout, JFrame, JMenuItem}

import scala.Array.canBuildFrom
import scala.collection.JavaConversions.asScalaIterator
import scala.collection.mutable.{Buffer, Map}
import scala.swing.Component

import org.jfree.chart.{ChartPanel, JFreeChart, LegendItem, LegendItemCollection, LegendItemSource}
import org.jfree.chart.axis.NumberAxis
import org.jfree.chart.plot.{CombinedDomainXYPlot, XYPlot}
import org.jfree.chart.renderer.xy.XYItemRenderer
import org.jfree.chart.title.LegendTitle
import org.jfree.ui.{ApplicationFrame, RectangleEdge}

abstract class JFreePlotter {

  var combinedPlot : CombinedDomainXYPlot = null
  var subPlots = Map[String, (XYPlot, Int)]()
  var subPlotsList = Buffer[XYPlot]()
  var chart : JFreeChart = null
  var chartPanel : ChartPanel = null
  //var popupLocation = new java.awt.Point(0,0)
  @volatile var attached = false

  def initPlot = {
    val saveAsPdf = new JMenuItem("Save as PDF") // NRL: Non-BSD Dep.
    val hideOther = new JMenuItem("Hide Other Plots")
    val hideThis = new JMenuItem("Hide This Plot")
    val mergeVisible = new JMenuItem("Merge Visible Plots")
    val resetView = new JMenuItem("Reset View")
    var curPlot : XYPlot = null
    chartPanel = new ChartPanel(null, true, true, true, true, false) {
      override def displayPopupMenu(x: Int, y: Int) {
        curPlot = combinedPlot.findSubplot(chartPanel.getChartRenderingInfo().getPlotInfo(), 
                                           new java.awt.Point(x,y))
        val enableShowHide = curPlot != null
        hideOther.setEnabled(enableShowHide)
        hideThis.setEnabled(enableShowHide)
        mergeVisible.setEnabled(combinedPlot.getSubplots.size > 1)
        super.displayPopupMenu(x,y)
      }
    }
    saveAsPdf.addActionListener(new ActionListener() {               // NRL: Non-BSD Dep.
      def actionPerformed(event: ActionEvent) {                      // NRL
        val d = new SaveAsDailog(Component.wrap(chartPanel), chart)  // NRL
        d.pack                                                       // NRL
        d.open                                                       // NRL
      }                                                              // NRL  
    })                                                               // NRL
    val popupMenu = chartPanel.getPopupMenu
    popupMenu.addSeparator
    popupMenu.add(saveAsPdf) // NRL: Non-BSD Dep.
    hideOther.addActionListener(new ActionListener() {
      def actionPerformed(event: ActionEvent) {
        val oldPlots = combinedPlot.getSubplots.toArray
        for (p <- oldPlots) {
          if (p != curPlot)
            combinedPlot.remove(p.asInstanceOf[XYPlot])
        }
      }
    })
    hideThis.addActionListener(new ActionListener() {
      def actionPerformed(event: ActionEvent) {
        import scala.collection.JavaConversions._
        val oldPlots = combinedPlot.getSubplots.toArray
        for (p <- oldPlots) {
          if (p == curPlot)
            combinedPlot.remove(p.asInstanceOf[XYPlot])
        }
      }
    })
    // Merge the currently visible plots
    mergeVisible.addActionListener(new ActionListener() {
      def actionPerformed(event: ActionEvent) = mergeVisablePlots
    })
    resetView.addActionListener(new ActionListener() {
      def actionPerformed(event: ActionEvent) {
        combinedPlot = newCombinedPlot
        for (p <- subPlotsList)
          combinedPlot.add(p,1)
        resetChart(combinedPlot)
        chartPanel.restoreAutoBounds()
      }
    })
    popupMenu.addSeparator
    popupMenu.add(hideOther)
    popupMenu.add(hideThis)
    popupMenu.addSeparator
    popupMenu.add(mergeVisible)
    popupMenu.addSeparator
    popupMenu.add(resetView)
    chartPanel.setBackground(Color.white)
    resetPlot
  }

  def mergeVisablePlots = {
    val mergedPlot = initXYPlot("") //TODO Externalize string, make settable from CLI
    var dataSetIndex = 0
    val sps = combinedPlot.getSubplots.toArray
    for ((p,pi) <- sps zip Stream.from(0)) {
      val xyp = p.asInstanceOf[XYPlot]
      val ren = renderer( // Make a unique color for this enclosure
        Color.getHSBColor(pi/(1.0f*sps.size), 1.0f, 0.7f))
      for (i <- 0 until xyp.getDatasetCount) {
        mergedPlot.setDataset(dataSetIndex, xyp.getDataset(i))
        mergedPlot.setRenderer(dataSetIndex, ren)
        dataSetIndex += 1
      }
      val newPlot = newCombinedPlot
      newPlot.add(mergedPlot)
      resetChart(newPlot)
      chart.addLegend(createLegend(mergedPlot))
    }
  }
  
  /** Add a legend, making sure there are no duplicate legend items. */
  def createLegend(plot: XYPlot) = {
    val legendItemsOld = plot.getLegendItems
    val legendItemsNew = new LegendItemCollection()
    for (i <- 0 until legendItemsOld.getItemCount) {
      val li = legendItemsOld.get(i)
      val varName = li.getLabel
      val legendItems = for {x <- legendItemsNew.iterator} yield x
      val shouldAdd = varName != "HIDE_ME" && !(legendItems exists (_.asInstanceOf[LegendItem].getLabel == varName))
      if (shouldAdd)
        legendItemsNew.add(li)
    }
    val lt = new LegendTitle(new LegendItemSource() {
      val lic = new LegendItemCollection
      lic.addAll(legendItemsNew)
      def getLegendItems = lic
    })
    lt.setPosition(RectangleEdge.TOP)
    lt
  }
  
  def newCombinedPlot = new CombinedDomainXYPlot(new NumberAxis("Time"))

  def resetChart(pl: CombinedDomainXYPlot) = { 
    detachChart
    combinedPlot = pl
    chart = new JFreeChart("", JFreeChart.DEFAULT_TITLE_FONT, pl, false)
  }

  // FIXME (if its worth it): If setChart throws we will be in an
  //   inconsistent state - kevina
  def detachChart = {
    attached = false
    chartPanel.setChart(null)
  }

  def attachChart = {
    if (!App.ui.jPlotI.enabled) 
      throw new RuntimeException("attaching when disabled!")
    attached = true
    chartPanel.setChart(chart)
  }
  
  def resetPlot = {
    resetChart(newCombinedPlot)
    subPlots.clear
    subPlotsList.clear
  }
  
  def createFrame(frametitle: String) = {
    initPlot
    val frame = new ApplicationFrame(frametitle)
    frame.getContentPane.setLayout(new BoxLayout(frame.getContentPane,BoxLayout.Y_AXIS))
    frame.setContentPane(chartPanel)
    frame.setBounds(0, 0, 600, 400)
    frame.setVisible(true)
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    frame
  }

  def initXYPlot(legendLabel: String) = {
    val p = new XYPlot()
    p.setDomainGridlinesVisible(true)
    p.setRangeGridlinesVisible(true)
    p.setRangeGridlinePaint(Color.gray)
    p.setDomainGridlinePaint(Color.gray)
    p.setRangeAxis(0, new NumberAxis(legendLabel))
    p
  }

  def renderer(color: Color) : XYItemRenderer

  def addToPlot(d: Object) : Unit

  def convertToPDF(width: Int, height: Int, filename: String) { // NRL: Non BSD. Dep
    ToPDF.JFreeChartToPDF(chart, width, height, filename)   // NRL
  }                                                             // NRL
}



