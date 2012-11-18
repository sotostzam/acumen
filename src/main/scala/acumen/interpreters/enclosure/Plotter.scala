package acumen.interpreters.enclosure

import java.awt.geom.Rectangle2D
import java.awt.BasicStroke
import java.awt.Color
import java.io.FileOutputStream
import scala.swing._
import org.jfree.chart.plot._
import org.jfree.chart.renderer.xy.XYDifferenceRenderer
import org.jfree.chart.title.LegendTitle
import org.jfree.chart._
import org.jfree.data.xy._
import org.jfree.ui.ApplicationFrame
import org.jfree.ui.RectangleEdge
import com.itextpdf.awt.DefaultFontMapper
import com.itextpdf.text.pdf.PdfContentByte
import com.itextpdf.text.pdf.PdfTemplate
import com.itextpdf.text.pdf.PdfWriter
import com.itextpdf.text.Document
import javax.swing.JFrame
import javax.swing.BoxLayout
import scala.collection.mutable.Map
import javax.swing.event.ChangeListener
import org.jfree.chart.event.ChartChangeListener
import org.jfree.chart.event.ChartChangeEvent
import javax.swing.JMenuItem
import java.awt.event.ActionListener
import java.awt.event.ActionEvent
import acumen.ui.plot.SaveAsDailog
import org.jfree.chart.axis.NumberAxis
import acumen.interpreters.enclosure._

class Plotter {

  var combinedPlot : CombinedDomainXYPlot = null
  var combinedPlotAll = combinedPlot
  var subPlots: Map[String, (XYPlot, Int)] = Map[String, (XYPlot, Int)]()
  val enclosureRen = enclosureRenderer(Color.red)
  var chart : JFreeChart = null
  var chartPanel : ChartPanel = null
  var popupLocation = new java.awt.Point(0,0)

  def initPlot = {
    val saveAsPdf = new JMenuItem("Save as PDF")
    val hideOther = new JMenuItem("Hide Other Plots")
    val hideThis = new JMenuItem("Hide This Plot")
    val showAll = new JMenuItem("Show All Plots")
    val mergeVisible = new JMenuItem("Merge Visible Plots")
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

    saveAsPdf.addActionListener(new ActionListener() {
      def actionPerformed(event: ActionEvent) {
        val d = new SaveAsDailog(Component.wrap(chartPanel), chart)
        d.pack
        d.open
      }
    })
    val popupMenu = chartPanel.getPopupMenu
    popupMenu.addSeparator
    popupMenu.add(saveAsPdf)
    hideOther.addActionListener(new ActionListener() {
      def actionPerformed(event: ActionEvent) {
        if (combinedPlotAll == combinedPlot)
          combinedPlotAll = combinedPlot.clone.asInstanceOf[CombinedDomainXYPlot]
        import scala.collection.JavaConversions._
        val oldPlots = combinedPlot.getSubplots.toArray
        for (p <- oldPlots) {
          if (p != curPlot)
            combinedPlot.remove(p.asInstanceOf[XYPlot])
        }
      }
    })
    hideThis.addActionListener(new ActionListener() {
      def actionPerformed(event: ActionEvent) {
        if (combinedPlotAll == combinedPlot)
          combinedPlotAll = combinedPlot.clone.asInstanceOf[CombinedDomainXYPlot]
        import scala.collection.JavaConversions._
        val oldPlots = combinedPlot.getSubplots.toArray
        for (p <- oldPlots) {
          if (p == curPlot)
            combinedPlot.remove(p.asInstanceOf[XYPlot])
        }
      }
    })
    showAll.addActionListener(new ActionListener() {
      def actionPerformed(event: ActionEvent) {
        resetPlotView(combinedPlotAll)
      }
    })
    mergeVisible.addActionListener(new ActionListener() {
      def actionPerformed(event: ActionEvent) {
        if (combinedPlotAll == combinedPlot)
          combinedPlotAll = combinedPlot.clone.asInstanceOf[CombinedDomainXYPlot]
        val mergedPlot = initXYPlot("")
        var dataSetIndex = 0
        var pi = 0
        val varNames = Seq[String]()
        val sps = combinedPlot.getSubplots.toArray
        for (p <- sps) {
          val xyp = p.asInstanceOf[XYPlot]
          val c = xyp.getDatasetCount - 1
          val ren = enclosureRenderer(Color.getHSBColor((1.0f*pi)/(1.0f*sps.size), 1.0f, 0.7f))
          for (i <- 0 until c) {
            val dataSet = xyp getDataset i
            mergedPlot setDataset (dataSetIndex, dataSet)
            mergedPlot setRenderer (dataSetIndex, ren)
            dataSetIndex += 1
          }
          pi += 1
          val newPlot = newCombinedPlot
          newPlot add mergedPlot
          resetPlotView(newPlot)
        }
      }
    })
    popupMenu.addSeparator
    popupMenu.add(hideOther)
    popupMenu.add(hideThis)
    popupMenu.add(showAll)
    popupMenu.add(mergeVisible)
    chartPanel.setBackground(Color.white)
    resetPlot
  }
  
  def newCombinedPlot = new CombinedDomainXYPlot(new NumberAxis("Time"))

  def resetPlotView(pl: CombinedDomainXYPlot) = { 
    combinedPlot = pl
    chart = new JFreeChart("", JFreeChart.DEFAULT_TITLE_FONT, pl, false)
    chartPanel.setChart(chart)
  }
  
  def resetPlot = {
    resetPlotView(newCombinedPlot)
    combinedPlotAll = combinedPlot
    subPlots.clear
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

  def createEnclosureDataset(
    intervalStart: Double,
    intervalEnd: Double,
    upperApproximation: Double => Double,
    lowerApproximation: Double => Double,
    //    stepSize: Double,
    extraSamples: Int,
    legendLabel: String,
    fun: Double => Double): XYSeriesCollection = {
    val lower: XYSeries = new XYSeries(legendLabel, false, false)
    val upper: XYSeries = new XYSeries("HIDE_ME", false, false)
    lower.add(intervalStart, lowerApproximation(intervalStart));
    upper.add(intervalStart, upperApproximation(intervalStart))
    for (i <- 1 to extraSamples) {
      val x = (intervalStart + intervalEnd) * (i.doubleValue) / (extraSamples + 1)
      lower.add(x, lowerApproximation(x))
      upper.add(x, upperApproximation(x))
    }
    lower.add(intervalEnd, lowerApproximation(intervalEnd));
    upper.add(intervalEnd, upperApproximation(intervalEnd))
    val res = new XYSeriesCollection()
    res.addSeries(lower)
    res.addSeries(upper)
    res.setIntervalWidth(0.01)
    res
  }

  def addEnclosure(
    intervalStart: Double,
    intervalEnd: Double,
    upperApproximation: Double => Double,
    lowerApproximation: Double => Double,
    //    stepSize: Double,
    extraSamples: Int,
    color: Color,
    legendLabel: String,
    fun: Double => Double) {
	
    val (subPlot, numberOfDatasets) = subPlots.get(legendLabel) match {
      case Some(t) => t
      case None => {
        val p = initXYPlot(legendLabel)
        combinedPlot.add(p, 1)
        (p, 0)
      }
    }
        
    val e = createEnclosureDataset(intervalStart, intervalEnd, upperApproximation, lowerApproximation, extraSamples, //stepSize,
      legendLabel, fun)

    subPlot.setDataset(numberOfDatasets, e)
    subPlot.setRenderer(numberOfDatasets, enclosureRen)
    subPlots(legendLabel) = (subPlot, numberOfDatasets + 1)
    
    chartPanel.invalidate
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

  //TODO Make this visually merge overlapping enclosures into a single area with one outline.
  def enclosureRenderer(color: Color) = {
    val semiTransparentColor = new Color(color.getRed(), color.getGreen(), color.getBlue(), 20)
    val ren = new XYDifferenceRenderer(Color.red, semiTransparentColor, false)
    ren.setStroke(new BasicStroke(1.0f))
    ren.setSeriesPaint(0, color)
    ren.setSeriesPaint(1, color)
    ren
  }

  def convertToPDF(chart: JFreeChart, width: Int, height: Int, filename: String) {
    val document: Document = new Document(new com.itextpdf.text.Rectangle(width, height))
    try {
      val writer: PdfWriter = PdfWriter.getInstance(document, new FileOutputStream(filename))
      document.open
      val cb: PdfContentByte = writer.getDirectContent
      val tp: PdfTemplate = cb.createTemplate(width, height)
      val g2d: Graphics2D = tp.createGraphics(width, height, new DefaultFontMapper)
      val r2d: Rectangle2D = new Rectangle2D.Double(0, 0, width, height)
      chart.draw(g2d, r2d)
      g2d.dispose
      cb.addTemplate(tp, 0, 0)
    } catch {
      case e: Exception => e.printStackTrace
    } finally {
      document.close()
    }
  }

  // FIXME?: Maybe move these two methods into a
  // UnivariateAffineEnclosurePlotter to keep plotter from depending
  // on UnivariateAffineEnclosure, than again, maybe its not worth it.
  // -- kevina

  def plotUAE(e: UnivariateAffineEnclosure, fun: Double => Double)(implicit rnd: Rounding) = {
    val color = Color.red
    for ((varName, it) <- e.components) {
      def low(t: Double) = it.low(t) match { case Interval(lo, _) => lo.doubleValue }
      def high(t: Double) = it.high(t) match { case Interval(_, hi) => hi.doubleValue }
      val dom = it.domain
      val (lo, hi) = dom match { case Interval(l, h) => (l.doubleValue, h.doubleValue) }
      addEnclosure(lo, hi, high, low, 0, color, varName, fun)
    }
  }

  def plot(es: Seq[UnivariateAffineEnclosure], fun: Double => Double)(implicit rnd: Rounding) : Unit = {
    combinedPlot.setNotify(false)
    for (e <- es) plotUAE(e, fun)
    combinedPlot.setNotify(true)
  }

  // Plot into a new frame
  def plot(frametitle: String)(fun: Double => Double)(es: Seq[UnivariateAffineEnclosure])(implicit rnd: Rounding) : Unit = {
    val frame = createFrame(frametitle)
    plot(es, fun)
  }


}
