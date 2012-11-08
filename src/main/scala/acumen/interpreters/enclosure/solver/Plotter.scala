package acumen.interpreters.enclosure.solver

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

abstract trait Plotter {
  
  val chartPanels: Map[String,(ChartPanel,Int)] = Map[String,(ChartPanel,Int)]()

  def createFrame(frametitle: String) = {
    val frame = new ApplicationFrame(frametitle)
    frame.getContentPane.setLayout(new BoxLayout(frame.getContentPane,BoxLayout.Y_AXIS))
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
    legendLabel: String): TableXYDataset = {
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
    //    for {
    //      x <- (0 to Math.floor((intervalEnd - intervalStart) / stepSize).toInt)
    //        .map((p) => intervalStart + (p * stepSize))
    //    } {
    //      lower.add(x, lowerApproximation(x));
    //      upper.add(x, upperApproximation(x))
    //    }
    val res: DefaultTableXYDataset = new DefaultTableXYDataset()
    res.addSeries(lower)
    res.addSeries(upper)
    res.setIntervalWidth(0.01)
    res
  }

  def addColoredFunctionEnclosure(
    intervalStart: Double,
    intervalEnd: Double,
    upperApproximation: Double => Double,
    lowerApproximation: Double => Double,
    //    stepSize: Double,
    extraSamples: Int,
    legendLabel: String,
    color:Color,
    frame:ApplicationFrame) {
    addEnclosure(intervalStart, intervalEnd, upperApproximation, lowerApproximation, extraSamples, //stepSize,
      color, legendLabel, frame)
  }
  
  def addFunctionEnclosure(
    intervalStart: Double,
    intervalEnd: Double,
    upperApproximation: Double => Double,
    lowerApproximation: Double => Double,
    //    stepSize: Double,
    extraSamples: Int,
    legendLabel: String,
    frame:ApplicationFrame) {
    addEnclosure(intervalStart, intervalEnd, upperApproximation, lowerApproximation, extraSamples, //stepSize,
      Color.blue, legendLabel, frame)
  }

  def addDerivativeEnclosure(
    intervalStart: Double,
    intervalEnd: Double,
    upperApproximation: Double => Double,
    lowerApproximation: Double => Double,
    //    stepSize: Double,
    extraSamples: Int,
    legendLabel: String,
    frame:ApplicationFrame) {
    addEnclosure(intervalStart, intervalEnd, upperApproximation, lowerApproximation, extraSamples, //stepSize,
      new Color(0, 127, 0), legendLabel, frame)
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
    frame:ApplicationFrame) {

    val (chartPanel: ChartPanel, numberOfDatasets: Int) = chartPanels.get(legendLabel) match {
       case None => {
        val chart: JFreeChart = createChart("", "Time", legendLabel)
        chart.getPlot().setBackgroundPaint(Color.white)
        chart.getXYPlot().setDomainGridlinesVisible(true)
        chart.getXYPlot().setRangeGridlinesVisible(true)
        chart.getXYPlot().setRangeGridlinePaint(Color.gray)
        chart.getXYPlot().setDomainGridlinePaint(Color.gray)
        val chartPanel: ChartPanel = new ChartPanel(chart);
        frame.getContentPane.add(chartPanel);
        (chartPanel,0)
      }
      case Some(t) => t
    }
    val chart = chartPanel.getChart
    
    val e = createEnclosureDataset(intervalStart, intervalEnd, upperApproximation, lowerApproximation, extraSamples, //stepSize,
      legendLabel)

    val semiTransparentColor = new Color(color.getRed(), color.getGreen(), color.getBlue(), 20) //127)
    val ren = new XYDifferenceRenderer(Color.red, semiTransparentColor, false)

    ren.setStroke(new BasicStroke(1.0f))
    ren.setSeriesPaint(0, color)
    ren.setSeriesPaint(1, color)

    val plot: XYPlot = chart.getXYPlot()
    plot.setDataset(numberOfDatasets, e)
    plot.setRenderer(numberOfDatasets, ren)
    chart.removeLegend
    frame.invalidate
    chartPanels(legendLabel) = (chartPanel,numberOfDatasets + 1)
  }

  def createChart(title: String, xlabel: String, ylabel: String): JFreeChart = {
    val c = ChartFactory.createStackedXYAreaChart(
      title,
      xlabel,
      ylabel,
      null,
      PlotOrientation.VERTICAL,
      true, // legend
      true, // tool tips
      false // URLs
      )
    c
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

}