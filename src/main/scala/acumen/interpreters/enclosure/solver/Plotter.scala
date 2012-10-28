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

trait Plotter {
  val chart: JFreeChart = createChart("", "", "");

  var numberOfDatasets = 0;

  def createFrame(frametitle: String) {
    chart.getPlot().setBackgroundPaint(Color.white)
    chart.getXYPlot().setDomainGridlinesVisible(true)
    chart.getXYPlot().setRangeGridlinesVisible(true)
    chart.getXYPlot().setRangeGridlinePaint(Color.gray)
    chart.getXYPlot().setDomainGridlinePaint(Color.gray)
    val frame = new ApplicationFrame(frametitle)
    val chartPanel: ChartPanel = new ChartPanel(chart);
    chartPanel.setPreferredSize(new java.awt.Dimension(500, 270));
    frame.setContentPane(chartPanel);
    frame.setBounds(0, 0, 600, 400)
    frame.setVisible(true)
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
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
    color:Color) {
    addEnclosure(intervalStart, intervalEnd, upperApproximation, lowerApproximation, extraSamples, //stepSize,
      color, legendLabel)
  }
  
  def addFunctionEnclosure(
    intervalStart: Double,
    intervalEnd: Double,
    upperApproximation: Double => Double,
    lowerApproximation: Double => Double,
    //    stepSize: Double,
    extraSamples: Int,
    legendLabel: String) {
    addEnclosure(intervalStart, intervalEnd, upperApproximation, lowerApproximation, extraSamples, //stepSize,
      Color.blue, legendLabel)
  }

  def addDerivativeEnclosure(
    intervalStart: Double,
    intervalEnd: Double,
    upperApproximation: Double => Double,
    lowerApproximation: Double => Double,
    //    stepSize: Double,
    extraSamples: Int,
    legendLabel: String) {
    addEnclosure(intervalStart, intervalEnd, upperApproximation, lowerApproximation, extraSamples, //stepSize,
      new Color(0, 127, 0), legendLabel)
  }

  def addEnclosure(
    intervalStart: Double,
    intervalEnd: Double,
    upperApproximation: Double => Double,
    lowerApproximation: Double => Double,
    //    stepSize: Double,
    extraSamples: Int,
    color: Color,
    legendLabel: String) {

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
    //pruneLegend(plot)
    chart.removeLegend

    numberOfDatasets = numberOfDatasets + 1
  }

  /**
   * Make a copy of legend without the last legend item
   */
  def pruneLegend(plot: XYPlot) {
    val lis = plot.getLegendItems()
    if (lis.getItemCount() < 2) return
    val newLegendItems = new LegendItemCollection()
    for (i <- 0 until lis.getItemCount() - 2) { newLegendItems.add(lis.get(i)) }
    val newSource = new LegendItemSource() {
      val lic: LegendItemCollection = new LegendItemCollection()
      lic.addAll(newLegendItems)
      def getLegendItems(): LegendItemCollection = lic
    }
    chart.removeLegend()
    chart.addLegend(new LegendTitle(newSource))
    chart.getLegend().setPosition(RectangleEdge.BOTTOM)
  }

  def createChart(title: String, xlabel: String, ylabel: String): JFreeChart = {
    ChartFactory.createStackedXYAreaChart(
      title,
      xlabel,
      ylabel,
      null,
      PlotOrientation.VERTICAL,
      true, // legend
      true, // tool tips
      false // URLs
      )
  }

  def convertToPDF(width: Int, height: Int, filename: String) {
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