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
import acumen.interpreters.enclosure.AbstractFrame
import javax.swing.JMenuItem
import java.awt.event.ActionListener
import java.awt.event.ActionEvent
import acumen.ui.plot.SaveAsDailog

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
    legendLabel: String,
    fun: Double => Double): TableXYDataset = {
    val lower: XYSeries     = new XYSeries(legendLabel, false, false)
    val upper: XYSeries     = new XYSeries("HIDE_ME", false, false)
    val funSeries: XYSeries = new XYSeries("SOME_FUNCTION", false, false)
    lower.add(intervalStart, lowerApproximation(intervalStart));
    upper.add(intervalStart, upperApproximation(intervalStart))
    if (fun != null) funSeries.add(intervalStart, fun(intervalStart))
    for (i <- 1 to extraSamples) {
      val x = (intervalStart + intervalEnd) * (i.doubleValue) / (extraSamples + 1)
      lower.add(x, lowerApproximation(x))
      upper.add(x, upperApproximation(x))
      if (fun != null) funSeries.add(x, fun(x))
    }
    lower.add(intervalEnd, lowerApproximation(intervalEnd))
    upper.add(intervalEnd, upperApproximation(intervalEnd))
    if (fun != null) funSeries.add(intervalEnd, fun(intervalEnd))
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
    if (fun != null) res.addSeries(funSeries)
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
    frame:AbstractFrame,
    fun: Double => Double) {
    addEnclosure(intervalStart, intervalEnd, upperApproximation, lowerApproximation, extraSamples, //stepSize,
      color, legendLabel, frame, fun)
  }
  
  def addFunctionEnclosure(
    intervalStart: Double,
    intervalEnd: Double,
    upperApproximation: Double => Double,
    lowerApproximation: Double => Double,
    //    stepSize: Double,
    extraSamples: Int,
    legendLabel: String,
    frame: AbstractFrame, 
    fun: Double => Double) {
    addEnclosure(intervalStart, intervalEnd, upperApproximation, lowerApproximation, extraSamples, //stepSize,
      Color.blue, legendLabel, frame, fun)
  }

  def addDerivativeEnclosure(
    intervalStart: Double,
    intervalEnd: Double,
    upperApproximation: Double => Double,
    lowerApproximation: Double => Double,
    //    stepSize: Double,
    extraSamples: Int,
    legendLabel: String,
    frame:AbstractFrame,
    fun: Double => Double) {
    addEnclosure(intervalStart, intervalEnd, upperApproximation, lowerApproximation, extraSamples, //stepSize,
      new Color(0, 127, 0), legendLabel, frame, fun)
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
    frame:AbstractFrame,
    fun: Double => Double) {

    val (chartPanel: ChartPanel, numberOfDatasets: Int) = chartPanels.get(legendLabel) match {
       case None => {
        val chart: JFreeChart = createChart("", "Time", legendLabel)
        chart.setNotify(false)
        chart.getPlot().setBackgroundPaint(Color.white)
        chart.getXYPlot().setDomainGridlinesVisible(true)
        chart.getXYPlot().setRangeGridlinesVisible(true)
        chart.getXYPlot().setRangeGridlinePaint(Color.gray)
        chart.getXYPlot().setDomainGridlinePaint(Color.gray)
        val chartPanel: ChartPanel = new ChartPanel(chart);
        val saveAsPdf = new JMenuItem("Save as PDF")
        val comp = this
        saveAsPdf.addActionListener(new ActionListener() {
          def actionPerformed(event: ActionEvent) {
            val d = new SaveAsDailog(Component.wrap(chartPanel), chart)
            d.pack
            d.open
          }
        })
        chartPanel.getPopupMenu.add(saveAsPdf)
        frame.add(chartPanel);
        (chartPanel,0)
      }
      case Some(t) => t
    }
    val chart = chartPanel.getChart
    
    val e = createEnclosureDataset(intervalStart, intervalEnd, upperApproximation, lowerApproximation, extraSamples, //stepSize,
      legendLabel, fun)

    val semiTransparentColor = new Color(color.getRed(), color.getGreen(), color.getBlue(), 20) //127)
    val ren = new XYDifferenceRenderer(Color.red, semiTransparentColor, false)

    ren.setStroke(new BasicStroke(1.0f))
    ren.setSeriesPaint(0, color)
    ren.setSeriesPaint(1, color)
    if (fun != null) ren.setSeriesPaint(2, Color.BLACK)

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
