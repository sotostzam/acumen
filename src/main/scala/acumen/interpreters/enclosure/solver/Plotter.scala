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
import org.jfree.chart.axis.NumberAxis

abstract trait Plotter {

  val combinedPlot = new CombinedDomainXYPlot(new NumberAxis("Time"))
  val subPlots: Map[String, (XYPlot, Int)] = Map[String, (XYPlot, Int)]()
  val enclosureRen = enclosureRenderer(Color.red)

  def createChartPanel = {
    val chart = new JFreeChart("", JFreeChart.DEFAULT_TITLE_FONT, combinedPlot, false);
    val panel = new ChartPanel(chart, true, true, true, true, false)
    val saveAsPdf = new JMenuItem("Save as PDF")
    val comp = this //TODO Find out if this is how this should be done
    saveAsPdf.addActionListener(new ActionListener() {
      def actionPerformed(event: ActionEvent) {
        val d = new SaveAsDailog(Component.wrap(panel), chart)
        d.pack
        d.open
      }
    })
    panel.getPopupMenu.add(saveAsPdf)
    panel.setBackground(Color.white)
    panel
  }
  
  def createFrame(frametitle: String) = {
    val frame = new ApplicationFrame(frametitle)
    frame.getContentPane.setLayout(new BoxLayout(frame.getContentPane,BoxLayout.Y_AXIS))
    val panel = createChartPanel
    frame.setContentPane(panel)
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
    frame:AbstractFrame,
    fun: Double => Double) {
    addEnclosure(intervalStart, intervalEnd, upperApproximation, lowerApproximation, extraSamples, //stepSize,
      color, legendLabel, frame, fun)
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
	
    combinedPlot.setNotify(false)
    val (subPlot, numberOfDatasets) = subPlots.get(legendLabel) match {
      case Some(t) => t
      case None => {
        val p = new XYPlot()
        p.setDomainGridlinesVisible(true)
        p.setRangeGridlinesVisible(true)
        p.setRangeGridlinePaint(Color.gray)
        p.setDomainGridlinePaint(Color.gray)
        p.setRangeAxis(0,new NumberAxis(legendLabel))
        combinedPlot.add(p, 1)
        (p, 0)
      }
    }
        
    val e = createEnclosureDataset(intervalStart, intervalEnd, upperApproximation, lowerApproximation, extraSamples, //stepSize,
      legendLabel, fun)

    subPlot.setDataset(numberOfDatasets, e)
    subPlot.setRenderer(numberOfDatasets, enclosureRen)
    subPlots(legendLabel) = (subPlot, numberOfDatasets + 1)
    
    combinedPlot.setNotify(false)
    
    frame.invalidate
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

}
