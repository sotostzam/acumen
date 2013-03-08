package acumen
package ui
package plot

import java.awt.event.{ActionListener, ActionEvent}
import java.awt.geom.Rectangle2D
import java.awt.Color
import java.io.FileOutputStream

import scala.Array.canBuildFrom
import scala.collection.JavaConversions.asScalaIterator
import scala.collection.mutable.{Map, Buffer}
import scala.swing.Graphics2D
import scala.swing.Component

import org.jfree.chart.axis.NumberAxis
import org.jfree.chart.plot.{XYPlot, CombinedDomainXYPlot}
import org.jfree.chart.renderer.xy.XYItemRenderer
import org.jfree.chart.title.LegendTitle
import org.jfree.chart.{LegendItemSource, LegendItemCollection, LegendItem, JFreeChart, ChartPanel}
import org.jfree.ui.{RectangleEdge, ApplicationFrame}

import com.itextpdf.awt.DefaultFontMapper
import com.itextpdf.text.pdf.{PdfWriter, PdfTemplate, PdfContentByte}
import com.itextpdf.text.Document

import javax.swing.{JMenuItem, JFrame, BoxLayout}
object ToPDF {

  def JFreeChartToPDF(chart: JFreeChart, width: Int, height: Int, filename: String) {
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
