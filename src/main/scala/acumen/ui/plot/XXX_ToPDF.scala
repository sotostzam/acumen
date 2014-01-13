package acumen
package ui
package plot

import java.awt.geom.Rectangle2D
import java.io.FileOutputStream

import org.apache.batik.dom.GenericDOMImplementation
import org.apache.batik.svggen.SVGGraphics2D
import org.apache.batik.transcoder.TranscoderInput
import org.apache.batik.transcoder.TranscoderOutput
import org.apache.fop.svg.PDFTranscoder
import org.jfree.chart.JFreeChart

object ToPDF {
  
  def JFreeChartToPDF(chart: JFreeChart, width: Int, height: Int, filename: String) {
    try {
      // Convert JFreeChart to an SVG DOM
      val document = GenericDOMImplementation.getDOMImplementation
        .createDocument("http://www.w3.org/2000/svg", "svg", null)
      val root = document.getDocumentElement
      root.setAttributeNS(null, "width", width.toString)
      root.setAttributeNS(null, "height", height.toString)
      val svgGenerator = new SVGGraphics2D(document)
      chart.draw(svgGenerator, new Rectangle2D.Double(0, 0, width, height))
      // Transcode DOM to PDF and write to disk
      val pdfOutputSteam = new FileOutputStream(filename)
      try {
        svgGenerator getRoot document.getDocumentElement
        val transcoderInput = new TranscoderInput(document)
        val transcoderOutput = new TranscoderOutput(pdfOutputSteam)
        val transcoder = new PDFTranscoder()
        transcoder.transcode(transcoderInput, transcoderOutput)
      } finally {
        pdfOutputSteam.flush
        pdfOutputSteam.close
        svgGenerator.dispose
      }
    } catch {
      case e: Exception => e.printStackTrace
    }
  }

}
