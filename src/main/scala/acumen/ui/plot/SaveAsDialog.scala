package acumen
package ui
package plot
import scala.swing.Dialog
import javax.swing.JSpinner
import scala.swing.FlowPanel
import scala.swing.BorderPanel
import scala.swing.BoxPanel
import scala.swing.GridPanel
import javax.swing.JLabel
import scala.swing.Button
import scala.swing.TextField
import scala.swing.Orientation
import scala.swing.FileChooser
import java.io.File
import scala.swing.Component
import org.jfree.chart.JFreeChart
import com.itextpdf.text.pdf.PdfWriter
import com.itextpdf.text.Document
import java.io.FileOutputStream
import com.itextpdf.awt.DefaultFontMapper
import java.awt.geom.Rectangle2D

class SaveAsDailog (parent: Component, chart:JFreeChart) extends Dialog(null) {
  modal = true

  private var currentDir = new File(".")
  private def fresh = Files.getFreshFile(currentDir, "png")
  private var currentWidth = 640
  private var currentHeight = 480

  val widthSpin = new JSpinner() { setValue(currentWidth) }
  val heightSpin = new JSpinner() { setValue(currentHeight) }
  val inputField = new TextField(fresh.getCanonicalPath, 20)
  val openButton = Button("Browse") {
    val fc = new FileChooser(currentDir) {
      selectedFile = new File(inputField.text)
    }
    val returnVal = fc.showOpenDialog(parent)
    if (returnVal == FileChooser.Result.Approve) {
      val file = fc.selectedFile
      inputField.text = file.getAbsolutePath
    }
  }
  val upperPane = new GridPanel(2, 2) {
    border = javax.swing.BorderFactory.createEmptyBorder(5, 5, 5, 5)
    vGap = 5
    peer.add(new JLabel("Width"))
    peer.add(widthSpin)
    peer.add(new JLabel("Height"))
    peer.add(heightSpin)
  }
  val lowerPane =
    new FlowPanel(FlowPanel.Alignment.Leading)(inputField, openButton)
  val options = new BoxPanel(Orientation.Vertical) {
    contents += (upperPane, lowerPane)
  }
  val cancel = Button("Cancel")(dispose)
  val save = Button("Save") {
    val f = new File(inputField.text)
    currentDir = f.getParentFile
    currentHeight = heightSpin.getValue.asInstanceOf[Int]
    currentWidth = widthSpin.getValue.asInstanceOf[Int]
//    plotPanel.render(f, currentWidth, currentHeight)
    convertToPDF(chart, currentWidth, currentHeight, inputField.text)
    dispose
  }
  val buttons = new FlowPanel(FlowPanel.Alignment.Trailing)(cancel, save)
  contents = new BorderPanel {
    add(options, BorderPanel.Position.Center)
    add(buttons, BorderPanel.Position.South)
  }
  
  def convertToPDF(c: JFreeChart, width: Int, height: Int, filename: String) {
    val document = new Document(new com.itextpdf.text.Rectangle(width, height))
    try {
      val writer = PdfWriter.getInstance(document, new FileOutputStream(filename))
      document.open
      val cb = writer.getDirectContent
      val tp = cb.createTemplate(width, height)
      val g2d = tp.createGraphics(width, height, new DefaultFontMapper)
      val r2d = new Rectangle2D.Double(0, 0, width, height)
      c.draw(g2d, r2d)
      g2d.dispose
      cb.addTemplate(tp, 0, 0)
    } catch {
      case e: Exception => e.printStackTrace
    } finally {
      document.close()
    }
  }
  
  
}