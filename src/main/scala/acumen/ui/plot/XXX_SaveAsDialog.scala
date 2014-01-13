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
import java.io.FileOutputStream
import java.awt.geom.Rectangle2D

class SaveAsDailog (parent: Component, chart: JFreeChart) extends Dialog(null) {
  modal = true

  private var currentDir = new File(".")
  private def fresh = Files.getFreshFile(currentDir, "pdf")
  private var currentWidth = 640
  private var currentHeight = 480

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
  val options = new FlowPanel(FlowPanel.Alignment.Leading)(inputField, openButton)
  val cancel = Button("Cancel")(dispose)
  val save = Button("Save") {
    val f = new File(inputField.text)
    currentDir = f.getParentFile
    ToPDF.JFreeChartToPDF(chart, currentWidth, currentHeight, inputField.text)
    dispose
  }
  val buttons = new FlowPanel(FlowPanel.Alignment.Trailing)(cancel, save)
  contents = new BorderPanel {
    add(options, BorderPanel.Position.Center)
    add(buttons, BorderPanel.Position.South)
  }

}
