package acumen.ui.tl

import java.awt.Color
import java.awt.Dimension
import java.awt.Font
import scala.swing.Action
import scala.swing.{BorderPanel, BoxPanel}
import scala.swing.{Alignment,Orientation}
import scala.swing.Button
import scala.swing.Frame
import scala.swing.Label
import scala.swing.TextArea
import javax.swing.border.EmptyBorder
import javax.swing.WindowConstants

/**
 * Dialog containing information about Acumen.
 * 
 * Information about authors is read from the AUTHORS file,
 * which is copied into the resources directory by sbt.
 */
object AboutDialog extends Frame {

  title = "About"
  minimumSize = new Dimension(340, 350)
  
  contents = new BorderPanel {
    val padding = new EmptyBorder(10, 10, 10, 10)
    val version = new BoxPanel(Orientation.Vertical) {
        contents += new Label("Acumen " + acumen.util.System.version) {
          background = Color.WHITE
          opaque = true
          xLayoutAlignment = 0.5
        }
        acumen.util.System.buildId.foreach { s => contents += new Label("Build id " + s) {
          font = font.deriveFont(java.awt.Font.PLAIN)
          background = Color.WHITE
          opaque = true
          xLayoutAlignment = 0.5
        }}
        border = padding
        background = Color.WHITE
    }
    val authors = new TextArea {
      editable = false
      border = padding
      val authorsText = // Load AUTHORS file from project root
        scala.io.Source.fromInputStream(AboutDialog.getClass getResourceAsStream "/AUTHORS")
          .getLines().mkString("\n")
      peer setFont new Font("Monospaced", Font.PLAIN, 12)
      peer setLineWrap true
      peer setWrapStyleWord true
      peer setText authorsText
    }
    val ok = new Button("OK"){
      background = Color.WHITE
      opaque = true
      action = new Action("OK") { def apply = AboutDialog.close }
    }
    add(version, BorderPanel.Position.North)
    add(authors, BorderPanel.Position.Center)
    add(ok, BorderPanel.Position.South)
  }

  pack()
  visible = false

}
