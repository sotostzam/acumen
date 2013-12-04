package acumen.ui.tl

import java.awt.Color
import java.awt.Dimension
import java.awt.Font
import scala.swing.Action
import scala.swing.BorderPanel
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
    val version = new Label("Acumen " + acumen.util.System.version) {
      border = padding
      background = Color.WHITE
      opaque = true
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