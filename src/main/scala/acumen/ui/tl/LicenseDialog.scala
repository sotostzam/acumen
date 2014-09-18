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
import scala.swing.ScrollPane
import javax.swing.border.EmptyBorder
import javax.swing.WindowConstants

/**
 * Dialog containing License information.
 * 
 * Information about authors is read from the AUTHORS file,
 * which is copied into the resources directory by sbt.
 */
object LicenseDialog extends Frame {

  title = "License"
  minimumSize = new Dimension(340, 350)
  
  contents = new BorderPanel {
    val padding = new EmptyBorder(10, 10, 10, 10)
    val version = new Label("Acumen " + acumen.util.System.version) {
      border = padding
      background = Color.WHITE
      opaque = true
    }
    def textArea = new ScrollPane(new TextArea(20,72) {
      editable = false
      border = padding
      text = // Load file from project root
        (scala.io.Source.fromInputStream(LicenseDialog.getClass.getResourceAsStream("/" + "LICENSE")).getLines().mkString("\n") ++ 
         "\n\n\n" ++
         scala.io.Source.fromInputStream(LicenseDialog.getClass.getResourceAsStream("/" + "LICENSE-Rice")).getLines().mkString("\n") ++
         "\n\n\n" ++
         scala.io.Source.fromInputStream(LicenseDialog.getClass.getResourceAsStream("/" + "LICENSE-AIC")).getLines().mkString("\n"))
      font = new Font("Monospaced", Font.PLAIN, 12)
      lineWrap = false
      peer.setCaretPosition(0)
    })
    val ok = new Button("OK"){
      background = Color.WHITE
      opaque = true
      action = new Action("OK") { def apply = LicenseDialog.close }
    }
    add(version, BorderPanel.Position.North)
    add(textArea, BorderPanel.Position.Center)
    add(ok, BorderPanel.Position.South)
  }
  pack()
  visible = false
}
