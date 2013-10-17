package acumen.ui.tl

import scala.swing.Frame
import javax.swing.JEditorPane
import javax.swing.JScrollPane
import javax.swing.WindowConstants
import javax.swing.border.EmptyBorder
import javax.swing.text.Document
import javax.swing.text.html.HTMLDocument
import java.net.URL

/** Window to display the Acumen manual, invoked from the Help menu. */
class  ManualBrowser
object ManualBrowser extends Frame {

  title = "User Guide and Reference Manual"
  
  val jep = new JEditorPane
  jep.setEditable(false)
  jep.setBorder(new EmptyBorder(5, 5, 5, 0))

  jep.setPage(getClass getResource "manual.html")
  jep.getDocument.asInstanceOf[HTMLDocument].getStyleSheet.addRule("body { font-family: sans-serif; }")
  
  peer.add(new JScrollPane(jep))
  peer.setSize(300, 400)
  peer.setDefaultCloseOperation(WindowConstants.HIDE_ON_CLOSE)
  
}