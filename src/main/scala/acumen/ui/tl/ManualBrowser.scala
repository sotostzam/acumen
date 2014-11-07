package acumen.ui.tl

import java.awt.{ Desktop, Toolkit }
import java.awt.datatransfer.StringSelection
import java.net.URL
import javax.swing.JEditorPane
import javax.swing.JScrollPane
import javax.swing.WindowConstants
import javax.swing.border.EmptyBorder
import javax.swing.event.HyperlinkListener
import javax.swing.event.HyperlinkEvent
import javax.swing.text.Document
import javax.swing.text.html.HTMLDocument
import scala.swing.Frame
import javax.swing.JOptionPane
import javax.swing.JTextArea

/** Window to display the Acumen manual, invoked from the Help menu. */
object ManualBrowser extends Frame {

  title = "User Guide and Reference Manual"
  
  val jep = new JEditorPane{
    override def copy() {
      // Remove junk character stemming from how &nbsp; is interpreted by JEditorPane 
      // (not accepted as whitespace by Acumen parser)
      val cleanedSelection = getSelectedText.map{ c => if (c.intValue == 160) ' ' else c }
      val cb = Toolkit.getDefaultToolkit.getSystemClipboard
      cb.setContents(new StringSelection(cleanedSelection), null)
    }
  }
  jep.setEditorKit(JEditorPane createEditorKitForContentType "text/html")
  jep.setEditable(false)
  jep.setBorder(new EmptyBorder(5, 5, 5, 0))
  
  jep.addHyperlinkListener(new HyperlinkListener {
    def hyperlinkUpdate(e: HyperlinkEvent) {
      if(e.getEventType == HyperlinkEvent.EventType.ACTIVATED)
        if(Desktop isDesktopSupported)
          Desktop.getDesktop browse (e.getURL.toURI)
        else {
          val message = new JTextArea("A newer version of Java (6 or later) is required to open links.\n" + 
            "Please try the online version of this manual at: http://bit.ly/Acumen-manual-2014.")
          message setEditable false
          message setOpaque false
          JOptionPane.showMessageDialog(ManualBrowser.peer, message, "Can not open link", JOptionPane.ERROR_MESSAGE)
        }
    }
  })

  jep.setPage(getClass getResource "manual.html")
  jep.getDocument.asInstanceOf[HTMLDocument].getStyleSheet.addRule("body { font-family: sans-serif; }")
  
  peer.add(new JScrollPane(jep))
  peer.setSize(630, 400)
  peer.setDefaultCloseOperation(WindowConstants.HIDE_ON_CLOSE)
  
}