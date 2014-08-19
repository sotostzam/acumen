package acumen
package ui
package tl

import java.awt.Color
import swing._
import javax.swing.ListCellRenderer
import javax.swing.DefaultListCellRenderer
import javax.swing.JList
import javax.swing.JLabel
import java.awt.Component
import javax.swing.border.LineBorder
import Console.{ConsoleMessage, ErrorMessage, NormalMessage}

class Console extends ListView[ConsoleMessage] {

  private var oldEntries = 0
  var done = false
  val initColor = selectionBackground
  renderer = ListView.Renderer.wrap[ConsoleMessage](new ConsoleCellRenderer)
  
  val consoleFlasher = new SwingUtil.Flasher()
  
  class ConsoleCellRenderer extends ListCellRenderer {
    protected val defaultRenderer = new DefaultListCellRenderer
  
    override def getListCellRendererComponent(
      list:JList, value:Any, index:Int, 
      isSelected:Boolean, cellHasFocus:Boolean) : Component = { 
      val messageIsOld = index >= oldEntries
      val message = value match {
        case NormalMessage(m) => m
        case ErrorMessage(m) =>
          "<html>"+
          (if (messageIsOld) "ERROR:"
          else "<font color=red>ERROR:</font>") +
          "<pre>"+ 
          (m.replaceAll("<","&lt;")
            .replaceAll(">","&gt;")
            .replaceAll("\n","<br/>")) + 
          "</pre></html>"
        case _ => ""
      }
      val renderer = (defaultRenderer.getListCellRendererComponent(list,message,index,false,false)).asInstanceOf[JLabel]
      //renderer.setBorder(LineBorder.createGrayLineBorder)
      if (messageIsOld) renderer.setForeground(Color.LIGHT_GRAY)
      if (isSelected) renderer.setBackground(new Color(240,240,240))
      renderer
    }
  }

  def log(message:String) = {
    selectionBackground = initColor
    if (done) {
      logMessage(NormalMessage(message))
      done = false
    }
    else {
      if (listData.isEmpty) listData = Seq(NormalMessage(message))
      else listData = (listData.head match {
        case NormalMessage(m) => NormalMessage(m+message) 
        case ErrorMessage(m) => ErrorMessage(m+message) 
      }) +: listData.tail
    }
  }

  def newLine = { done = true }
  
  def fadeOldMessages() = { oldEntries = 0 }

  def logError(message:String) = {
    logMessage(ErrorMessage(message))
    done = true
    SwingUtil.flashFunction(
      App.ui.consolePage.background_=, Color.WHITE, Color.RED, consoleFlasher)
  }
  
  private def logMessage(m: ConsoleMessage) {
	  oldEntries += 1
    listData = m +: listData
  }

  def copySelection() {
    val selection = new java.awt.datatransfer.StringSelection(this.peer.getSelectedValues.map{
        case NormalMessage(m)     => m
        case ErrorMessage(m)      => m
      }.mkString("\n")
       .replaceAll("<br/>", "\n").replaceAll("&nbsp;", " ") // Convert HTML whitespace to pure text
       .replaceAll("\\<.*?\\>", "")) // Clean remaining tags
    val toolkit = java.awt.Toolkit.getDefaultToolkit
    toolkit.getSystemClipboard.setContents(selection, selection)
    if (toolkit.getSystemSelection != null) // X11
      toolkit.getSystemSelection.setContents(selection, selection)
    this.peer.clearSelection // so that text can again be copied from editor 
  }
  
  this.peer.getActionMap.put("copy", new javax.swing.AbstractAction{
    def actionPerformed(e: java.awt.event.ActionEvent) { copySelection() }
  })
  
}
object Console {

  sealed abstract class ConsoleMessage
  case class NormalMessage(message: String) extends ConsoleMessage
  case class ErrorMessage(message: String) extends ConsoleMessage

}
