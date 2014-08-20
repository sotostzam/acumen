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
import java.awt.event.{ MouseAdapter, MouseEvent }
import javax.swing.border.LineBorder
import Console._
import Errors.{ ParseError, PositionalAcumenError }
import scala.util.parsing.input.Position

class Console extends ListView[ConsoleMessage] {

  private var oldEntries = 0
  var done = false
  val initColor = selectionBackground
  renderer = ListView.Renderer.wrap[ConsoleMessage](new ConsoleCellRenderer)
  
  val consoleFlasher = new SwingUtil.Flasher()
  
  class ConsoleCellRenderer extends ListCellRenderer {
    protected val defaultRenderer = new DefaultListCellRenderer
    override def getListCellRendererComponent(
      list: JList, value: Any, index: Int, 
      isSelected: Boolean, cellHasFocus: Boolean): Component = { 
      val messageIsOld = index >= oldEntries
      val message = value match {
        case NormalMessage(m) => m
        case ErrorMessage(m) => formatErrorMessage(m, None, messageIsOld)
        case PositionalErrorMessage(m, p) => formatErrorMessage(m, Some(p), messageIsOld)
        case _ => ""
      }
      val renderer = (defaultRenderer.getListCellRendererComponent(list,message,index,false,false)).asInstanceOf[JLabel]
      if (messageIsOld) renderer.setForeground(Color.LIGHT_GRAY)
      if (isSelected) renderer.setBackground(new Color(240,240,240))
      renderer
    }
  }
  
  def formatErrorMessage(m: String, pos: Option[Position], messageIsOld: Boolean): String =
    "<html>"+
    (if (messageIsOld) "ERROR:"
    else "<font color=red>ERROR:</font>") +
    "<pre>"+ 
    (m.replaceAll("<","&lt;")
      .replaceAll(">","&gt;")
      .replaceAll("\n","<br/>")) + 
    "</pre></html>"

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

  def logError(e: Throwable) = {
    logMessage(e match {
      case pe: PositionalAcumenError => PositionalErrorMessage(pe.getMessage, pe.pos)
      case _                         => ErrorMessage(e.getMessage)
    })
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
  
  def scrollToError() {
    this.peer.getSelectedValues.head match {
        // Any Position from an acumen error message should either be
        // an EnhancedPosition or NoPosition.  If it is an
        // EnhancedPosition than we should only scroll to the error if
        // the message corresponds to the file in the buffer, i.e.,
        // p.file.isEmpty.  If it is NoPosition, there is no positional
        // information and hence nothing to scroll to.
      case PositionalErrorMessage(m, p:EnhancedPosition) if p.file.isEmpty =>
        val ta = ui.App.ui.codeArea.textArea
        ta.setCaretPosition(ta.getDocument.getDefaultRootElement.getElement(p.line - 1).getStartOffset + p.column - 1)
        ui.App.ui.codeArea.centerLineInScrollPane
        ta.addLineHighlight(p.line - 1, new Color(255, 240, 240))
        ta.requestFocus
      case _ =>
    }
  }
  
  this.peer.getActionMap.put("copy", new javax.swing.AbstractAction{
    def actionPerformed(e: java.awt.event.ActionEvent) { copySelection() }
  })

  this.peer.addMouseListener(new MouseAdapter {
    override def mouseClicked(e: MouseEvent) { if (e.getClickCount == 2) scrollToError() }
  })

}
object Console {

  sealed abstract class ConsoleMessage
  case class NormalMessage(message: String) extends ConsoleMessage
  case class ErrorMessage(message: String) extends ConsoleMessage
  case class PositionalErrorMessage(message: String, pos: Position) extends ConsoleMessage

}
