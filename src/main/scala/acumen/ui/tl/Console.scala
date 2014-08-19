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
        case ErrorMessage(m)              => formatErrorMessage(m, None, messageIsOld)
        case PositionalErrorMessage(m, p) => formatErrorMessage(m, Some(p), messageIsOld)
        case HypothesisReport(md)         => summarizeHypothesisOutcomes(md,messageIsOld)
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

  private def summarizeHypothesisOutcomes(md: Metadata, messageIsOld: Boolean): String = {
    val (mLenCId, mLenCN, mLenHN) = md.hyp.foldLeft((0, 0, 0)) {
      case ((rid, rcn, rhn), ((id,cn,hn), od)) =>
        val sid = id.cid.toString
        ( Math.max(rid, sid.length)
        , Math.max(rcn, cn.x.length)
        , Math.max(rhn, hn.map(_.length).getOrElse(0)))
    }
    val (report, numPassed) = md.hyp.toList.reverse.foldLeft(("", 0)) {
      case ((tmpReport, tmpAllPassed), ((id, cn, hn), od)) =>
        val sid = s"(#${id.cid.toString}:${cn.x})".padTo(mLenCId + mLenCN + 2, "&nbsp;").mkString
        val shn = hn.map("'" + _ + "'").getOrElse("").padTo(mLenHN + 2, "&nbsp;").mkString
        val sod = od.map("Falsified at " + _).getOrElse("OK")
        val r = s"$sid $shn $sod<br/>"
        ( (if (od.nonEmpty && !messageIsOld) color("red", "- ") else "+ ") + r + tmpReport
        , tmpAllPassed + (if (od.isEmpty) 1 else 0))
    }
    val numFalse = md.hyp.keys.size - numPassed
    val c = (numFalse, messageIsOld) match {
      case (_, true) => ""
      case (0, _)    => "green"
      case _         => "red"
    }
    val header = (numFalse, md.hyp.keys.size) match {
      case (0, 1) => color(c, s"HYPOTHESIS NOT FALSIFIED")
      case (0, t) => color(c, s"NONE OF $t HYPOTHESES FALSIFIED")
      case (1, 1) => color(c, "HYPOTHESIS FALSIFIED")
      case (n, t) => color(c, s"$n/$t HYPOTHESES FALSIFIED")
    }
    s"<html>$header<br/><font face=monospace>$report</font></html>"
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
        case HypothesisReport(md) => HypothesisReport(md)  //FIXME 
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
  
  def logHypothesisReport(md: Metadata) =
    if (md.hyp.nonEmpty)
      logMessage(HypothesisReport(md))
  
  private def logMessage(m: ConsoleMessage) {
	  oldEntries += 1
    listData = m +: listData
  }

  def copySelection() {
    val selection = new java.awt.datatransfer.StringSelection(this.peer.getSelectedValues.map{
        case NormalMessage(m)     => m
        case ErrorMessage(m)      => m
        case HypothesisReport(md) => summarizeHypothesisOutcomes(md, true)
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
      case PositionalErrorMessage(m, p) =>
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
  
  /* Utilities */
  
  def color(c: String, s: String) = if (c == "") s else s"<font color=$c>$s</font>"
  
}
object Console {

  sealed abstract class ConsoleMessage
  case class NormalMessage(message: String) extends ConsoleMessage
  case class HypothesisReport(md: Metadata) extends ConsoleMessage
  case class ErrorMessage(message: String) extends ConsoleMessage
  case class PositionalErrorMessage(message: String, pos: Position) extends ConsoleMessage

}
