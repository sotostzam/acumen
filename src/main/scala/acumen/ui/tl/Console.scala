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
  val selectedColor = new Color(240,240,240)
  renderer = ListView.Renderer.wrap[ConsoleMessage](new ConsoleCellRenderer)
  
  val consoleFlasher = new SwingUtil.Flasher()
  
  class ConsoleCellRenderer extends ListCellRenderer {
    protected val defaultRenderer = new DefaultListCellRenderer
    override def getListCellRendererComponent(
      list: JList, value: Any, index: Int, 
      isSelected: Boolean, cellHasFocus: Boolean): Component = { 
      val messageIsOld = index >= oldEntries
      val message = value match {
        case NormalMessage(m)             => m
        case ErrorMessage(m)              => formatErrorMessage(m, None, messageIsOld)
        case PositionalErrorMessage(m, p) => formatErrorMessage(m, Some(p), messageIsOld)
        case HypothesisReport(md, st, et) => summarizeHypothesisOutcomes(md, messageIsOld)
      }
      val renderer = (defaultRenderer.getListCellRendererComponent(list,message,index,false,false)).asInstanceOf[JLabel]
      if (messageIsOld) renderer.setForeground(Color.LIGHT_GRAY)
      if (isSelected) renderer.setBackground(selectedColor)
      renderer
    }
  }

  def formatErrorMessage(m: String, pos: Option[Position], messageIsOld: Boolean): String =
    "<html>" + color("red", messageIsOld, "ERROR:") + "<pre>" + 
      m.replaceAll("<","&lt;")
       .replaceAll(">","&gt;")
       .replaceAll("\n","<br/>") + 
      "</pre></html>"

  private def summarizeHypothesisOutcomes(md: SomeMetadata, messageIsOld: Boolean): String = {
    def colorSuccess(m: String)   = color("green",  messageIsOld, m)
    def colorUncertain(m: String) = color("ff9900", messageIsOld, m)
    def colorFailure(m: String)   = color("red",    messageIsOld, m)
    val (mLenCId, mLenCN, mLenHN) = md.hyp.foldLeft((0, 0, 0)) {
      case ((rid, rcn, rhn), ((id,cn,hn), od)) =>
        val sid = id.cid.toString
        ( Math.max(rid, sid.length)
        , Math.max(rcn, cn.x.length)
        , Math.max(rhn, hn.map(_.length).getOrElse(0)))
    }
    val (report, successes, uncertains, failures) = md.hyp.toList.reverse.foldLeft(("", 0, 0, 0)) {
      case ((resReport, resS, resU, resF), ((id, cn, hn), ho)) =>
        val sid = s"(#${id.cid.toString}:${cn.x})".padTo(mLenCId + mLenCN + 2, "&nbsp;").mkString
        val shn = hn.map("'" + _ + "'").getOrElse("").padTo(mLenHN + 2, "&nbsp;").mkString
        def fail(prefix: String, t: String, e: Set[(Dot,CValue)]) = 
          s"$prefix $t" + (if (e isEmpty) "" else ", where " + e.map { case (d, v) => 
            val lhs = Pretty pprint (if (d.obj == Var(util.Canonical.self)) Var(d.field) else (d:Expr)) 
            s"$lhs = ${Pretty pprint v}"
          }.mkString(", "))
        val (s, u, f, symbol, sho) = ho match {
          case TestSuccess            => (1, 0, 0,                "+",       "Tested")
          case TestFailure(t, e)      => (0, 0, 1, colorFailure  ("-"), fail("Tested false at", t.toString, e))
          case CertainSuccess         => (1, 0, 0,                "+",       "Proved")
          case UncertainFailure(t, e) => (0, 1, 0, colorUncertain("?"), fail("Inconclusive over", s"[${t._1}..${t._2}]", e))
          case CertainFailure(t, e)   => (0, 0, 1, colorFailure  ("-"), fail("Disproved over", s"[${t._1}..${t._2}]", e))
        }
        (s"$symbol $sid $shn $sho<br/>$resReport", resS + s, resU + u, resF + f)
    }
    val domain = s" OVER [${md.timeDomain._1}..${md.timeDomain._2}]" 
    val header = (successes, uncertains, failures) match {
      case (_, _, f) if f > 0 =>
        colorFailure ("SOME HYPOTHESES " + (if (md.rigorous) "DISPROVED" else "FALSIFIED") + domain)
      case (_, u, 0) if u > 0 =>
        colorUncertain("SOME HYPOTHESES INCONCLUSIVE" + domain)
      case _  =>
        colorSuccess((if (md.rigorous) "ALL HYPOTHESES PROVED" else "NO HYPOTHESES FALSIFIED") + domain)
    }
    val summary = s"$successes TRUE, $failures FALSE, $uncertains INCONCLUSIVE"
    s"<html>$header<br/>$summary<br/><font face=monospace>$report</font></html>"
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
        case HypothesisReport(md,st,et) => HypothesisReport(md,st,et)  //FIXME 
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
  
  def logHypothesisReport(md: Metadata, startTime: Double, endTime: Double) = md match {
    case NoMetadata =>
    case smd: SomeMetadata => logMessage(HypothesisReport(smd, startTime, endTime)) 
  }
  
  private def logMessage(m: ConsoleMessage) {
	  oldEntries += 1
    listData = m +: listData
  }

  def copySelection() {
    val selection = new java.awt.datatransfer.StringSelection(this.peer.getSelectedValues.map{
      case NormalMessage(m)             => m
      case ErrorMessage(m)              => m
      case HypothesisReport(md, st, et) => summarizeHypothesisOutcomes(md, true)
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
  
  /* Utilities */
  
  def color(c: String, messageIsOld: Boolean, s: String) = 
    if (messageIsOld) s else s"<font color=$c>$s</font>"
  
}
object Console {

  sealed abstract class ConsoleMessage
  case class NormalMessage(message: String) extends ConsoleMessage
  case class HypothesisReport(md: SomeMetadata, startTime: Double, endTime: Double) extends ConsoleMessage
  case class ErrorMessage(message: String) extends ConsoleMessage
  case class PositionalErrorMessage(message: String, pos: Position) extends ConsoleMessage
  
  def logHypothesisReport(md: Metadata, startTime: Double, endTime: Double): Unit =
    if (ui.App.ui == null) 
      () // TODO Acumen running headless, log to command line
    else 
      ui.App.ui.console.logHypothesisReport(md, startTime, endTime)
}
