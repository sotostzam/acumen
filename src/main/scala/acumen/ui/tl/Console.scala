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
import Errors.{ ParseError, PositionalAcumenError }
import scala.util.parsing.input.{Position,NoPosition}
import Logger._

class Console extends ListView[(Msg, Boolean /*messageIsOld*/)] { self =>

  import Console._

  renderer = ListView.Renderer.wrap[(Msg,Boolean)](new ListCellRenderer {
    protected val defaultRenderer = new DefaultListCellRenderer
    override def getListCellRendererComponent(list: JList, v: Any, index: Int, 
                                              isSelected: Boolean, cellHasFocus: Boolean): Component = { 
      val value = v.asInstanceOf[(Msg,Boolean)]
      val messageIsOld = value._2
      val message = value._1 match {
        case Message(ERROR, m, p)         => formatErrorMessage(m.msg, p, messageIsOld)
        case Message(_,     m, _)         => m.msg
        case HypothesisReport(md, st, et) => summarizeHypothesisOutcomes(md, messageIsOld)
      }
      val renderer = (defaultRenderer.getListCellRendererComponent(list,message,index,false,false)).asInstanceOf[JLabel]
      if (messageIsOld) renderer setForeground Color.LIGHT_GRAY
      if (isSelected) renderer setBackground MessageColorSelected
      renderer
    }
  })
  
  def formatErrorMessage(m: String, pos: Position, messageIsOld: Boolean): String =
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
        def fail(prefix: String, t: String, e: Set[(Dot,GValue)]) = 
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

  def append(instr: Instruction) = instr match {
    case StatusUpdate(before, msg, after) => listData.headOption match {
      case Some((Message(STATUS, oldMsg, _),_)) if before =>
        listData = (Message(STATUS, TextMsg(oldMsg + msg + (if (after) "..." else ""))), false) +: listData.tail
      case _ =>
        listData = (Message(STATUS, TextMsg(msg + (if (after) "..." else ""))), false) +: listData
    }
    case Separator => 
      listData = listData.map { case (msg,_) => (msg,true) }
    case m:Msg =>
      listData = (m, false) +: listData
      m match {
        case mesg @ Message(_, ExceptionMsg(e), _) =>
          scrollToError(mesg)
          System.err.println("This exception reported to console:")
          e.printStackTrace()
        case mesg: Message =>
          scrollToError(mesg)
        case _ => /* noop */
      }
      if (m.level == ERROR) 
        App.ui.lowerPane.tabs.peer.setSelectedComponent(App.ui.consolePage.self.peer)
  }

  def copySelection() {
    val selection = new java.awt.datatransfer.StringSelection(this.peer.getSelectedValues.map{
      case (Message(_,msg,_),_)           => msg
      case (HypothesisReport(md, st, et),_) => summarizeHypothesisOutcomes(md, true)
      }.mkString("\n")
       .replaceAll("<br/>", "\n").replaceAll("&nbsp;", " ") // Convert HTML whitespace to pure text
       .replaceAll("\\<.*?\\>", "")) // Clean remaining tags
    val toolkit = java.awt.Toolkit.getDefaultToolkit
    toolkit.getSystemClipboard.setContents(selection, selection)
    if (toolkit.getSystemSelection != null) // X11
      toolkit.getSystemSelection.setContents(selection, selection)
    this.peer.clearSelection // so that text can again be copied from editor 
  }
  
  def scrollToError(errorMessage: Message) {
    errorMessage match {
        // Any Position from an acumen error message should either be
        // an EnhancedPosition or NoPosition.  If it is an
        // EnhancedPosition than we should only scroll to the error if
        // the message corresponds to the file in the buffer, i.e.,
        // p.file.isEmpty.  If it is NoPosition, there is no positional
        // information and hence nothing to scroll to.
      case Message(_, m, p:EnhancedPosition) if p.file.isEmpty =>
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
    override def mouseClicked(e: MouseEvent) { 
      if (e.getClickCount == 2) self.peer.getSelectedValues.head match {
        case (m: Message, _) => scrollToError(m)
      }
    }
  }) 
  
  /* Utilities */
  
  def color(c: String, messageIsOld: Boolean, s: String) = 
    if (messageIsOld) s else s"<font color=$c>$s</font>"
  
}

object Console {

  val MessageColorInit = new Color(9,80,208)
  val MessageColorSelected = new Color(240,240,240)

}
