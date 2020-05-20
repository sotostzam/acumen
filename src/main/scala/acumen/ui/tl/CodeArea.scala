package acumen
package ui
package tl

import java.io.{File, FileWriter}
import scala.collection.mutable.{ArrayBuffer, Buffer}
import scala.swing.FileChooser
import acumen.interpreters.enclosure.Parameters
import javax.swing._
import javax.swing.event._
import scala.io.Source

class CodeArea {

  var codeText = "";
  def updateCodeText(txt: String): Unit = codeText = txt
  def sendInitCodeArea():Unit           = { Main.webInterface.socketSend(ujson.write(ujson.Obj("event" -> "codeArea", "text" -> templates(TEMPLATE_STUB)))) }
  def sendCodeArea(text: String):Unit   = { Main.webInterface.socketSend(ujson.write(ujson.Obj("event" -> "codeArea", "text" -> text))) }

  val PROMPT_CANCEL = "Cancel"
  val PROMPT_SAVE_AND_CONTINUE = "Save and continue"
  val PROMPT_DISCARD_AND_CONTINUE = "Discard and continue"
  
  /* ---- state variables ---- */
  var currentFile: Option[File] = None
  var lastFileRun: Option[File] = None
  var editedSinceLastSave: Boolean = false
  var editedSinceLastAutoSave: Boolean = false
  var editedSinceLastRun: Boolean = false
  
  private val pathChangeListeners: Buffer[ChangeListener] = new ArrayBuffer
  
  /* templates */
  
  lazy val TEMPLATE_STUB = "stub"
  lazy val TEMPLATE_MODEL = "model"
  lazy val TEMPLATE_MAIN = "main"
  lazy val TEMPLATE_INIT = "init"
  lazy val TEMPLATE_IF = "if"
  lazy val TEMPLATE_MATCH = "match"
  lazy val TEMPLATE_HS = "hs"
  lazy val TEMPLATE_MODE = "mode"
  lazy val TEMPLATE_EVENT = "event"
  lazy val TEMPLATE_PS = "ps"
  
  private def modelInitT(modelName: String, parameters: String, inits: String) =
    s"model $modelName($parameters) =\ninitially\n$inits"
  private def modelT(modelName: String, parameters: String, inits: String) =
    modelInitT(modelName, parameters, inits) + "\nalways\n    "
  private def mainModel(inits: String) = modelT("Main", "simulator", inits)
  lazy val templates = Map(
    (TEMPLATE_STUB,  modelInitT("Main","simulator","")),
    (TEMPLATE_MODEL, modelT("","","")),
    (TEMPLATE_MAIN,  mainModel("")),
    (TEMPLATE_INIT,  "initially  always"),
    (TEMPLATE_IF,    "if \n  \nnoelse,"),
    (TEMPLATE_MATCH, "match \n  -> \n    \n,"),
    (TEMPLATE_HS,    mainModel("mode = \"\"") + "match mode with\n    [ \"\" -> \n      \n    ],\n"),
    (TEMPLATE_MODE,  "  \"\" -> \n  if  mode+ = \"\"  noelse,\n  "),
    (TEMPLATE_EVENT, "if  mode+ = \"\"  noelse,\n"),
    (TEMPLATE_PS,    Parameters.defaults.map{case (p,v) => "simulator.%s := %s".format(p,v)}.mkString(";\n"))
  )

  /* --- file handling ---- */

  def currentDir =
    currentFile match {
      case Some(f) => if (f.isDirectory) f else f.getParentFile
      case None    => Files.currentDir
    }

  def setCurrentFile(f: Option[File]) = {
    currentFile = f
    currentFile match {
      case Some(f) =>
        Main.webInterface.socketSend(ujson.write(ujson.Obj("event" -> "setFilename", "data" -> f.getName)))
      case None    =>
        Main.webInterface.socketSend(ujson.write(ujson.Obj("event" -> "setFilename", "data" -> "[Untitled]")))
    }
    notifyPathChangeListeners
  }

  def fileChangedSinceLastRun(): Boolean = {
    if (lastFileRun != currentFile) true else false
  }
  def setEdited = {
    editedSinceLastSave = true
    editedSinceLastAutoSave = true
    editedSinceLastRun = true
  }

  def newFile: Unit = withErrorReporting {
    setCurrentFile(None)
    Main.webInterface.socketSend(ujson.write(ujson.Obj("event" -> "codeArea", "text" -> templates(TEMPLATE_STUB))))
  }

  def loadFile(file: File): Unit = {
    val bufferedSource = Source.fromFile(file)
    val currentFileLines = bufferedSource.getLines.toList
    sendCodeArea(currentFileLines.mkString("\n"))
    bufferedSource.close
    setCurrentFile(Some(file))
    editedSinceLastSave = false
  }

  def openFile(path: File): Unit = withErrorReporting {
    val fc = new FileChooser(path)
    fc.fileFilter = CodeArea.acumenFileFilter
    val returnVal = fc.showOpenDialog(App.ui.body)
    if (returnVal == FileChooser.Result.Approve) {
      if (fc.selectedFile.isFile) {
        loadFile(fc.selectedFile)
        notifyPathChangeListeners
      }
    }
  }
  
  def preventWorkLoss(a: => Unit) { if (!editedSinceLastSave || confirmContinue()) a } 

  def confirmSave(c: java.awt.Component, f: File) = {
    val message =
      "File " + f.toString +
        " already exists.\nAre you sure you want to overwrite it?"
    JOptionPane.showConfirmDialog(c, message,
      "Really?", JOptionPane.YES_NO_OPTION) == JOptionPane.YES_OPTION
  }

  def confirmContinue() = {
    val message = "You have changed this file since the last time it was saved.\n" +
                  "Please confirm your desired action."
    val possibleActions: Array[Object] =
      Array(PROMPT_SAVE_AND_CONTINUE, PROMPT_DISCARD_AND_CONTINUE, PROMPT_CANCEL)
    possibleActions(JOptionPane.showOptionDialog(
      null, message, "Warning", JOptionPane.DEFAULT_OPTION, JOptionPane.WARNING_MESSAGE,
      null, possibleActions, possibleActions(2))) match {
      case PROMPT_SAVE_AND_CONTINUE    => saveFile(updateCurrentFile = false); true 
      case PROMPT_DISCARD_AND_CONTINUE => true
      case PROMPT_CANCEL               => false
    }
  }
  
  /**
   * Used to save the editor's contents to the current file.
   * The parameter updateCurrentFile can be used when the save 
   * is done as an auxiliary action, to prevent other GUI 
   * components from reacting by focusing on the saved file. 
   */
  def saveFile(updateCurrentFile: Boolean = true): Unit = withErrorReporting {
    currentFile match {
      case Some(f) =>
        if (f.isDirectory) saveFileAs(updateCurrentFile)
        else writeText(f, updateCurrentFile)
      case None => 
        saveFileAs(updateCurrentFile)
    }
  }
  
  /**
   * Used to save the editor's contents to the a new file.
   * The parameter updateCurrentFile can be used when the save 
   * is done as an auxiliary action, to prevent other GUI 
   * components from reacting by focusing on the saved file. 
   */
  def saveFileAs(updateCurrentFile: Boolean = true): Unit = withErrorReporting {
    val fc = new FileChooser(currentDir)
    currentFile match {
        case Some(f) => fc.selectedFile = f
        case _ =>
      }
    if (fc.showSaveDialog(App.ui.body) == FileChooser.Result.Approve) {
      val filePath = fc.selectedFile.getAbsolutePath
      val f = if (filePath.contains("acm")) fc.selectedFile
              else new File(fc.selectedFile.getAbsolutePath + ".acm")
      if (!f.exists || confirmSave(App.ui.body.peer, f)) writeText(f, updateCurrentFile)
      setCurrentFile(Some(f))
    }
  }

  /** Write editor text contents to file, optionally updating the current file reference. */
  private def writeText(file: File, updateCurrentFile: Boolean) = {
    val writer = new FileWriter(file)
    writer.write(codeText)
    writer.close
    if (updateCurrentFile)
      setCurrentFile(Some(file))
    editedSinceLastSave = false
    notifyPathChangeListeners
  }
  
  def autoSave = withErrorReporting {
    if (editedSinceLastAutoSave) {
      val file = Files.getFreshFile
      val writer = new FileWriter(file)
      writer.write(codeText)
      writer.close
    }
  }

  def withErrorReporting(action: => Unit) = App.ui.withErrorReporting(action)

  if (Main.openFile != null) {
    loadFile(Main.openFile)
    notifyPathChangeListeners
  }
  
  /* Let the FileTree browser listen for path changes, due to file open/save actions. 
   * Open and Save As may change the path. 
   */
  def addPathChangeListener(l : ChangeListener) { pathChangeListeners += l }
  def removePathChangeListener(l : ChangeListener) { pathChangeListeners -= l }
  def notifyPathChangeListeners() { for (l <- pathChangeListeners) l.stateChanged(new ChangeEvent(this)) }
}

object CodeArea {
  val acumenFileFilter = new javax.swing.filechooser.FileFilter() {
    val acumenFileName = """.*\.acm|README""".r
    def accept(f: File): Boolean =
      if (f isDirectory) true
      else acumenFileName.pattern.matcher(f getName).matches
    def getDescription = ".acm and README files"
  }
}

