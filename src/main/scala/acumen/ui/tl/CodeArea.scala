package acumen
package ui
package tl

import java.awt.Font
import java.io._
import javax.swing.{JOptionPane, KeyStroke}
import javax.swing.event.{DocumentEvent, DocumentListener}
import javax.swing.text._
import javax.swing.undo._
import scala.collection.JavaConversions._
import scala.swing._
import org.fife.ui.rsyntaxtextarea.RSyntaxTextArea
import java.awt.LayoutManager
import java.awt.BorderLayout
import org.fife.ui.rsyntaxtextarea.SyntaxConstants
import org.fife.ui.rsyntaxtextarea.TokenMakerFactory
import org.fife.ui.rsyntaxtextarea.AbstractTokenMakerFactory

import acumen.ui.App;
import acumen.ui.Files;

class CodeArea extends Panel { //EditorPane {

  val syntaxTextArea = createSyntaxTextArea
  this.peer.setLayout(new BorderLayout)
  this.peer.add(syntaxTextArea,BorderLayout.CENTER)
  
  /* ---- state variables ---- */
  var currentFile : Option[File] = None
  var editedSinceLastSave : Boolean = false
  var editedSinceLastAutoSave : Boolean = false

  val filenameLabel = new Label("[Untitled]")

  /** 
   * Create RSyntaxTextArea component. It provides features such as syntax highlighting and indentation.
   * The highlighting is based on a syntax specification AcumenTokenMakerMaker.xml which has been compiled 
   * to the java class acumen.ui.tl.AcumenTokenMaker.java.
   * For more information, see https://github.com/effective-modeling/acumen/wiki/Acumen-development
   */
  def createSyntaxTextArea: RSyntaxTextArea = {
    val sta = new RSyntaxTextArea
    TokenMakerFactory.getDefaultInstance.asInstanceOf[AbstractTokenMakerFactory].
      putMapping("AcumenTokenMaker", classOf[acumen.ui.tl.AcumenTokenMaker].getName)
    sta.setSyntaxEditingStyle("AcumenTokenMaker")
    sta.setHighlightCurrentLine(false)
    sta.setTabSize(2)
    sta.setFont(new Font("Monospaced", Font.PLAIN, 12))
    sta
  }
  
  //
  // Undo: Copied from 
  //   http://docs.oracle.com/javase/tutorial/uiswing/components/generaltext.html
  //

  //var undo = new UndoManager()
  var undo = new UndoManager()
  // Create a undo action and add it to the text component
  peer.getActionMap().put("Undo",
         new javax.swing.text.TextAction("Undo") {
           def actionPerformed(e:java.awt.event.ActionEvent) {
                 //try {
                     if (undo.canUndo()) {
                         println("Undoing!")
                         undo.undo();
                     }
                 //} catch {case e:Exception => }
                 
             }
        });
 // Create a redo action and add it to the text component
   peer.getActionMap().put("Redo",
         new javax.swing.text.TextAction("Redo") {
           def actionPerformed(e:java.awt.event.ActionEvent) {
                 //try {
                     if (undo.canRedo()) {
                         undo.redo();
                     }
                 //} catch {case e:Exception => }
                 
             }
        });
  // Bind the undo action to ctl-Z
  peer.getInputMap().put(KeyStroke.getKeyStroke("control Z"), "Undo");
  // Bind the redo action to ctl-Y
  peer.getInputMap().put(KeyStroke.getKeyStroke("control Y"), "Redo");		

  //
  // End copy
  //

  /* --- file handling ---- */

  def currentDir =
    currentFile match {
      case Some(f) => f.getParentFile
      case None    => Files.currentDir
    }

  def setCurrentFile(f:Option[File]) = {
    currentFile = f
    filenameLabel.text = 
      currentFile match {
        case Some(f) => f.getName
        case None    => "[Untitled]"
      }
  }

  def setEdited = {
    if (!editedSinceLastSave) filenameLabel.text += " (changed)"
    editedSinceLastSave = true
    editedSinceLastAutoSave = true
  }

  def listenDocument = {
    syntaxTextArea.getDocument.addDocumentListener(
      new DocumentListener {
        def changedUpdate(e:DocumentEvent) { setEdited }
        def insertUpdate(e:DocumentEvent) { setEdited }
        def removeUpdate(e:DocumentEvent) { setEdited }
      })
    // Listen for undo and redo events
    syntaxTextArea.getDocument.addUndoableEditListener(undo)
  }

  def newFile : Unit = withErrorReporting {
    if (!editedSinceLastSave || confirmContinue(App.ui.body.peer)) {
      syntaxTextArea.setText("")
      setCurrentFile(None)
      editedSinceLastSave = false
      undo.discardAllEdits()
    }
  }

  def openFile(path: File) : Unit = withErrorReporting {
    if (!editedSinceLastSave || confirmContinue(App.ui.body.peer)) {
      val fc = new FileChooser(path)
      val returnVal = fc.showOpenDialog(App.ui.body)
      if (returnVal == FileChooser.Result.Approve) {
        val file = fc.selectedFile
//        peer.setPage(file.toURI.toString)
        syntaxTextArea.read(new FileReader(file),null)
        undo = new UndoManager()
        listenDocument
        setCurrentFile(Some(file))
        editedSinceLastSave = false
      }
    }
  }
  
  def confirmSave(c: java.awt.Component, f:File) = {
    val message = 
      "File " + f.toString + 
      " already exists.\nAre you sure you want to overwrite it?"
    JOptionPane.showConfirmDialog(c, message,
      "Really?", JOptionPane.YES_NO_OPTION) == JOptionPane.YES_OPTION
  }

  def confirmContinue(c:java.awt.Component) = {
    val message = "Last changes have not been saved\n" +
                  "Are you sure you want to continue?"
    JOptionPane.showConfirmDialog(c, message,
      "Really?", JOptionPane.YES_NO_OPTION) == JOptionPane.YES_OPTION
  }

  def saveFileAs : Unit = withErrorReporting {
    val fc = new FileChooser(currentDir)
    val returnVal = fc.showSaveDialog(App.ui.body)
    if (returnVal == FileChooser.Result.Approve) {
      val file = fc.selectedFile
      if (!file.exists || confirmSave(App.ui.body.peer, file)) {
        val writer = new FileWriter(fc.selectedFile)
        writer.write(syntaxTextArea.getText)
        writer.close
        setCurrentFile(Some(file))
        editedSinceLastSave = false
      }
    }
  }

  def saveFile : Unit = withErrorReporting {
    currentFile match {
      case Some(f) =>
        val writer = new FileWriter(f)
        writer.write(syntaxTextArea.getText)
        writer.close
        editedSinceLastSave = false
      case None => saveFileAs
    }
  }

  def autoSave = withErrorReporting {
    if (editedSinceLastAutoSave) {
      val file = Files.getFreshFile
      val writer = new FileWriter(file)
      writer.write(syntaxTextArea.getText)
      writer.close
    }
  }

  def withErrorReporting(action: => Unit) = App.ui.withErrorReporting(action)

  listenTo(App.pub)
  reactions += {
    case st:App.State => 
      st match {
        case App.Stopped => enabled = true
        case _           => enabled = false
      }
  }

}

