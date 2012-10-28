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

class CodeArea extends EditorPane {

  val monospaced = new Font("Monospaced", Font.PLAIN, 12) 

  /* ---- state variables ---- */
  var currentFile : Option[File] = None
  var editedSinceLastSave : Boolean = false
  var editedSinceLastAutoSave : Boolean = false

  val filenameLabel = new Label("[Untitled]")

  font = monospaced

  //
  // Undo: Copied from scala web site
  //

   // New text utilities, e.g., undo, redo
  val undo = new UndoManager()
  var doc  = peer.getDocument() 
  // Create a undo action and add it to the text component
  peer.getActionMap().put("Undo",
         new javax.swing.text.TextAction("Undo") {
           def actionPerformed(e:java.awt.event.ActionEvent) {
                 try {
                     if (undo.canUndo()) {
                         undo.undo();
                     }
                 } catch {case e:Exception => }
                 
             }
        });
 // Create a redo action and add it to the text component
   peer.getActionMap().put("Redo",
         new javax.swing.text.TextAction("Redo") {
           def actionPerformed(e:java.awt.event.ActionEvent) {
                 try {
                     if (undo.canRedo()) {
                         undo.redo();
                     }
                 } catch {case e:Exception => }
                 
             }
        });
  // Listen for undo and redo events
  doc.addUndoableEditListener(undo);
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

  def listenDocument = 
    peer.getDocument.addDocumentListener(
      new DocumentListener {
        def changedUpdate(e:DocumentEvent) { setEdited }
        def insertUpdate(e:DocumentEvent) { setEdited }
        def removeUpdate(e:DocumentEvent) { setEdited }
      })

  def newFile : Unit = withErrorReporting {
    if (!editedSinceLastSave || confirmContinue(Acumen.ui.body.peer)) {
      text = ""
      listenDocument
      setCurrentFile(None)
      editedSinceLastSave = false
    }
  }

  def openFile(path: File) : Unit = withErrorReporting {
    if (!editedSinceLastSave || confirmContinue(Acumen.ui.body.peer)) {
      val fc = new FileChooser(path)
      val returnVal = fc.showOpenDialog(Acumen.ui.body)
      if (returnVal == FileChooser.Result.Approve) {
        val file = fc.selectedFile
        peer.setPage(file.toURI.toString)
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
    val returnVal = fc.showSaveDialog(Acumen.ui.body)
    if (returnVal == FileChooser.Result.Approve) {
      val file = fc.selectedFile
      if (!file.exists || confirmSave(Acumen.ui.body.peer, file)) {
        val writer = new FileWriter(fc.selectedFile)
        writer.write(text)
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
        writer.write(text)
        writer.close
        editedSinceLastSave = false
      case None => saveFileAs
    }
  }

  def autoSave = withErrorReporting {
    if (editedSinceLastAutoSave) {
      val file = Files.getFreshFile
      val writer = new FileWriter(file)
      writer.write(text)
      writer.close
    }
  }

  def withErrorReporting(action: => Unit) = Acumen.ui.withErrorReporting(action)

  listenTo(Acumen.pub)
  reactions += {
    case StateChanged(st) => 
      st match {
        case AppState.Stopped => enabled = true
        case _                => enabled = false
      }
  }

}

