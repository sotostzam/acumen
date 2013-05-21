package acumen
package ui
package tl

import java.awt.BorderLayout
import java.awt.Color
import java.awt.Font
import java.awt.GraphicsEnvironment
import java.io.File
import java.io.FileReader
import java.io.FileWriter
import scala.Array.canBuildFrom
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Buffer
import scala.swing.FileChooser
import scala.swing.Label
import scala.swing.Panel
import scala.xml.XML
import org.fife.ui.autocomplete.AutoCompletion
import org.fife.ui.autocomplete.BasicCompletion
import org.fife.ui.autocomplete.DefaultCompletionProvider
import org.fife.ui.rsyntaxtextarea.AbstractTokenMakerFactory
import org.fife.ui.rsyntaxtextarea.RSyntaxTextArea
import org.fife.ui.rsyntaxtextarea.SyntaxConstants
import org.fife.ui.rsyntaxtextarea.TokenMakerFactory
import org.fife.ui.rsyntaxtextarea.TokenTypes
import org.fife.ui.rsyntaxtextarea.templates.StaticCodeTemplate
import acumen.Parser
import acumen.ui.App
import acumen.ui.Files
import acumen.ui.GraphicalMain
import acumen.util.System.FILE_SUFFIX_MODEL
import javax.swing.JFileChooser
import javax.swing.JOptionPane
import javax.swing.event.ChangeEvent
import javax.swing.event.ChangeListener
import javax.swing.event.DocumentEvent
import javax.swing.event.DocumentListener
import javax.swing.event.TreeSelectionEvent
import javax.swing.event.TreeSelectionListener
import javax.swing.UIManager
import scala.swing.Button
import acumen.interpreters.enclosure.Parameters
import javax.swing.KeyStroke

class CodeArea extends Panel with TreeSelectionListener {

  val textArea = createSyntaxTextArea
  
  val DEFAULT_FONT_SIZE = 12
  val DEFAULT_FONT_NAME = Font.MONOSPACED
  val KNOWN_FONTS = Set("courier new", "courier", "monaco", "consolas", "lucida console",
    "andale mono", "profont", "monofur", "proggy", "droid sans mono",
    "lettergothic", "bitstream vera sans mono", "crystal", "ubuntu monospace")

  val PROMPT_CANCEL = "Cancel"
  val PROMPT_SAVE_AND_CONTINUE = "Save and continue"
  val PROMPT_DISCARD_AND_CONTINUE = "Discard and continue"
    
  this.peer.setLayout(new BorderLayout)
  this.peer.add(textArea, BorderLayout.CENTER)
  
  /* ---- state variables ---- */
  var currentFile: Option[File] = None
  var editedSinceLastSave: Boolean = false
  var editedSinceLastAutoSave: Boolean = false

  val filenameLabel = new Label("[Untitled]")
  
  private val pathChangeListeners: Buffer[ChangeListener] = new ArrayBuffer

  /**
   * Create RSyntaxTextArea component. It provides features such as syntax highlighting and indentation.
   * The highlighting is based on a syntax specification AcumenTokenMakerMaker.xml which has been compiled
   * to the java class acumen.ui.tl.AcumenTokenMaker.java.
   * For more information, see https://github.com/effective-modeling/acumen/wiki/Acumen-development
   */
  def createSyntaxTextArea: RSyntaxTextArea = {
    val sta = new RSyntaxTextArea
    sta.setFont(new Font("Monospaced", Font.PLAIN, 12))
    TokenMakerFactory.getDefaultInstance.asInstanceOf[AbstractTokenMakerFactory].
      putMapping("AcumenTokenMaker", classOf[acumen.ui.tl.AcumenTokenMaker].getName)
    sta.setSyntaxEditingStyle("AcumenTokenMaker")
    val commentStyle = sta.getSyntaxScheme.getStyle(TokenTypes.COMMENT_EOL)
    commentStyle.font = commentStyle.font.deriveFont(Font.PLAIN)
    sta.setHighlightCurrentLine(false)
    sta.setTabSize(2)
    sta.setTabsEmulated(true) // Use soft tabs
    if (GraphicalMain.useCompletion) {
      val completionProvider = createCompletionProvider(sta)
      val autoCompletion = new AutoCompletion(completionProvider)
      autoCompletion setTriggerKey KeyStroke.getKeyStroke("TAB")
      autoCompletion install sta
    }
    if (GraphicalMain.useTemplates) {
      RSyntaxTextArea setTemplatesEnabled true
      createCodeTemplateManager
    }
    sta
  }

  def createCompletionProvider(textArea: RSyntaxTextArea) = {
    val cp = new DefaultCompletionProvider {
      // Make it possible to complete strings ending with .
      override def isValidChar(ch: Char) = super.isValidChar(ch) || ch=='.';
    }
    val style = textArea.getSyntaxEditingStyle
    val acumenTokenMakerSpec = XML.load(getClass.getClassLoader.getResourceAsStream("acumen/ui/tl/AcumenTokenMaker.xml"))
    val censored = List("Continuous", "Discrete", "rint")
    if (acumenTokenMakerSpec != null) { // Try to read keywords and functions from XML
      for (keyword <- acumenTokenMakerSpec \\ "keyword" if (!censored.contains(keyword.text)))
        cp.addCompletion(new BasicCompletion(cp, keyword.text))
      for (keyword <- acumenTokenMakerSpec \\ "function" if (!censored.contains(keyword.text)))
        cp.addCompletion(new BasicCompletion(cp, keyword.text))
    }
    cp.addCompletion(new BasicCompletion(cp, "Main"))
    cp.addCompletion(new BasicCompletion(cp, "simulator"))
    for (paramName <- Parameters.defaults.map(_._1))
      cp.addCompletion(new BasicCompletion(cp, "simulator." + paramName))
    cp
  }

  def createCodeTemplateManager =
    for (
      t <- List(
        ("class", "class ()\n  private  end\nend"),
        ("main", "class Main(simulator)\n  private  end\nend"),
        ("private", "private  end"),
        ("if", "if \n  \nend;"),
        ("switch", "switch \n  case \n    \nend;"),
        ("case", "case\n  "),
        ("hs", "class Main(simulator)\n  private mode := \"\"; end\n  switch mode\n    case \"\"\n      \n  end\nend"),
        ("mode", "case \"\"\n  if  mode := \"\" end;\n  "),
        ("event", "if  mode := \"\" end;\n"),
        ("ps", Parameters.defaults.map{case (p,v) => "simulator.%s := %s".format(p,v)}.mkString(";\n")))
    ) { RSyntaxTextArea.getCodeTemplateManager addTemplate new StaticCodeTemplate(t._1, t._2, null) }

  /* --- file handling ---- */

  def currentDir =
    currentFile match {
      case Some(f) => if (f.isDirectory) f else f.getParentFile
      case None    => Files.currentDir
    }

  def setCurrentFile(f: Option[File]) = {
    currentFile = f
    filenameLabel.text =
      currentFile match {
        case Some(f) => f.getName
        case None    => "[Untitled]"
      }
  }

  def setEdited = {
    if (!editedSinceLastSave) filenameLabel.text += " (unsaved)"
    editedSinceLastSave = true
    editedSinceLastAutoSave = true
  }

  def listenDocument = {
    textArea.getDocument.addDocumentListener(
      new DocumentListener {
        def changedUpdate(e: DocumentEvent) {}
        def insertUpdate(e: DocumentEvent) { setEdited }
        def removeUpdate(e: DocumentEvent) { setEdited }
      })
    textArea.discardAllEdits()
  }

  def newFile: Unit = withErrorReporting {
    preventWorkLoss {
      textArea.setText("")
      setCurrentFile(None)
      editedSinceLastSave = false
      textArea.discardAllEdits()
    }
  }

  def loadFile(file: File): Unit = {
    textArea.read(new FileReader(file), null)
    listenDocument
    setCurrentFile(Some(file))
    enableEditing
    editedSinceLastSave = false
    textArea setCaretPosition 0
  }

  def openFile(path: File): Unit = withErrorReporting {
    preventWorkLoss {
      val fc = new FileChooser(path)
      fc.peer.setFileSelectionMode(JFileChooser.FILES_AND_DIRECTORIES)
      fc.fileFilter = CodeArea.acumenFileFilter
      val returnVal = fc.showOpenDialog(App.ui.body)
      if (returnVal == FileChooser.Result.Approve) {
        if (fc.selectedFile.isFile)
          loadFile(fc.selectedFile)
        else { // Selected a directory => Clear current file
          textArea.setText("")
          setCurrentFile(Some(fc.selectedFile))
          editedSinceLastSave = false
          textArea.discardAllEdits()
        }
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
      case PROMPT_SAVE_AND_CONTINUE    => saveFile; true 
      case PROMPT_DISCARD_AND_CONTINUE => true
      case PROMPT_CANCEL               => false
    }
  }

  def saveFileAs: Unit = withErrorReporting {
    val fc = new FileChooser(currentDir)
    val returnVal = fc.showSaveDialog(App.ui.body)
    if (returnVal == FileChooser.Result.Approve) {
      val file = fc.selectedFile
      if (!file.exists || confirmSave(App.ui.body.peer, file)) {
        val writer = new FileWriter(fc.selectedFile)
        writer.write(textArea.getText)
        writer.close
        setCurrentFile(Some(file))
        editedSinceLastSave = false
        notifyPathChangeListeners
      }
    }
  }

  def saveFile: Unit = withErrorReporting {
    currentFile match {
      case Some(f) =>
        if (f.isDirectory)
          saveFileAs
        else {
          val writer = new FileWriter(f)
          writer.write(textArea.getText)
          writer.close
          setCurrentFile(currentFile)
          editedSinceLastSave = false
          notifyPathChangeListeners
        }
      case None => saveFileAs
    }
  }

  def autoSave = withErrorReporting {
    if (editedSinceLastAutoSave) {
      val file = Files.getFreshFile
      val writer = new FileWriter(file)
      writer.write(textArea.getText)
      writer.close
    }
  }

  def withErrorReporting(action: => Unit) = App.ui.withErrorReporting(action)

  /* Listen to simulation state changes. When running, the editor is disabled. */
  listenTo(App.pub)
  reactions += {
    case st: App.State =>
      st match {
        case App.Stopped => enableEditing
        case _           => disableEditing
      }
  }

  /* Listen for selection events in FileTree browser. */
  def valueChanged(e: TreeSelectionEvent) {
    if (GraphicalMain.syncEditorWithBrowser) {
      val lpc = e.getPath.getLastPathComponent
      val file =
        if (lpc.isInstanceOf[TreeFile]) lpc.asInstanceOf[TreeFile]
        else lpc.asInstanceOf[File]
      if (file.isFile) preventWorkLoss(loadFile(file))
    }
  }

  def enableEditing = {
    textArea setEnabled (true)
    textArea setSyntaxEditingStyle (currentFile match {
      case Some(f) if f.getName.endsWith(FILE_SUFFIX_MODEL) => "AcumenTokenMaker"
      case None => "AcumenTokenMaker" // Just started Acumen, assume we are editing a model
      case _ => SyntaxConstants.SYNTAX_STYLE_NONE
    })
    textArea setForeground Color.black
  }

  def disableEditing = {
    textArea setEnabled (false)
    textArea setSyntaxEditingStyle SyntaxConstants.SYNTAX_STYLE_NONE
    textArea setForeground Color.gray
  }

  if (GraphicalMain.openFile != null) {
    try {
      loadFile(GraphicalMain.openFile)
      notifyPathChangeListeners
    }
    catch {
      case e =>
        System.err.println("Unable To Open File: ")
        System.err.println("  " + e)
        exit(2)
    }
  }

  /* Font management */

  def setFontName(n: String) =
    textArea.setFont(new Font(n, textArea.getFont getStyle, textArea.getFont getSize))

  val supportedFonts =
    DEFAULT_FONT_NAME +: GraphicsEnvironment.getLocalGraphicsEnvironment.getAvailableFontFamilyNames.filter(KNOWN_FONTS contains _.toLowerCase)

  private def setFontSize(size: Int) =
    textArea.setFont(new Font(textArea.getFont getName, textArea.getFont getStyle, size))

  def increaseFontSize = setFontSize(textArea.getFont.getSize + 2)

  def decreaseFontSize = if (textArea.getFont.getSize > 3) setFontSize(textArea.getFont.getSize - 2)

  def resetFontSize = setFontSize(DEFAULT_FONT_SIZE)

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

