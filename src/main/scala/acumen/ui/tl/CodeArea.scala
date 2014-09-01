package acumen
package ui
package tl

import java.awt.Color
import java.awt.Font
import java.awt.GraphicsEnvironment
import java.awt.event.ActionEvent
import java.awt.event.ActionListener
import java.awt.event.KeyEvent.{ VK_C, VK_CLOSE_BRACKET, VK_OPEN_BRACKET }
import java.io.{
  File, FileReader, FileWriter
}
import scala.Array.canBuildFrom
import scala.collection.mutable.{
  ArrayBuffer, Buffer
}
import scala.swing.{
  FileChooser, Label, Panel
}
import scala.xml.XML
import org.fife.ui.autocomplete.{
  AutoCompletion, BasicCompletion, DefaultCompletionProvider
}
import org.fife.ui.rsyntaxtextarea.{
  AbstractTokenMakerFactory, RSyntaxTextArea, RSyntaxTextAreaEditorKit,
  SyntaxConstants, TokenMakerFactory, TokenTypes
}
import org.fife.ui.rsyntaxtextarea.templates.StaticCodeTemplate
import org.fife.ui.rtextarea.{
  SearchContext, SearchEngine
}
import org.fife.ui.rsyntaxtextarea.TokenTypes.{
  COMMENT_DOCUMENTATION, COMMENT_EOL, COMMENT_KEYWORD, COMMENT_MARKUP, COMMENT_MULTILINE
}
import acumen.Main
import acumen.interpreters.enclosure.Parameters
import acumen.ui.App
import acumen.ui.Files
import acumen.util.System.FILE_SUFFIX_MODEL
import javax.swing._
import javax.swing.event._
import java.util.concurrent.atomic.AtomicBoolean
import javax.swing.text.BadLocationException
import java.awt.Point
import java.awt.event.InputEvent

class CodeArea extends Panel with TreeSelectionListener {

  val textArea = createSyntaxTextArea
  val searchField = new JTextField(30)
  val findReplaceToolBar = createFindReplaceToolbar
  
  val searchFieldFlasher = new SwingUtil.Flasher()
  
  val DEFAULT_FONT_SIZE = 12
  val DEFAULT_FONT_NAME = Font.MONOSPACED
  val KNOWN_FONTS = Set("courier new", "courier", "monaco", "consolas", "lucida console",
    "andale mono", "profont", "monofur", "proggy", "droid sans mono",
    "lettergothic", "bitstream vera sans mono", "crystal", "ubuntu monospace")

  val PROMPT_CANCEL = "Cancel"
  val PROMPT_SAVE_AND_CONTINUE = "Save and continue"
  val PROMPT_DISCARD_AND_CONTINUE = "Discard and continue"
    
  /* ---- state variables ---- */
  var currentFile: Option[File] = None
  var editedSinceLastSave: Boolean = false
  var editedSinceLastAutoSave: Boolean = false
  var editedSinceLastRun: Boolean = false

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
    for { // Make all comment fonts plain (not italic)
      commentType <- List(COMMENT_DOCUMENTATION, COMMENT_EOL, COMMENT_KEYWORD, COMMENT_MARKUP, COMMENT_MULTILINE)
      commentStyle = sta.getSyntaxScheme getStyle commentType
    } { commentStyle.font = commentStyle.font deriveFont Font.PLAIN }
    sta.setHighlightCurrentLine(false)
    sta.setTabSize(2)
    sta.setTabsEmulated(true) // Use soft tabs
    updateCompletionProvider(sta, Main.defaultSemantics.interpreter)
    if (Main.useTemplates) {
      RSyntaxTextArea setTemplatesEnabled true
      createCodeTemplateManager
    }
    sta
  }
  
  def updateCompletionProvider(i: Interpreter): Unit = if (textArea != null) updateCompletionProvider(textArea, i)
  
  private def updateCompletionProvider(ta: RSyntaxTextArea, i: Interpreter) = if (Main.useCompletion) {
    val completionProvider = createCompletionProvider(ta, i)
    val autoCompletion = new AutoCompletion(completionProvider)
    autoCompletion setTriggerKey KeyStroke.getKeyStroke("TAB")
    autoCompletion install ta
  }

  def createCompletionProvider(textArea: RSyntaxTextArea, i: Interpreter) = {
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
    for (p <- i.visibleParameters.keys) cp.addCompletion(new BasicCompletion(cp, "simulator." + p))
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
    notifyPathChangeListeners
  }

  def setEdited = {
    if (!editedSinceLastSave) filenameLabel.text += " (unsaved)"
    editedSinceLastSave = true
    editedSinceLastAutoSave = true
    editedSinceLastRun = true
  }

  def listenDocument = {
    textArea.getDocument.addDocumentListener(
      new DocumentListener {
        def changedUpdate(e: DocumentEvent) { textArea.removeAllLineHighlights }
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
      textArea.discardAllEdits
      textArea.requestFocus
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
      fc.fileFilter = CodeArea.acumenFileFilter
      val returnVal = fc.showOpenDialog(App.ui.body)
      if (returnVal == FileChooser.Result.Approve) {
        if (fc.selectedFile.isFile) {
          loadFile(fc.selectedFile)
          notifyPathChangeListeners
        }
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
      val f = fc.selectedFile
      if (!f.exists || confirmSave(App.ui.body.peer, f))
        writeText(f, updateCurrentFile)
      setCurrentFile(Some(f))
    }
  }

  /** Write editor text contents to file, optionally updating the current file reference. */
  private def writeText(file: File, updateCurrentFile: Boolean) = {
    val writer = new FileWriter(file)
    writer.write(textArea.getText)
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
    if (Main.synchEditorWithBrowser) {
      val lpc = e.getPath.getLastPathComponent
      val file =
        if (lpc.isInstanceOf[TreeFile]) lpc.asInstanceOf[TreeFile]
        else lpc.asInstanceOf[File]
      if (file.isFile && !(currentFile.isDefined && currentFile.get.getAbsolutePath == file.getAbsolutePath))
        preventWorkLoss(loadFile(file))
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

  if (Main.openFile != null) {
    loadFile(Main.openFile)
    notifyPathChangeListeners
  }
  
  /** Find and replace tool bar (currently supports only find). */
  def createFindReplaceToolbar: JToolBar = {
    val toolBar = new JToolBar()
    val matchCaseCB = new JCheckBox("Case Sensitive")
    val regexCB = new JCheckBox("RegEx")
    val nextButton = new JButton(">")
    val prevButton = new JButton("<")
    searchField.addActionListener(new ActionListener{
      def actionPerformed(e: ActionEvent) = nextButton doClick 0 
    })
    nextButton setActionCommand "FindNext"
    prevButton setActionCommand "FindPrevious"
    val findActionListener = new ActionListener {
      def actionPerformed(e: ActionEvent) {
        val command = e.getActionCommand
        val forward = "FindNext" equals command
        val searchContext = new SearchContext()
        val searchText = searchField.getText
        if (searchText.nonEmpty) {
          searchContext setSearchFor searchText
          searchContext setMatchCase matchCaseCB.isSelected
          searchContext setRegularExpression regexCB.isSelected
          searchContext setSearchForward forward
          searchContext setWholeWord false
          val searchResult = SearchEngine.find(textArea, searchContext)
          if (!searchResult.wasFound)
            SwingUtil.flashFunction(
              searchField.setBackground, Color.WHITE, new Color(255,128,128), searchFieldFlasher)
        }
      }
    }
    nextButton addActionListener findActionListener
    prevButton addActionListener findActionListener
    toolBar add new JLabel("Find")
    toolBar add searchField
    toolBar add prevButton
    toolBar add nextButton
    toolBar add matchCaseCB
    toolBar add regexCB
    toolBar setVisible false
    toolBar setFloatable false
    toolBar
  }
  
  /** Center textArea viewport on current caret position. */
  def centerLineInScrollPane() {
    try {
      val container = SwingUtilities.getAncestorOfClass(classOf[JViewport], textArea)
      val r = textArea.modelToView(textArea.getCaretPosition)
      val viewport = container.asInstanceOf[JViewport]
      val extentHeight = viewport.getExtentSize.height
      val viewHeight = viewport.getViewSize.height
      val y = Math.min(Math.max(0, r.y - (extentHeight / 2)), viewHeight - extentHeight)
      viewport.setViewPosition(new Point(0, y))
    } catch { case ble: BadLocationException => }
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

  /* Indentation */
  
  def increaseIndent() = textArea.getActionMap.get("increaseIndentAction").actionPerformed(null)
  def decreaseIndent() = textArea.getActionMap.get("decreaseIndentAction").actionPerformed(null)

  textArea.getInputMap.put(KeyStroke.getKeyStroke(VK_OPEN_BRACKET, util.System.shortcutMask), "decreaseIndentAction")
  textArea.getInputMap.put(KeyStroke.getKeyStroke(VK_CLOSE_BRACKET, util.System.shortcutMask), "increaseIndentAction")
  textArea.getActionMap.put("increaseIndentAction", new RSyntaxTextAreaEditorKit.InsertTabAction())
  textArea.getActionMap.put("decreaseIndentAction", new RSyntaxTextAreaEditorKit.DecreaseIndentAction())
  
  /* Copy code as RTF (syntax highlighted) */
  
  textArea.getInputMap.put(KeyStroke.getKeyStroke(VK_C, util.System.shortcutMask), "copyAsRTFAction")
  textArea.getActionMap.put("copyAsRTFAction", new RSyntaxTextAreaEditorKit.CopyAsRtfAction())
  
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

