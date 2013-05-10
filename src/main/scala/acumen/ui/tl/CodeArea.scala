package acumen
package ui
package tl

import java.awt.event.ActionEvent
import java.awt.BorderLayout
import java.awt.Font
import java.awt.GraphicsEnvironment
import java.io._
import java.util.HashSet
import scala.collection.JavaConversions._
import scala.swing._
import scala.xml.XML
import org.fife.ui.autocomplete.AutoCompletion
import org.fife.ui.autocomplete.BasicCompletion
import org.fife.ui.autocomplete.DefaultCompletionProvider
import org.fife.ui.rsyntaxtextarea.templates.StaticCodeTemplate
import org.fife.ui.rsyntaxtextarea.AbstractTokenMakerFactory
import org.fife.ui.rsyntaxtextarea.RSyntaxTextArea
import org.fife.ui.rsyntaxtextarea.SyntaxConstants
import org.fife.ui.rsyntaxtextarea.TokenMakerFactory
import org.fife.ui.rsyntaxtextarea.TokenTypes
import acumen.ui.App
import acumen.ui.Files
import acumen.util.System.FILE_SUFFIX_MODEL
import javax.swing.event.DocumentEvent
import javax.swing.event.DocumentListener
import javax.swing.text._
import javax.swing.undo._
import javax.swing.JOptionPane
import javax.swing.KeyStroke
import java.awt.Color


class CodeArea extends Panel { //EditorPane {

  val textArea = createSyntaxTextArea

  val DEFAULT_FONT_SIZE = 12
  val DEFAULT_FONT_NAME = Font.MONOSPACED
  val KNOWN_FONTS = Set("courier new", "courier", "monaco", "consolas", "lucida console", 
		  				"andale mono", "profont", "monofur", "proggy", "droid sans mono", 
		  			    "lettergothic", "bitstream vera sans mono", "crystal", "ubuntu monospace")
  
  this.peer.setLayout(new BorderLayout)
  this.peer.add(textArea,BorderLayout.CENTER)
  
  /* ---- state variables ---- */
  var currentFile : Option[File] = None
  var editedSinceLastSave : Boolean = false
  var editedSinceLastAutoSave : Boolean = false

  val filenameLabel = new Label("[Untitled]")
  val acumenFileFilter = new javax.swing.filechooser.FileFilter() {
    val acumenFileName = """.*\.acm|README""".r 
    def accept(f: File): Boolean = 
      if (f isDirectory) true
      else acumenFileName.pattern.matcher(f getName).matches
    def getDescription = ".acm and README files"
  }
  
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
      autoCompletion install sta
    }
    if (GraphicalMain.useTemplates) {
      RSyntaxTextArea setTemplatesEnabled true
      createCodeTemplateManager
    }
    sta
  }
  
  def createCompletionProvider(textArea: RSyntaxTextArea) = {
    val cp = new DefaultCompletionProvider
    val style = textArea.getSyntaxEditingStyle
    val acumenTokenMakerSpec = XML.load(getClass.getClassLoader.getResourceAsStream("acumen/ui/tl/AcumenTokenMaker.xml"))
    if (acumenTokenMakerSpec != null) { // Try to read keywords and functions from XML
      for (val keyword <- acumenTokenMakerSpec \\ "keyword")
        cp.addCompletion(new BasicCompletion(cp, keyword.text))
      for (val keyword <- acumenTokenMakerSpec \\ "function")
        cp.addCompletion(new BasicCompletion(cp, keyword.text))
    } // If this is unsuccessful, add the reserved words specified in the parser
    else for (k <- Parser.lexical.reserved) cp.addCompletion(new BasicCompletion(cp, k))
    cp.addCompletion(new BasicCompletion(cp, "simulator"))
    cp
  }

  def createCodeTemplateManager = 
    for (t <- List(
 	  ("class",   "class ()\n  private  end\nend"),
      ("main",    "class Main(simulator)\n  private  end\nend"),
 	  ("private", "private  end"),
      ("if",      "if \n  \nend;"),
      ("switch",  "switch \n  case \n    \nend;"),
      ("case",    "case\n  "),
      ("hs",      "class Main(simulator)\n  private mode := \"\"; end\n  switch mode\n    case \"\"\n      \n  end\nend"),
      ("mode",    "case \"\"\n  if  mode := \"\" end;\n  "),
      ("event",   "if  mode := \"\" end;\n"),
      ("ps",      "simulator.endTime := 3;\nsimulator.minSolverStep := 0.01;\nsimulator.minLocalizationStep := 0.001;\nsimulator.minComputationImprovement := 0.0001;")
    )) { RSyntaxTextArea.getCodeTemplateManager addTemplate new StaticCodeTemplate(t._1, t._2, null) }
  
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
    textArea.getDocument.addDocumentListener(
      new DocumentListener {
        def changedUpdate(e:DocumentEvent) {}
        def insertUpdate(e:DocumentEvent) { setEdited }
        def removeUpdate(e:DocumentEvent) { setEdited }
      })
    textArea.discardAllEdits()
  }

  def newFile : Unit = withErrorReporting {
    if (!editedSinceLastSave || confirmContinue(App.ui.body.peer)) {
      textArea.setText("")
      setCurrentFile(None)
      editedSinceLastSave = false
      textArea.discardAllEdits()
    }
  }

  def loadFile(file: File) : Unit = {
    textArea.read(new FileReader(file),null)
    listenDocument
    setCurrentFile(Some(file))
    enableEditing
    editedSinceLastSave = false
  }

  def openFile(path: File) : Unit = withErrorReporting {
    if (!editedSinceLastSave || confirmContinue(App.ui.body.peer)) {
      val fc = new FileChooser(path)
      fc.fileFilter = acumenFileFilter
      val returnVal = fc.showOpenDialog(App.ui.body)
      if (returnVal == FileChooser.Result.Approve) {
        loadFile(fc.selectedFile)
        textArea setCaretPosition 0
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
        writer.write(textArea.getText)
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
        writer.write(textArea.getText)
        writer.close
        setCurrentFile(currentFile)
        editedSinceLastSave = false
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

  listenTo(App.pub)
  reactions += {
    case st:App.State => 
      st match {
        case App.Stopped => enableEditing
        case _           => disableEditing
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
    } catch {
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
    
}
