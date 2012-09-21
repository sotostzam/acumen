package acumen
package ui

import java.lang.Thread
import collection.JavaConversions._

import java.awt.Font
import java.awt.Color
import java.awt.RenderingHints
import java.awt.GraphicsEnvironment
import java.awt.Desktop
import javax.swing.JOptionPane
import javax.swing.SwingUtilities
import javax.swing.event.DocumentListener
import javax.swing.event.DocumentEvent
import java.io._
import swing._
import swing.event._

 

object GraphicalMain extends SimpleSwingApplication {

  /* ---- definitions ------ */

  val monospaced = new Font("Monospaced", Font.PLAIN, 12) 

  val play = new Action("play") {
    icon = Icons.play
    def apply = { autoSave;  appModel.play }
    toolTip = "Run Simulation"
  }
  val step = new Action("step") {
    icon = Icons.step
    def apply = { autoSave; appModel.step }
    toolTip = "Compute one simulation step"
  }
  val pause = new Action("pause") {
    icon = Icons.pause
    def apply = appModel.pause
    toolTip = "Pause simulation"
  }
  val stop = new Action("stop") {
    icon = Icons.stop
    def apply = appModel.stop
    toolTip = "Stop simulation (cannot resume)"
  }

  /* ---- state variables ---- */

  val appModel = new AppModel(codeArea.text)
  var currentFile : Option[File] = None
  var editedSinceLastSave : Boolean = false
  var editedSinceLastAutoSave : Boolean = false
	var lastNumberOfThreads = 2

  /* ----- UI setup ------- */

  /* 1. left pane */
  /* 1.1 upper pane */
  val bPlay = new Button(play) { peer.setHideActionText(true) }
  val bStep = new Button(step) { peer.setHideActionText(true) }
  val bStop = new Button(stop) { peer.setHideActionText(true) }
  val upperButtons = 
    new FlowPanel(FlowPanel.Alignment.Leading)(bPlay, bStep, bStop)
  val codeArea = new EditorPane {
    font = monospaced
  }
  val statusZone = new StatusZone
  val upperBottomPane = new BoxPanel(Orientation.Horizontal) {
    contents += upperButtons
    contents += statusZone
  }
  val filenameLabel = new Label("[Untitled]")
  val upperPane = new BorderPanel {
    add(filenameLabel, BorderPanel.Position.North)
    add(new ScrollPane(codeArea), BorderPanel.Position.Center)
    add(upperBottomPane,  BorderPanel.Position.South) 
  }
  
  /* 1.2 lower pane */
  val console = new Console

  val lowerPane = new BorderPanel {
    add(new Label("Console"), BorderPanel.Position.North)
    add(new ScrollPane(console), BorderPanel.Position.Center)
  }

  val leftPane = 
    new SplitPane(Orientation.Horizontal, upperPane, lowerPane) { 
      oneTouchExpandable = true
      resizeWeight = 0.9
    }

  val traceModel = appModel.tmodel
  //val traceModel = new IntervalTraceModel(appModel.tmodel)
  //val traceModel = new FakeEnclosureTraceModel(appModel.tmodel)

  /* 2 right pane */
  val traceTable = new Table { 
    model = traceModel
    autoResizeMode = Table.AutoResizeMode.Off
  }

  val traceView = new TraceView(false, false, false, traceModel)
  val pointedView = new PointedView(traceView)

  val tab1 = new BorderPanel {
    add(new FlowPanel(FlowPanel.Alignment.Leading)(pointedView), 
        BorderPanel.Position.North)
    add(traceView, BorderPanel.Position.Center)
  }
  val tab2 = new ScrollPane(traceTable)
  val rightPane = new TabbedPane {
    pages ++= List(new TabbedPane.Page("Plot", tab1), 
                   new TabbedPane.Page("Trace", tab2))
  }


  /* main component */
  val body = 
    new SplitPane(Orientation.Vertical, leftPane, rightPane) { 
      oneTouchExpandable = true
      resizeWeight = 0.2
    }

  /* menu bar */
 
  val bar = new MenuBar {
    contents += new Menu("File") {
      mnemonic = Key.F
      contents += new MenuItem(Action("New")({ newFile })) 
                      { mnemonic = Key.N }
      contents += new MenuItem(Action("Open")({ openFile(currentDir) })) 
                      { mnemonic = Key.O }
      contents += new MenuItem(Action("Save")(saveFile))
                      { mnemonic = Key.S }
      contents += new MenuItem(Action("Save As")(saveFileAs))
                      { mnemonic = Key.A }
      contents += new MenuItem(Action("Recover")({ openFile(Files.autoSavedDir) }))
                      { mnemonic = Key.R }
      contents += new MenuItem(Action("Exit")(exit))
                      { mnemonic = Key.E }
    }

    contents += new Menu("Plotting") {
      mnemonic = Key.P
      contents += new Menu("Style") {
        val rb1 = new RadioMenuItem("") {
          selected = true
          action = Action("Lines") { traceView.setPlotStyle(Lines()) }
        }
        val rb2 = new RadioMenuItem("") {
          selected = false
          action = Action("Dots") { traceView.setPlotStyle(Dots()) }
        }
        val rb3 = new RadioMenuItem("") {
          selected = false
          action = Action("Both") { traceView.setPlotStyle(Both()) }
        }
        contents ++= Seq(rb1,rb2,rb3)
        new ButtonGroup(rb1,rb2,rb3)
      }
      contents += new CheckMenuItem("") {
        selected = false
        action = Action("Automatically plot simulator fields") { 
					traceView.toggleSimulator(this.selected) 
				}
      }
      contents += new CheckMenuItem("") {
        selected = false
        action = Action("Automatically plot child counter fields") { 
					traceView.toggleNextChild(this.selected) 
				}
      }
      contents += new CheckMenuItem("") {
        selected = false
        action = Action("Automatically random number generator seeds") { 
					traceView.toggleSeeds(this.selected)
				}
      }
    }

    contents += new Menu("Semantics") {
      mnemonic = Key.S
      val rb1 = new RadioMenuItem("") {
        selected = true // This is the default semantics
        action = Action("Purely Functional") { appModel.setInterpreter(interpreters.reference.Interpreter) }
      }
      val rb2 = new RadioMenuItem("") {
        selected = false
        action = Action("Imperative (Parallel)") {
          def diag = Dialog.showInput(
            body, "Choose a number of threads",
            "Parallel Interpreter", Dialog.Message.Question,
            Swing.EmptyIcon, Seq(), lastNumberOfThreads.toString)
          def go: Unit = try {
            def n: String = diag.getOrElse(n)
            lastNumberOfThreads = Integer.parseInt(n)
            appModel.setInterpreter(new interpreters.parallel.Interpreter(lastNumberOfThreads))
            console.log("Number of threads set to " + lastNumberOfThreads + ".")
          } catch {
            case _ =>
              console.logError("Bad number of threads.")
              go
          }
          go
        }
      }
      val rb3 = new RadioMenuItem("") {
        selected = false 
        action = Action("Enclosure") { appModel.setInterpreter(interpreters.enclosure.Interpreter) }
      }
      contents ++= Seq(rb1,rb2,rb3)
      new ButtonGroup(rb1,rb2,rb3)
    }
   
    contents += new Menu("Help") {
      mnemonic = Key.H
      contents += new MenuItem(Action("Tutorial")(tutorial))
                      { mnemonic = Key.T }
      contents += new MenuItem(Action("About")(about))
                      { mnemonic = Key.A }
    }
  }
 
  /* gluing everything together */
  def top = new MainFrame {
    title = "Acumen"
    contents = body
    menuBar = bar
    size = new Dimension(1024,768)
  }

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
    codeArea.peer.getDocument.addDocumentListener(
      new DocumentListener {
        def changedUpdate(e:DocumentEvent) { setEdited }
        def insertUpdate(e:DocumentEvent) { setEdited }
        def removeUpdate(e:DocumentEvent) { setEdited }
      })

  def newFile : Unit = withErrorReporting {
    if (!editedSinceLastSave || confirmContinue(body.peer)) {
      codeArea.text = ""
      listenDocument
      setCurrentFile(None)
      editedSinceLastSave = false
      appModel.reset
    }
  }

  def openFile(path: File) : Unit = withErrorReporting {
    if (!editedSinceLastSave || confirmContinue(body.peer)) {
      val fc = new FileChooser(path)
      val returnVal = fc.showOpenDialog(body)
      if (returnVal == FileChooser.Result.Approve) {
        val file = fc.selectedFile
        codeArea.peer.setPage(file.toURI.toString)
        listenDocument
        setCurrentFile(Some(file))
        editedSinceLastSave = false
        appModel.reset
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
    val returnVal = fc.showSaveDialog(body)
    if (returnVal == FileChooser.Result.Approve) {
      val file = fc.selectedFile
      if (!file.exists || confirmSave(body.peer, file)) {
        val writer = new FileWriter(fc.selectedFile)
        writer.write(codeArea.text)
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
        writer.write(codeArea.text)
        writer.close
        editedSinceLastSave = false
      case None => saveFileAs
    }
  }

  def exit : Unit = withErrorReporting {
    if (!editedSinceLastSave || confirmContinue(body.peer)) quit
  }

  def withErrorReporting(action: => Unit) : Unit = {
    try action 
    catch { case e => reportError(e) }
  }

  def reportError(e:Throwable) {
    val em = e.getMessage
    console.logError(if (em!=null) em else e.toString)
  }

  def autoSave = withErrorReporting {
    if (editedSinceLastAutoSave) {
      val file = Files.getFreshFile
      val writer = new FileWriter(file)
      writer.write(codeArea.text)
      writer.close
    }
  }
 
  def redraw = traceView.redraw

  /* ------ simple dialogs ----- */

  def about = { 
    val version = acumen.util.System.version
    Dialog.showMessage(body, "Acumen " + version, "About")
  }
  def tutorial = {
    if (Desktop.isDesktopSupported) {
      val desktop = Desktop.getDesktop
      try { desktop.browse(acumen.util.System.tutorialUrl) } 
      catch { case e:IOException => reportError(e) }
    } else reportError(new Exception("Could not find a web browser."))
  }

  /* ----- events handling ---- */
  
  listenTo(appModel)

  def updateButtons = {
    play.enabled = appModel.playEnabled
    if (appModel.isEnclosure) {
      pause.enabled = false
      stop.enabled = false
      step.enabled = false
    } else {
      pause.enabled = appModel.pauseEnabled 
      stop.enabled = appModel.stopEnabled 
      step.enabled = appModel.stepEnabled 
    }
  }

  def reflectState = {
    updateButtons
    codeArea.enabled = appModel.codeEnabled

    appModel.state match {
      case Stopped() =>
        bPlay.action = play
        console.log("Stopped.")
        console.newLine
      case Paused() =>
        bPlay.action = play
        console.log("Paused. ")
      case Playing() =>
        bPlay.action = pause
        console.log("Running...")
    }
  }

  reactions += {
    case InterpreterChanged() => updateButtons
    case StateChanged() => reflectState
    case Error(e)       => reportError(e)
    case Progress(p)    => statusZone.setProgress(p)
    case ProgressMsg(m) => console.log(m); console.newLine
  }

  /* ----- initialisation ----- */

  reflectState
  listenDocument
  console.log("<html>Welcome to Acumen.<br/>"+
              "Please see LICENSE file for licensing details.</html>")
  console.newLine
  appModel.reset
}

