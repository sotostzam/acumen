package acumen
package ui

import tl._
import interpreter._
import java.lang.Thread
import scala.actors._
import collection.JavaConversions._
import java.awt.Font
import java.awt.Color
import java.awt.RenderingHints
import java.awt.GraphicsEnvironment
import java.awt.Desktop
import java.awt.Font
import java.awt.GraphicsEnvironment
import java.awt.KeyboardFocusManager
import java.awt.KeyEventDispatcher
import java.awt.RenderingHints
import java.awt.Toolkit
import java.awt.event.KeyEvent
import java.awt.event.KeyEvent._
import java.awt.event.InputEvent._
import java.io._
import javax.swing.AbstractAction
import javax.swing.JCheckBox
import javax.swing.JOptionPane
import javax.swing.JToolBar
import javax.swing.SwingUtilities
import javax.swing.undo._
import javax.swing.text._
import javax.swing.KeyStroke
import javax.swing.event.DocumentListener
import javax.swing.event.DocumentEvent
import org.fife.ui.rtextarea.RTextScrollPane
import swing.{Action, BorderPanel, BoxPanel, ButtonGroup, CheckMenuItem, 
			  Component, Dialog, Dimension, FileChooser, FlowPanel, Label, 
			  Menu, MainFrame, MenuBar, MenuItem, Orientation, Publisher, RadioMenuItem, 
			  ScrollPane, Separator, SimpleSwingApplication, SplitPane, Swing, 
			  TabbedPane, Table}
import swing.event._
import scala.Boolean
import acumen.interpreters.{reference,newreference,enclosure,imperative}
import acumen.interpreters.enclosure.ivp.PicardSolver
import acumen.interpreters.enclosure.ivp.LohnerSolver
import acumen.interpreters.enclosure.event.pwl.PWLEventEncloser
import acumen.interpreters.enclosure.event.pwl.PWLEventEncloser
import acumen.interpreters.enclosure.event.tree.TreeEventEncloser

// class Acumen = Everything that use to be GraphicalMain.  Graphical
// Main can't be an object it will cause Swing components to be
// initialized outside of the Event Dispatcher Thread (EDT).  Thus 
// we make it a seperate class and have GraphicalMain top() method
// initialize it

class App extends SimpleSwingApplication {
  import App._

  App.ui = this
  
  val DEFAULT_HEIGHT = 768
  val DEFAULT_WIDTH = 1024

  private val cores = Runtime.getRuntime.availableProcessors
  
  // Create a special actor to listen to events from other threads

  case object EXIT
  val actor = new Actor with Publisher {
    override val scheduler = new SchedulerAdapter {
      def execute(fun: => Unit) { Swing.onEDT(fun) }
    }

    def act() {
      Supervisor.watch(this, "Main UI", { restart })
      loop {
        react {
          case Error(e) => reportError(e)
          case Progress(p) => statusZone.setProgress(p)
          case ProgressMsg(m) =>
            console.log(m); console.newLine
          case Progress3d(p) => threeDtab.setProgress(p)

          case EXIT => exit

          case SendInit => controller ! Init(codeArea.textArea.getText, interpreter)

          case msg: Event =>
            //println("Publishing This Msg: " + msg)
            publish(msg)

          case msg => println("Unknown Msg in GM: " + msg)
        }
      }
    }
  }

  App.actor = actor
  App.pub = actor

  /* ---- state variables ---- */
  val controller = new Controller
  var lastNumberOfThreads = 2

  //**************************************
  //**************************************

  /* ----- UI setup ------- */
  
  private val NONE = VK_UNDEFINED
  /* Reusable actions */
  private val playAction                    = mkAction(    "Run",                       VK_R, VK_G,       upperButtons.bPlay.doClick)
  private val pauseAction                   = mkAction(    "Pause",                     VK_R, VK_G,       upperButtons.bPlay.doClick)
  private val stepAction                    = mkAction(    "Step",                      VK_T, VK_B,       upperButtons.bStep.doClick)
  private val stopAction                    = mkAction(    "Stop",                      VK_S, VK_T,       upperButtons.bStop.doClick)
  private val newAction                     = mkAction(    "New",                       VK_N, VK_N,       codeArea.newFile)
  private val openAction                    = mkAction(    "Open",                      VK_O, VK_O,       codeArea.openFile(codeArea.currentDir))
  private val saveAction                    = mkAction(    "Save",                      VK_S, VK_S,       codeArea.saveFile())
  private val saveAsAction                  = mkActionMask("Save As",                   VK_A, VK_S,       shortcutMask | SHIFT_MASK, codeArea.saveFileAs())
  private val recoverAction                 = mkAction(    "Recover",                   VK_R, VK_R,       codeArea.openFile(Files.autoSavedDir))
  private val exportTableAction           = new Action(  "Export Table"){mnemonic = VK_E; def apply = exportTable}
  private val exitAction                    = mkAction(    "Exit",                      VK_E, VK_Q,       exit)
  private val cutAction                     = mkAction(    "Cut",                       VK_T, VK_X,       codeArea.textArea.cut)
  private val copyAction                    = mkAction(    "Copy",                      VK_C, VK_C,       codeArea.textArea.copyAsRtf)
  private val pasteAction                   = mkAction(    "Paste",                     VK_P, VK_V,       codeArea.textArea.paste)
  private val selectAllAction               = mkAction(    "Select All",                VK_A, VK_A,       codeArea.textArea.selectAll)
  private val increaseFontSizeAction        = mkAction(    "Enlarge Font",              VK_I, VK_PLUS,    codeArea increaseFontSize)
  private val decreaseFontSizeAction        = mkAction(    "Reduce Font",               VK_D, VK_MINUS,   codeArea decreaseFontSize)
  private val resetFontSizeAction           = mkAction(    "Reset Font",                VK_R, VK_0,       codeArea resetFontSize)
  private val showLineNumbersAction         = mkAction(    "Line Numbers",              VK_L, VK_L,       toggleLineNumbers)
  private val plotStyleLinesAction          = new Action(  "Lines")      { mnemonic =   VK_L; def apply = plotView.setPlotStyle(plot.Lines()) }
  private val plotStyleDotsAction           = new Action(  "Dots")       { mnemonic =   VK_D; def apply = plotView.setPlotStyle(plot.Dots()) }
  private val plotStyleBothAction           = new Action(  "Both")       { mnemonic =   VK_B; def apply = plotView.setPlotStyle(plot.Both()) }
  private val floatingPointNewAction        = mkActionMask("Traditional Functional 2",  VK_2, NONE,       shortcutMask | SHIFT_MASK, setInterpreter("newreference"))
  private val floatingPointAction           = mkActionMask("Traditional Functional",    VK_F, VK_R,       shortcutMask | SHIFT_MASK, setInterpreter("reference"))
  private val floatingPointImperativeAction = mkActionMask("Traditional Imparative",    VK_I, VK_I,       shortcutMask | SHIFT_MASK, setInterpreter("imperative")) 
  private val floatingPointParallelAction   = mkActionMask("Traditional Parallel",      VK_P, VK_P,       shortcutMask | SHIFT_MASK, promptForNumberOfThreads)
  private val pwlHybridSolverAction         = mkActionMask("Enclosure PWL",             VK_L, VK_L,       shortcutMask | SHIFT_MASK, setInterpreter("enclosure-pwl")) 
  private val eventTreeHybridSolverAction   = mkActionMask("Enclosure EVT",             VK_T, VK_T,       shortcutMask | SHIFT_MASK, setInterpreter("enclosure-evt"))
  private val contractionAction             = mkActionMask("Contraction",               VK_C, VK_C,       shortcutMask | SHIFT_MASK, enclosure.Interpreter.toggleContraction)
  private val tutorialAction                = mkAction(    "Core Acumen Tutorial",      VK_T, VK_F1,      tutorial)
  private val aboutAction                   = new Action(  "About")      { mnemonic =   VK_A; def apply = about }
  
  /* Shows a dialog asking the user how many threads to use in the parallel interpreter. */
  private def promptForNumberOfThreads = {
    def diag = Dialog.showInput(
      body, "Choose a number of threads",
      "Parallel Interpreter", Dialog.Message.Question,
      Swing.EmptyIcon, Seq(), lastNumberOfThreads.toString)
    def go: Unit = try {
      def n: String = diag.getOrElse(n)
      val userNumberOfThreads = Integer.parseInt(n)
      lastNumberOfThreads = if (userNumberOfThreads > cores) {
        console.log("Number of threads set to " + cores + ", the number of cores available to Acumen.\n")
        cores
      } else {
        console.log("Number of threads set to " + userNumberOfThreads + ".\n")
        userNumberOfThreads
      }
      setInterpreter("parallel", lastNumberOfThreads.toString)
    } catch {
      case _ =>
        console.logError("Bad number of threads.")
        go
    }
    go
  }
  
  /* 1. left pane */
  /* 1.1 upper pane */
  val upperButtons = new ControlButtons

  val codeArea = new CodeArea
  val codeAreaScrollPane = new RTextScrollPane(codeArea.textArea, false)
  codeAreaScrollPane.setVerticalScrollBarPolicy(javax.swing.ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED)

  def toggleLineNumbers = codeAreaScrollPane.setLineNumbersEnabled(!codeAreaScrollPane.getLineNumbersEnabled)

  val statusZone = new StatusZone
  val upperBottomPane = new BoxPanel(Orientation.Horizontal) {
    contents += upperButtons
    contents += statusZone
  }

  val upperPane = new BorderPanel {
    add(codeArea.filenameLabel, BorderPanel.Position.North)
    add(Component.wrap(codeAreaScrollPane), BorderPanel.Position.Center)
    add(upperBottomPane, BorderPanel.Position.South)
  }

  /* 1.2 lower pane */
  val console = new tl.Console
  val fileBrowser = new FileBrowser(Files.currentDir, codeArea)
  fileBrowser.fileTree.peer.addTreeSelectionListener(codeArea)
  codeArea.addPathChangeListener(fileBrowser.fileTree)
  
  val lowerPane = new BorderPanel {
    // Synch button
    val synchButton = new JCheckBox()
    synchButton.setAction(new AbstractAction("Synch File Browser and Editor") {
      override def actionPerformed(e: java.awt.event.ActionEvent) {
        GraphicalMain.synchEditorWithBrowser = !GraphicalMain.synchEditorWithBrowser
        if (GraphicalMain.synchEditorWithBrowser)
          codeArea.currentFile match {
            case Some(file) => fileBrowser.fileTree.focus(file)
            case None => fileBrowser.fileTree.refresh
          }
      }
    })
    synchButton.setSelected(GraphicalMain.synchEditorWithBrowser)
    val toolbar = new JToolBar()
    toolbar.setFloatable(false)
    synchButton.setFocusable(false)
    synchButton.setBorderPainted(false)
    toolbar.add(synchButton)

    // Console / File Browser 
    val tabs = new TabbedPane {
      pages += new TabbedPane.Page("Console", new BorderPanel {
        add(new ScrollPane(console), BorderPanel.Position.Center)
      })
      pages += new TabbedPane.Page("File Browser", fileBrowser)
      preferredSize = new Dimension(DEFAULT_HEIGHT / 4, preferredSize.width)
    }

    add(Component.wrap(toolbar), BorderPanel.Position.North)
    add(tabs, BorderPanel.Position.Center)
  }
  

  val leftPane =
    new SplitPane(Orientation.Horizontal, upperPane, lowerPane) {
      oneTouchExpandable = true
      resizeWeight = 1.0
    }

  /* 2 right pane */
  val traceTable = new Table {
    //model = traceModel
    autoResizeMode = Table.AutoResizeMode.Off
  }

  // FIXME: This probably should't be here -- kevina
  val jPlotI = new plot.JPlotInput {
    def obj() = newPlotView
    def newData() = if (controller.model != null) controller.model.getNewData else null // XXX: Why is null check required?
    def addToPlot(d: Object) = {
      newPlotView.plotter.addToPlot(d)
      newPlotView.plotter.chartPanel.validate
    }
  }

  val plotView = new plot.PlotTab
  val pointedView = new plot.PointedView(plotView)

  val plotTab = new BorderPanel {
    add(new FlowPanel(FlowPanel.Alignment.Leading)(pointedView),
      BorderPanel.Position.North)
    add(plotView, BorderPanel.Position.Center)
  }
  var newPlotView: plot.JFreePlotTab = null
  var newPlotTab: BorderPanel = null
  if (!GraphicalMain.disableNewPlot) {
    newPlotView = new plot.JFreePlotTab
    newPlotTab = new BorderPanel {
      //TODO Implement and add something like pointedView for the new plotting code
      add(newPlotView, BorderPanel.Position.Center)
    }
    jPlotI.enabled = true
  }

  val traceTab = new ScrollPane(traceTable)
  var threeDtab = if (GraphicalMain.threeDState == ThreeDState.DISABLE) {
    console.log("Acumen3D disabled.")
    console.newLine
    if (GraphicalMain.need_quartz) {
      new threeD.DisabledThreeDTab("3D visualization disabled due to performace problems on Mac OS X. \n\nTo enable restart Java with -Dapple.awt.graphics.UseQuartz=true or use --3d to force 3D to be enabled.")
    } else {
      new threeD.DisabledThreeDTab("3D visualization disabled on the command line.")
    }
    //null
  } else if (GraphicalMain.threeDState == ThreeDState.LAZY) {
    new threeD.DisabledThreeDTab("3D visualization will be enabled when needed.")
  } else {
    start3D
  }

  def start3D = try {
    val res = new threeD.ThreeDTab(controller)
    GraphicalMain.threeDState = ThreeDState.ENABLE
    res
  } catch {
    case e =>
      console.log("Error loading Java3D: " + e)
      console.newLine
      console.log("Disabling 3D Tab.")
      console.newLine
      GraphicalMain.threeDState = ThreeDState.ERROR
      val errors = new StringWriter()
      e.printStackTrace(new PrintWriter(errors))
      new threeD.DisabledThreeDTab("Acumen 3D disabled.\nError loading Java3D: " + e +
        "\n\nFull backtrace:\n" + errors)
  }

  val views = new TabbedPane {
    assert(pages.size == 0)
    pages += new TabbedPane.Page("Plot", plotTab)
    val PLOT_IDX = pages.last.index
    val NEW_PLOT_IDX = if (newPlotTab != null) {
      pages += new TabbedPane.Page("New Plot", newPlotTab)
      selection.index = pages.last.index
      pages.last.index
    } else {
      -1
    }
    pages += new TabbedPane.Page("Table", traceTab)
    val TABLE_IDX = pages.last.index

    pages += new TabbedPane.Page("3D", threeDtab)
    val THREED_IDX = pages.last.index

    selection.reactions += {
      case SelectionChanged(_) =>
        possibleEnable3D
        actor.publish(ViewChanged(selection.index))
    }

    var shouldEnable3D = false

    def possibleEnable3D = {
      if (selection.index == THREED_IDX && shouldEnable3D)
        pages(THREED_IDX).content = start3D
    }
  }

  /* main component */
  val body =
    new SplitPane(Orientation.Vertical, leftPane, views) {
      oneTouchExpandable = true
      resizeWeight = 0.2
    }

  /* menu bar */

  /* Used to enable the contained MenuItems when the simulator stops and if the 
   * condition () => Boolean evaluates to true.
   */
  private val enabledWhenStopped = scala.collection.mutable.Buffer[(MenuItem,() => Boolean)]()
  
  private def enableWhenStopped(m: MenuItem) = enabledWhenStopped += (m, ()=>true)  
  
  /** Same as mkActionAccelMask, but with a default accelerator mask (depending on OS). */
  private def mkAction(name: String, m: Int, a: Int, act: => Unit) =
    mkActionMask(name, m, a, shortcutMask, act)

  /** Depending on the operating system, returns the appropriate mask key. */
  private def shortcutMask() = util.System.detectOperatingSystem match {
    case util.System.Windows | util.System.Unix | util.System.Other => CTRL_MASK
    case util.System.Mac => java.awt.event.InputEvent.META_MASK
  }
  
  /** 
   * Used to construct actions for MenuItems. Both m and a should be some VK from KeyEvent. 
   * m:     Mnemonic, used to select item from menu.
   * a:     Accelerator, used to launch action directly from keyboard, in combination with the aMask key.
   * aMask: Key to be pressed together with accelerator key to launch the action.
   * act:   Action to be performed when item is selected. 
   **/
  private def mkActionMask(name: String, m: Int, a: Int, aMask: Int, act: => Unit) = new Action(name) { 
    mnemonic = m; accelerator = if (a != NONE) Some(KeyStroke.getKeyStroke(a, aMask)) else None
    def apply = act
  } 
 
  private val playMenuItem = new MenuItem(playAction) 
  private val stepMenuItem = new MenuItem(stepAction)
  private val stopMenuItem = new MenuItem(stopAction)
 
  val bar = new MenuBar {
    contents += new Menu("File") {
      mnemonic = Key.F
      contents += new MenuItem(newAction)     { enableWhenStopped(this) }
      contents += new MenuItem(openAction)    { enableWhenStopped(this) }
      contents += new MenuItem(saveAction)
      contents += new MenuItem(saveAsAction)  
      contents += new MenuItem(exportTableAction) { enableWhenStopped(this) }
      contents += new MenuItem(recoverAction) { enableWhenStopped(this) }
      contents += new MenuItem(exitAction)
    }
    
    contents += new Menu("Edit") {
      mnemonic = Key.E
      contents += new MenuItem(cutAction) 
      contents += new MenuItem(copyAction)
      contents += new MenuItem(pasteAction)
      contents += new Separator
      contents += new MenuItem(selectAllAction)
    }

    contents += new Menu("View") {
      mnemonic = Key.V
      contents += new MenuItem(increaseFontSizeAction) 
      contents += new MenuItem(decreaseFontSizeAction)
      contents += new MenuItem(resetFontSizeAction)
      contents += new Menu("Font") {
	    mnemonic = Key.F
	    val fontNames = codeArea.supportedFonts.map { fontName =>
	      new RadioMenuItem(fontName) {
		    selected = codeArea.textArea.getFont.getName == fontName
		    action = Action(fontName) { codeArea setFontName fontName }
		  } 
	    }
	    contents ++= fontNames
	    new ButtonGroup(fontNames:_*)
	  }
      contents += new CheckMenuItem("Show line numbers") { 
        mnemonic = Key.L
        action = showLineNumbersAction
      }
    }

    contents += new Menu("Plotting") {
      
      mnemonic = Key.P
      contents += new Menu("Style") {
        mnemonic = Key.S
        val rb1 = new RadioMenuItem("") { selected = true; action = plotStyleLinesAction }
        val rb2 = new RadioMenuItem("") { selected = false; action = plotStyleDotsAction }
        val rb3 = new RadioMenuItem("") { selected = false; action = plotStyleBothAction }
        contents ++= Seq(rb1,rb2,rb3)
        new ButtonGroup(rb1,rb2,rb3)
      }
      contents += new CheckMenuItem("") { selected = false
        action = new Action("Simulator Fields") 
        		 { mnemonic = KeyEvent.VK_S; def apply = plotView.toggleSimulator(selected) }
      }
      contents += new CheckMenuItem("") { selected = false
        action = new Action("Child Count") 
        		 { mnemonic = KeyEvent.VK_C; def apply = plotView.toggleNextChild(selected) }
      }
      contents += new CheckMenuItem("") { selected = false
        action = new Action("RNG Seeds") 
        		 { mnemonic = KeyEvent.VK_R; def apply = plotView.toggleSeeds(selected) }
      }
    }
    
    contents += new Menu("Model") {
      mnemonic = Key.S
      contents ++= Seq(playMenuItem, stepMenuItem, stopMenuItem)
    }

    object semantics {
      val newRef = new RadioMenuItem("") {
        selected = false
        enableWhenStopped(this)
        action = floatingPointNewAction
      }
      val ref = new RadioMenuItem("") {
        selected = false
        enableWhenStopped(this)
        action = floatingPointAction
      }
      val impr = new RadioMenuItem("") {
        selected = false
        enableWhenStopped(this)
        action = floatingPointImperativeAction
      }
      val par = new RadioMenuItem("") {
        selected = false
        enableWhenStopped(this)
        action = floatingPointParallelAction
      }
      val pwl = new RadioMenuItem("") {
        action = pwlHybridSolverAction
        enableWhenStopped(this)
        selected = false // GraphicalMain.useEnclosures && 
          //enclosure.Interpreter.strategy.eventEncloser.getClass == classOf[PWLEventEncloser] 
      }
      val et = new RadioMenuItem("") {
        action = eventTreeHybridSolverAction
        enableWhenStopped(this) 
        selected = false // GraphicalMain.useEnclosures &&
          //enclosure.Interpreter.strategy.eventEncloser.getClass == classOf[TreeEventEncloser]
      }
      val bg = new ButtonGroup(ref, newRef, impr, par, pwl, et)
      val ls = new CheckMenuItem("") {
        action = contractionAction
        enabledWhenStopped += (this, () => interpreter.interpreter.getClass == enclosure.Interpreter.getClass)
        enabled = false //GraphicalMain.useEnclosures
        selected = enclosure.Interpreter.strategy.eventEncloser.ivpSolver.getClass == classOf[LohnerSolver]
        /* Enable/disable Contraction menu item depending on the chosen semantics */
        for (b <- bg.buttons) listenTo(b) 
        reactions += {
          case e: ButtonClicked =>
            enabled = interpreter.interpreter.getClass == enclosure.Interpreter.getClass
        }
      }
    }
    
    contents += new Menu("Semantics") {
      import semantics._
      mnemonic = Key.S
      contents ++= Seq(ref, newRef, impr, par, new Separator, pwl, et, new Separator, ls)
    }
   
    contents += new Menu("Help") {
      mnemonic = Key.H
      contents += new MenuItem(tutorialAction)
      contents += new MenuItem(aboutAction) 
    }
  }

  /* gluing everything together */
  def top = new MainFrame {
    title = "Acumen"
    contents = body
    menuBar = bar
    size = new Dimension(DEFAULT_WIDTH,DEFAULT_HEIGHT)
    // XXX: consider deleting
    override def closeOperation() {     
    exit
     }
    
  }

  def exit {
    //timer3d.destroy=true
    //receiver.destroy=true
    //threeDView.exit

    if (!codeArea.editedSinceLastSave || codeArea.confirmContinue()) {
      controller ! Stop
      actor ! EXIT
      quit
    }
  }

  def withErrorReporting(action: => Unit): Unit = {
    try action
    catch { case e => reportError(e) }
  }

  def reportError(e: Throwable) {
    val em = e.getMessage
    console.logError(if (em != null) em else e.toString)
    System.err.println("Note: Redirected this exception to console log:")
    e.printStackTrace()
  }

  //def redraw = traceView.redraw

  /* ------ simple dialogs ----- */

  def about = {
    val version = acumen.util.System.version
    Dialog.showMessage(body, "Acumen " + version, "About")
  }
  def tutorial = {
    if (Desktop.isDesktopSupported) {
      val desktop = Desktop.getDesktop
      try { desktop.browse(acumen.util.System.tutorialUrl) }
      catch { case e: IOException => reportError(e) }
    } else reportError(new Exception("Could not find a web browser."))
  }

  /* ----- events handling ---- */

  var state: State = Stopped
  var interpreter: InterpreterCntrl = null;
  def setInterpreter(args: String*) = {
    val intr = Main.selectInterpreter(args:_*)
    interpreter = InterpreterCntrl.cntrlForInterpreter(intr);
  }
  setInterpreter(GraphicalMain.interpreterString)
  interpreter.interpreter.id.toList match {
    case "reference" :: _=> bar.semantics.ref.selected = true
    case "newreference" :: _ => bar.semantics.newRef.selected = true
    case "imparative" :: _ => bar.semantics.impr.selected = true
    case "parallel" :: _ => bar.semantics.par.selected = true
    case "enclosure" :: tail if tail.contains("pwl") => bar.semantics.pwl.selected = true
    case "enclosure" :: tail if tail.contains("evt") => bar.semantics.et.selected = true
  }

  controller.start()

  listenTo(actor)
  // disable and enable menu items
  reactions += {
    case st:State =>
      playMenuItem.enabled = st match {case _:App.Playing => false; case _ => true}
      stopMenuItem.enabled = st match {case   App.Stopped => false; case _ => true}
      stepMenuItem.enabled = st match {case _:App.Ready   => true;  case _ => false}
      for ((el,cond) <- enabledWhenStopped) el.enabled = st == Stopped && cond() 
      st match {
        case _:App.Ready =>
          playMenuItem.text = "Run"
          playMenuItem.action = playAction
        case _:App.Playing =>
          playMenuItem.text = "Pause"
          playMenuItem.action = pauseAction
        case _ =>
      }
  }
  // update console
  reactions += {
    case st: State =>
      //println("GM: New State: " + st)
      st match {
        case Stopped =>
          console.log("Stopped.")
          console.newLine
        case Paused =>
          console.log("Paused. ")
          console.newLine
        case Starting =>
          console.fadeOldMessages()
          console.log("Starting...")
        case Resuming if state != Starting =>
          console.log("Resuming...")
        case _ =>
      }
      state = st
  }
  // enable 3d if required
  reactions += {
    case Stopped =>
      if (GraphicalMain.threeDState == ThreeDState.LAZY &&
        !controller.threeDData._3DData.isEmpty)
        views.shouldEnable3D = true
      views.possibleEnable3D
  }

  // FIXME: Move me into a seperate TraceTable class
  // and move tableI out of plotter and into the new class
  val defaultTableModel = traceTable.model
  traceTable.listenTo(actor)
  traceTable.reactions += {
    case st: State =>
      st match {
        case Starting =>
          traceTable.model = defaultTableModel
        //case _:Ready if traceTable.model.isInstanceOf[TraceModel] => 
        //  traceTable.model.asInstanceOf[TraceModel].fireTableStructureChanged()
        case _ =>
      }
    case plot.TraceModelReady(model) =>
      traceTable.model = model
      model.fireTableStructureChanged()
    case ViewChanged(idx) =>
      if (idx == views.TABLE_IDX && App.ui.controller.model != null) {
        plotView.plotPanel.tableI.enabled = true
        plotView.plotPanel.plotter ! plot.Refresh
      } else {
        plotView.plotPanel.tableI.enabled = false
      }
  }

  def confirmSave(c: java.awt.Component, f:File) = {
    val message = 
      "File " + f.toString + 
      " already exists.\nAre you sure you want to overwrite it?"
    JOptionPane.showConfirmDialog(c, message,
      "Really?", JOptionPane.YES_NO_OPTION) == JOptionPane.YES_OPTION
  }
  def exportTable = {
    val fc = new FileChooser()
    val returnVal = fc.showSaveDialog(App.ui.body)
    if (returnVal == FileChooser.Result.Approve) {

      // Hack to make sure table data is populated
      val prev = plotView.plotPanel.tableI.enabled
      plotView.plotPanel.tableI.enabled = true
      plotView.plotPanel.plotter ! plot.Refresh
      plotView.plotPanel.tableI.enabled = prev

      val file = fc.selectedFile
      if (!file.exists || confirmSave(App.ui.body.peer, file)) {
        val model = traceTable.model
        val out = new FileWriter(fc.selectedFile)
        var i = 0
        while (i < model.getColumnCount()) {
	  out.write(model.getColumnName(i) + "\t");
          i += 1
	}
	out.write("\n");
        i = 0
	while (i< model.getRowCount) {
          var j = 0;
	  while (j < model.getColumnCount) {
	    out.write(model.getValueAt(i,j).toString()+"\t");
            j += 1;
	  }
	  out.write("\n");
          i += 1;
	}
	out.close();
      }
    }
  }
  
  // Add application-wide keyboard shortcuts

  KeyboardFocusManager.getCurrentKeyboardFocusManager.addKeyEventDispatcher(new KeyEventDispatcher {
      def dispatchKeyEvent(e: KeyEvent): Boolean =
        if ((e.getModifiers == Toolkit.getDefaultToolkit.getMenuShortcutKeyMask ||
             e.getModifiers == java.awt.event.InputEvent.CTRL_MASK) &&
             e.getID        == java.awt.event.KeyEvent.KEY_PRESSED)
            e.getKeyCode match {
              case VK_R      => upperButtons.bPlay.doClick ; true
              case VK_T      => upperButtons.bStop.doClick ; true
              case VK_G      => upperButtons.bStep.doClick ; true
              case VK_S      => codeArea.saveFile() ; true
              case VK_O      => codeArea.openFile(codeArea.currentDir) ; true
              case VK_L      => toggleLineNumbers ; true
              case VK_PLUS | 
              	   VK_EQUALS => codeArea increaseFontSize ; true
              case VK_MINUS  => codeArea decreaseFontSize ; true
              case VK_0      => codeArea resetFontSize ; true
              case _         => false 
            }
        else false 
    })

  /* ----- initialisation ----- */

  actor.start
  codeArea.listenDocument
  console.log("<html>Welcome to Acumen.<br/>" +
    "Please see LICENSE file for licensing details.</html>")
  console.newLine
  actor.publish(Stopped)
  actor.publish(ViewChanged(views.selection.index))

  if (GraphicalMain.autoPlay)
    upperButtons.bPlay.doClick
}

object App {
  def init = {
    new App // the initialization will set the members below
  }

  var ui: App = null

  var actor: Actor = null
  var pub: Publisher = null

  def !(e: Any) = actor ! e
  def publish(e: Event) = pub.publish(e)

  sealed abstract class State extends Event
  sealed abstract class Ready extends State // Meta state: Simulation not running
  sealed abstract class Playing extends State // Meta state: Simulation running
  case object Starting extends Playing // State when starting for the first time
  case object Resuming extends Playing // State when resuming from paused state
  case object Stopped extends Ready // State when stopped
  case object Paused extends Ready // State when paused

  case class ViewChanged(idx: Int) extends Event
}

