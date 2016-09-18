
package acumen
package ui

import java.net.{InetAddress, URI}

import tl._
import interpreter._
import util.System.shortcutMask
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
import java.awt.event.KeyEvent.{VK_CLOSE_BRACKET => VK_RBRACKET, VK_OPEN_BRACKET => VK_LBRACKET}
import java.awt.event.InputEvent._
import java.io._
import javax.swing._

import org.fife.ui.rtextarea.{RTextArea, RTextScrollPane}

import swing.{Action, BorderPanel, BoxPanel, ButtonGroup, CheckMenuItem, Component, Dialog, Dimension, FileChooser, FlowPanel, Frame, Label, MainFrame, Menu, MenuBar, MenuItem, Orientation, Publisher, RadioMenuItem, ScrollPane, Separator, SimpleSwingApplication, SplitPane, Swing, TabbedPane, Table, TextField}
import swing.event._
import scala.Boolean
import acumen.interpreters.{enclosure, imperative2012}
import acumen.interpreters.{reference2012, reference2013}
import acumen.interpreters.enclosure.ivp.PicardSolver
import acumen.interpreters.enclosure.ivp.LohnerSolver
import acumen.interpreters.enclosure.event.pwl.PWLEventEncloser
import acumen.interpreters.enclosure.event.pwl.PWLEventEncloser
import acumen.interpreters.enclosure.event.tree.TreeEventEncloser
import acumen.{SemanticsImpl => S}
import acumen.ui.threeD._

// class Acumen = Everything that used to be GraphicalMain. Graphical
// Main can't be an object it will cause Swing components to be
// initialized outside of the Event Dispatcher Thread (EDT).  Thus 
// we make it a separate class and have GraphicalMain top() method
// initialize it

class App extends SimpleSwingApplication {
  import App._

  App.ui = this
  
  val DEFAULT_HEIGHT = 768
  val DEFAULT_WIDTH = 1024
  val IPADDRESS = IPaddress()

  private val cores = Runtime.getRuntime.availableProcessors
  
  var interpreter: InterpreterCntrl = InterpreterCntrl(Main.defaultSemantics)
  
  def setSemantics(si: SemanticsImpl[Interpreter]) = {
    Main.defaultSemantics = si
    ui.codeArea.updateCompletionProvider(si.interpreter())
    Logger.log("Selected the \"" + si.descr + "\" semantics.")
  }
  
  // Create a special actor to listen to events from other threads
  // runs on the EDT

  case object EXIT
  val actor = new Actor with Publisher {
    override val scheduler = new SchedulerAdapter {
      def execute(fun: => Unit) { Swing.onEDT(fun) }
    }

    def act() {
      Supervisor.watch(this, "Main UI", { restart })
      loop {
        react {
          case Progress(p) => statusZone.setProgress(p)
          case ConsoleMsg(instr) => console.append(instr)

          case EXIT => exit

          case SendInit => 
            withErrorReporting {
              interpreter = InterpreterCntrl(Main.defaultSemantics,Some(codeArea.textArea.getText))
              if (Main.defaultSemantics != interpreter.semantics) {
                warnSemanticsChange(Main.defaultSemantics, interpreter.semantics)
                setSemantics(interpreter.semantics)
                selectMenuItemFromSemantics()
              }
              controller ! Init(codeArea.textArea.getText, codeArea.currentDir, interpreter)
            }

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

  @volatile var modelFinished : Boolean = false

  //**************************************
  //**************************************

  /* ----- UI setup ------- */
  
  private val NONE = VK_UNDEFINED
  /* Reusable actions */

  private val playAction                      = mkAction(    "Run",                                 VK_R, VK_G,       upperButtons.bPlay.doClick)
  private val pauseAction                     = mkAction(    "Pause",                               VK_R, VK_G,       upperButtons.bPlay.doClick)
  private val stepAction                      = mkAction(    "Step",                                VK_T, VK_B,       upperButtons.bStep.doClick)
  private val stopAction                      = mkAction(    "Stop",                                VK_S, VK_T,       upperButtons.bStop.doClick)
  private val newAction                       = mkAction(    "New",                                 VK_N, VK_N,       codeArea.newFile)
  private val openAction                      = mkAction(    "Open",                                VK_O, VK_O,       codeArea.openFile(codeArea.currentDir))
  private val saveAction                      = mkAction(    "Save",                                VK_S, VK_S,       codeArea.saveFile())
  private val saveAsAction                    = mkActionMask("Save As",                             VK_A, VK_S,       shortcutMask | SHIFT_MASK, codeArea.saveFileAs())
  private val recoverAction                   = mkAction(    "Recover",                             VK_R, VK_R,       codeArea.openFile(Files.autoSavedDir))
  private val exportTableAction               = new Action(  "Export Table"){ mnemonic =            VK_E; def apply = exportTable()}
  private val exitAction                      = mkAction(    "Exit",                                VK_E, VK_Q,       exit())
  private val cutAction                       = mkAction(    "Cut",                                 VK_T, VK_X,       codeArea.textArea.cut)
  private val copyAction                      = mkAction(    "Copy",                                VK_C, VK_C,       if (ui.console.peer.getSelectedIndex != -1) ui.console.copySelection else codeArea.textArea.copyAsRtf)
  private val pasteAction                     = mkAction(    "Paste",                               VK_P, VK_V,       codeArea.textArea.paste)
  private val selectAllAction                 = mkAction(    "Select All",                          VK_A, VK_A,       codeArea.textArea.selectAll)
  private val findReplaceAction               = mkAction(    "Find",                                VK_F, VK_F,       toggleFindReplaceToolbar)
  private val increaseFontSizeAction          = mkAction(    "Enlarge Font",                        VK_I, VK_PLUS,    codeArea increaseFontSize)
  private val decreaseFontSizeAction          = mkAction(    "Reduce Font",                         VK_D, VK_MINUS,   codeArea decreaseFontSize)
  private val resetFontSizeAction             = mkAction(    "Reset Font",                          VK_R, VK_0,       codeArea resetFontSize)
  private val increaseIndentAction            = mkAction(    "Increase Indentation",                VK_N, VK_RBRACKET,codeArea increaseIndent) // Key binding set in CodeArea
  private val decreaseIndentAction            = mkAction(    "Decrease Indentation",                VK_E, VK_LBRACKET,codeArea decreaseIndent) // Key binding set in CodeArea
  private val showLineNumbersAction           = mkAction(    "Line Numbers",                        VK_L, VK_L,       toggleLineNumbers)
  private val plotStyleLinesAction            = new Action(  "Lines")       { mnemonic =            VK_L; def apply = plotView.setPlotStyle(plot.Lines()) }
  private val plotStyleDotsAction             = new Action(  "Dots")        { mnemonic =            VK_D; def apply = plotView.setPlotStyle(plot.Dots()) }
  private val plotStyleBothAction             = new Action(  "Both")        { mnemonic =            VK_B; def apply = plotView.setPlotStyle(plot.Both()) }
  private val reference2015Action             = mkActionMask("2015 Reference",                      VK_R, VK_R,       shortcutMask | SHIFT_MASK, setSemantics(S.Ref2015))
  private val reference2014Action             = mkActionMask("2014 Reference",                      VK_R, NONE,       shortcutMask | SHIFT_MASK, setSemantics(S.Ref2014))
  private val reference2013Action             = mkActionMask("2013 Reference",                      VK_R, NONE,       shortcutMask | SHIFT_MASK, setSemantics(S.Ref2013))
  private val reference2012Action             = mkActionMask("2012 Reference",                      VK_R, NONE,       shortcutMask | SHIFT_MASK, setSemantics(S.Ref2012))
  private val optimized2015Action             = mkActionMask("2015 Optimized",                      VK_O, VK_O,       shortcutMask | SHIFT_MASK, setSemantics(S.Opt2015))
  private val optimized2014Action             = mkActionMask("2014 Optimized",                      VK_O, NONE,       shortcutMask | SHIFT_MASK, setSemantics(S.Opt2014))
  private val optimized2013Action             = mkActionMask("2013 Optimized",                      VK_O, NONE,       shortcutMask | SHIFT_MASK, setSemantics(S.Opt2013))
  private val optimized2012Action             = mkActionMask("2012 Optimized",                      VK_O, NONE,       shortcutMask | SHIFT_MASK, setSemantics(S.Opt2012)) 
  private val parallel2012Action              = mkActionMask("2012 Parallel",                       VK_P, NONE,       shortcutMask | SHIFT_MASK, promptForNumberOfThreads)
  private val pwlHybridSolverAction           = mkActionMask("2013 PWL",                            VK_L, VK_L,       shortcutMask | SHIFT_MASK, setSemantics(S.Enclosure(S.PWL,contraction))) 
  private val eventTreeHybridSolverAction     = mkActionMask("2013 EVT",                            VK_T, VK_T,       shortcutMask | SHIFT_MASK, setSemantics(S.Enclosure(S.EVT,contraction)))
  private val enclosure2015Action             = mkActionMask("2015 Enclosure",                      VK_5, VK_D,       shortcutMask | SHIFT_MASK, setSemantics(S.Enclosure2015(contraction)))
  private val startSeverAction                = mkAction(    "Start Server",                        NONE, NONE,       startServer())
  private val stopServerAction                = mkAction(    "Stop Server",                         NONE, NONE,       stopServer())
  private val resetDeviceNum                  = mkAction(    "Reset Device",                        NONE, NONE,       resetDevice())
  private val contractionAction               = mkActionMask("Contraction",                         VK_C, VK_C,       shortcutMask | SHIFT_MASK, toggleContraction())
  private val normalizeAction                 = mkAction(    "Normalize (to H.A.)",                 VK_N, NONE,       toggleNormalization())
  private val manualAction                    = mkAction(    "Reference Manual",                    VK_M, VK_F1,      manual())
  private val bugReportAction                 = mkAction(    "Bugs",                                NONE, NONE,       bugReport())
  private val aboutAction                     = new Action(  "About")       { mnemonic =            VK_A; def apply = about() }
  private val licenseAction                   = new Action(  "License")     { mnemonic =            VK_L; def apply = license() }
  
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
        Logger.log("Number of threads set to " + cores + ", the number of cores available to Acumen")
        cores
      } else {
        Logger.log("Number of threads set to " + userNumberOfThreads)
        userNumberOfThreads
      }
      setSemantics(S.Parallel2012(lastNumberOfThreads))
    } catch {
      case _ =>
        Logger.error("Bad number of threads.")
        go
    }
    go
  }
  
  var contraction = false
  private def toggleContraction() = {
    contraction = !contraction;
    Main.defaultSemantics match {
      case S.Enclosure(eh, _) => 
        setSemantics(S.Enclosure(eh, contraction))
      case S.Enclosure2015(_) => 
        setSemantics(S.Enclosure2015(contraction))
    }
  }
  
  /* 1. left pane */
  /* 1.1 upper pane */
  val upperButtons = new ControlButtons

  val codeArea = new CodeArea
  val codeAreaScrollPane = new RTextScrollPane(codeArea.textArea, false)
  codeAreaScrollPane.setVerticalScrollBarPolicy(javax.swing.ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED)
  
  def toggleLineNumbers = codeAreaScrollPane.setLineNumbersEnabled(!codeAreaScrollPane.getLineNumbersEnabled)
  def fixed3DRatio = disable3DRatioItem.selected
  def toggleFindReplaceToolbar = {
    codeArea.findReplaceToolBar.setVisible(!codeArea.findReplaceToolBar.isVisible)
    if (codeArea.findReplaceToolBar.isVisible) codeArea.searchField.requestFocus 
    else codeArea.textArea.requestFocus
  }
  
  val codePanel = new BorderPanel {
    add(Component.wrap(codeAreaScrollPane), BorderPanel.Position.Center)
    add(Component.wrap(codeArea.findReplaceToolBar), BorderPanel.Position.South)
  }

  val statusZone = new StatusZone
  val upperBottomPane = new BoxPanel(Orientation.Horizontal) {
    contents += upperButtons
    contents += statusZone
  }
  
  val upperPane = new BorderPanel {
    add(codeArea.filenameLabel, BorderPanel.Position.North)
    add(codePanel, BorderPanel.Position.Center)  
    add(upperBottomPane, BorderPanel.Position.South)
  }

  /* 1.2 lower pane */
  val console = new tl.Console
  val fileBrowser = new FileBrowser(Files.currentDir, codeArea)
  fileBrowser.fileTree.peer.addTreeSelectionListener(codeArea)
  codeArea.addPathChangeListener(fileBrowser.fileTree)
  
  val consolePage = new TabbedPane.Page("Console", new BorderPanel {
    add(new ScrollPane(console), BorderPanel.Position.Center)
  })
  
  val lowerPane = new BorderPanel {
    // Synch button
    val synchButton = new JCheckBox()
    synchButton.setAction(new AbstractAction("Synch File Browser and Editor") {
      override def actionPerformed(e: java.awt.event.ActionEvent) {
        Main.synchEditorWithBrowser = !Main.synchEditorWithBrowser
        if (Main.synchEditorWithBrowser)
          codeArea.currentFile match {
            case Some(file) => fileBrowser.fileTree.focus(file)
            case None => fileBrowser.fileTree.refresh
          }
      }
    })
    synchButton.setSelected(Main.synchEditorWithBrowser)
    val toolbar = new JToolBar()
    toolbar.setFloatable(false)
    synchButton.setFocusable(false)
    synchButton.setBorderPainted(false)
    toolbar.add(synchButton)
    
    // Console / File Browser 
    val tabs = new TabbedPane {
      pages += consolePage
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
    def addToPlot(t: Tag, d: Object) = {
      newPlotView.plotter.addToPlot(t, d)
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
  if (!Main.disableNewPlot) {
    newPlotView = new plot.JFreePlotTab
    newPlotTab = new BorderPanel {
      //TODO Implement and add something like pointedView for the new plotting code
      add(newPlotView, BorderPanel.Position.Center)
    }
    jPlotI.enabled = true
  }

  val traceTab = new ScrollPane(traceTable)

  var threeDtab = new ThreeDTab(controller)

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

    pages += new TabbedPane.Page("_3D", threeDtab)
    val THREED_IDX = pages.last.index

    selection.reactions += {
      case SelectionChanged(_) =>
        possibleEnable3D()
        actor.publish(ViewChanged(selection.index))
    }

    var shouldEnable3D = false

    def threeDViewSelected = peer.getSelectedIndex == THREED_IDX
    def selectPlotView() = peer.setSelectedIndex(PLOT_IDX)
    def selectThreeDView() = peer.setSelectedIndex(THREED_IDX)
    
    def possibleEnable3D() =
      if (selection.index == THREED_IDX && shouldEnable3D) {
        App.ui.threeDtab = threeDtab
        pages(THREED_IDX).content = App.ui.threeDtab
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
    mkActionMask(name, m, a, shortcutMask(), act)

  /** 
   * Used to construct actions for MenuItems. Both m and a should be some VK from KeyEvent. 
   * m:     Mnemonic, used to select item from menu.
   * a:     Accelerator, used to launch action directly from keyboard, in combination with the aMask key.
   * aMask: Key to be pressed together with accelerator key to launch the action.
   * act:   Action to be performed when item is selected. 
   **/
  private def mkActionMask(name: String, m: Int, a: Int, aMask: Int, act: => Unit) = new Action(name) { 
    mnemonic = m; accelerator = if (a != NONE) Some(KeyStroke.getKeyStroke(a, aMask)) else None
    def apply() = act
  } 
 
  private val playMenuItem = new MenuItem(playAction) 
  private val stepMenuItem = new MenuItem(stepAction)
  private val stopMenuItem = new MenuItem(stopAction)

  private val startserverItem = new RadioMenuItem("Start Server") {selected = false; action = startSeverAction}
  private val resetDeviceItem = new MenuItem("Reset Device")  {action = resetDeviceNum}
  private val stopserverItem  = new MenuItem("Stop Server")  {action = stopServerAction}
  private val serverLinkItem  = new Menu("Server Link"){
    val url = IPADDRESS + ":8000/index"
    val rb1 = new MenuItem(url){
      action = new Action(url){ def apply() = Desktop.getDesktop.browse(new java.net.URI("http://" + url)) }
    }
    contents ++= Seq(rb1)
    new ButtonGroup(rb1)
  }
  private val disable3DRatioItem = new RadioMenuItem("Disable") {
    action = new Action("Disable") {
      def apply() = {
        threeDtab.threeDView.resize3DView(null)
      }
    }
  }
  private val fixed3DRatioItem = new RadioMenuItem("16:9") {
    selected = true
    action = new Action("16:9 Ratio") {
      def apply() = {
        threeDtab.threeDView.widthRatio = 16
        threeDtab.threeDView.heightRatio = 9
        threeDtab.threeDView.resize3DView(null)
      }
    }
  }
  private val custom3DRatioItem = new RadioMenuItem("Custom Ratio") {
    action = new Action("Custom Ratio") {
      def apply () = {
        val widthField = new JTextField(threeDtab.threeDView.widthRatio.toString, 5)
        val heightField = new JTextField(threeDtab.threeDView.heightRatio.toString, 5)
        val ratioPanel = new JPanel()
        ratioPanel.add(new JLabel("width:"))
        ratioPanel.add(widthField)
        ratioPanel.add(new JLabel("height:"))
        ratioPanel.add(heightField)
        val result = JOptionPane.showConfirmDialog(null, ratioPanel, "Please enter the ratio of width and height",
                                                   JOptionPane.OK_CANCEL_OPTION)
        def parseInt(s: String): Boolean = try {
          Some(s.toInt)
          true
        } catch { case e: Exception => false }
        if (result == JOptionPane.OK_OPTION) {
          if (parseInt(widthField.getText) && parseInt(heightField.getText)) {
            threeDtab.threeDView.widthRatio = widthField.getText.toInt
            threeDtab.threeDView.heightRatio = heightField.getText.toInt
            threeDtab.threeDView.resize3DView(null)
          } else {
            JOptionPane.showMessageDialog(null, "Error: Aspect ratio should be integer",
              "Error Massage", JOptionPane.ERROR_MESSAGE)
          }
        }
      }
    }
  }


  // FIXME Move all of this state into Main, and expose through CLI
  def getStartAnaglyph = false
  
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
      contents += new MenuItem(new Action("Undo"){
        override lazy val peer = RTextArea getAction RTextArea.UNDO_ACTION
        peer.setAccelerator(KeyStroke.getKeyStroke(VK_Z, shortcutMask))
        def apply = codeArea.textArea.undoLastAction
      })
      contents += new MenuItem(new Action("Redo"){
        override lazy val peer = RTextArea getAction RTextArea.REDO_ACTION
        peer.setAccelerator(KeyStroke.getKeyStroke(VK_Z, shortcutMask | SHIFT_MASK))
        def apply = codeArea.textArea.redoLastAction
      }) 
      contents += new Separator
      contents += new MenuItem(cutAction) 
      contents += new MenuItem(copyAction)
      contents += new MenuItem(pasteAction)
      contents += new Separator
      contents += new MenuItem(increaseIndentAction)
      contents += new MenuItem(decreaseIndentAction)
      contents += new Separator
      contents += new MenuItem(selectAllAction)
      contents += new CheckMenuItem("Find") {
        mnemonic = Key.F
        action = findReplaceAction
      }
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
        new ButtonGroup(fontNames: _*)
      }
      contents += new CheckMenuItem("Show line numbers") {
        mnemonic = Key.L
        action = showLineNumbersAction
      }
      contents += new Menu("Aspect Ratio") {
        contents ++= Seq(disable3DRatioItem, fixed3DRatioItem, custom3DRatioItem)
        new ButtonGroup(disable3DRatioItem,fixed3DRatioItem,custom3DRatioItem)
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
        		 { mnemonic = KeyEvent.VK_F; def apply = plotView.toggleSimulator(selected) }
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

    val bta = new RadioMenuItem("") {
      import Main.extraPasses
      toggleBTA()
      selected = true
      enableWhenStopped(this)     
    }
    
    object semantics {
      val ref2015 = new RadioMenuItem("") {
        selected = false
        enableWhenStopped(this)
        action = reference2015Action
      }
      val opt2015 = new RadioMenuItem("") {
        selected = false
        enableWhenStopped(this)
        action = optimized2015Action
      }
      val ref2014 = new RadioMenuItem("") {
        visible = Main.enableOldSemantics
        selected = false
        enableWhenStopped(this)
        action = reference2014Action
      }
      val opt2014 = new RadioMenuItem("") {
        visible = Main.enableOldSemantics
        selected = false
        enableWhenStopped(this)
        action = optimized2014Action
      }
      val ref2013 = new RadioMenuItem("") {
        visible = Main.enableOldSemantics
        selected = false
        enableWhenStopped(this)
        action = reference2013Action
      }
      val opt2013 = new RadioMenuItem("") {
        visible = Main.enableOldSemantics
        selected = false
        enableWhenStopped(this)
        action = optimized2013Action
      }
      val ref2012 = new RadioMenuItem("") {
        visible = Main.enableOldSemantics
        selected = false
        enableWhenStopped(this)
        action = reference2012Action
      }
      val opt2012 = new RadioMenuItem("") {
        visible = Main.enableOldSemantics
        selected = false
        enableWhenStopped(this)
        action = optimized2012Action
      }
      val par2012 = new RadioMenuItem("") {
        visible = Main.enableOldSemantics
        selected = false
        enableWhenStopped(this)
        action = parallel2012Action
      }
      val encPWL = new RadioMenuItem("") {
        visible = Main.enableOldSemantics
        action = pwlHybridSolverAction
        enableWhenStopped(this)
        selected = false // Main.useEnclosures && 
          //enclosure.Interpreter.strategy.eventEncloser.getClass == classOf[PWLEventEncloser] 
      }
      val encEVT = new RadioMenuItem("") {
        visible = Main.enableOldSemantics
        action = eventTreeHybridSolverAction
        enableWhenStopped(this) 
        selected = false // Main.useEnclosures &&
          //enclosure.Interpreter.strategy.eventEncloser.getClass == classOf[TreeEventEncloser]
      }
      val enc2015 = new RadioMenuItem("") {
        action = enclosure2015Action
        enableWhenStopped(this) 
        selected = false
      }
      val bg = new ButtonGroup(ref2013, opt2013, ref2014, opt2014, ref2015, opt2015, ref2012, opt2012, par2012, encPWL, encEVT, enc2015)
      val ls = new CheckMenuItem("") {
        def shouldBeEnabled = Main.defaultSemantics match { case _:S.Enclosure | _:S.Enclosure2015 => true; case _ => false }
        action = contractionAction
        enabledWhenStopped += (this, () => shouldBeEnabled)
        enabled = false //Main.useEnclosures
        selected = contraction
        /* Enable/disable Contraction menu item depending on the chosen semantics */
        for (b <- bg.buttons) listenTo(b) 
        reactions += {
          case e: ButtonClicked =>
            enabled = shouldBeEnabled
        }
      }
      val lc = new CheckMenuItem("") {
        action = normalizeAction
        selected = Main.extraPasses.contains("normalize")
      }
    }

    contents += new Menu("Semantics") {
      import semantics._
      mnemonic = Key.S
      contents += new Menu("Traditional") {
        mnemonic = Key.T
        contents += ref2015
        contents += opt2015
      }
      contents += new Menu("Enclosure") {
        mnemonic = Key.E
        contents ++= Seq(enc2015) ++ (if (Main.enableOldSemantics) Seq(new Separator, encPWL, encEVT) else Seq()) ++ Seq(new Separator, ls)
      }
      if (Main.enableAllSemantics) {
        contents += new Menu("Deprecated") {
          visible = Main.enableOldSemantics
          mnemonic = Key.D
          contents ++= Seq(ref2014, opt2014, new Separator, ref2013, opt2013, new Separator, ref2012, opt2012, par2012)
        }
        contents ++= Seq(new Separator,lc)
      }
    }

    // Every time when you want to use device data, check start serve Item is selected or not!
    contents += new Menu("Devices") {
      contents += startserverItem
      startserverItem.selected = false
      contents += stopserverItem
      stopserverItem.enabled = false
      contents += resetDeviceItem
      resetDeviceItem.enabled = false
      contents += serverLinkItem
      serverLinkItem.enabled = false
    }

    contents += new Menu("Help") {
      mnemonic = Key.H
      contents += new MenuItem(manualAction)
      contents += new MenuItem(aboutAction)
      contents += new MenuItem(bugReportAction)
      contents += new MenuItem(licenseAction) 
    }
  }
  def toggleBTA() = {
    import Main.extraPasses
    if (extraPasses.contains("BTA")) 
      extraPasses = extraPasses.filter(_ != "BTA")
    else
      extraPasses = extraPasses :+ "BTA"
  }
  
  def toggleNormalization() = {
    import Main.extraPasses
    if (extraPasses.contains("normalize")) 
      extraPasses = extraPasses.filter(_ != "normalize")
    else
      extraPasses = extraPasses :+ "normalize"
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

  def exit() {
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
    catch { case e: Exception => Logger.error(e) }
  }

  // returns true to continue and false to cancel
  def warnSemanticsChange(oldS: SemanticsImpl[_], newS: SemanticsImpl[_]) {
    val res = Dialog.showConfirmation(
      message = "Changing semantics from \"" + oldS.descr + "\" to \"" + newS.descr + "\".",
      title = "Changing semantics ...",
      optionType = Dialog.Options.OkCancel,
      messageType = Dialog.Message.Info)
    //console.logError("Changing semantics from " + oldS.descr + " to " + newS.descr + ".")
    if (res != Dialog.Result.Ok) throw new Errors.AcumenError {
      override def getMessage = "Simulation Canceled."
    }
  }

  //def redraw = traceView.redraw

  /* ------ simple dialogs ----- */

  def about() = {
    AboutDialog setLocationRelativeTo body
    AboutDialog visible = true
  }
  
  def license() = {
    LicenseDialog setLocationRelativeTo body
    LicenseDialog visible = true
  }
  
  /** Try to open bundled manual in external browser. On failure, use ManualBrowser. */
  def manual() =
    try { // See if Acumen is running from source (e.g. using sbt)
      Desktop.getDesktop.open(new File(ManualBrowser.getClass.getResource("manual.html").toURI))
    } catch { case _ => 
      try { // See if Acumen is running from JAR, inside release directory
        val jarPath = new File(ManualBrowser.getClass.getProtectionDomain
          .getCodeSource.getLocation.getPath).getParentFile.getPath
        Desktop.getDesktop.open(new File(jarPath + "/src/main/resources/acumen/ui/tl/manual.html"))
      } catch { case e: Throwable => // Acumen is running from JAR, outside release directory
        ManualBrowser setLocationRelativeTo body
        ManualBrowser.peer setVisible true 
        ManualBrowser.peer setFocusable true
        ManualBrowser.peer requestFocus
      }
    }

  def bugReport() =
    Desktop.getDesktop.browse(new URI("http://www.acumen-language.org/p/report-bug.html"))

  def IPaddress():String = {
    val localHost = InetAddress.getLocalHost
    val localIPaddress = localHost.getHostAddress
    localIPaddress
  }

  def startServer(): Unit = {
    BuildHost.BuildHost.start()
    if (!threeDtab.checkBoxState("realTime")) {
      threeDtab.setCheckBoxState(true, "realTime")
      threeDtab.setCheckBoxState(true, "matchWallClock")
    } else if (!threeDtab.checkBoxState("matchWallClock")) {
      threeDtab.setCheckBoxState(true, "matchWallClock")
    }
    resetDeviceItem.enabled = true
    serverLinkItem.enabled = true
    startserverItem.enabled = false
    stopserverItem.enabled  = true
  }

  def stopServer(): Unit = {
    resetDevice()
    BuildHost.BuildHost.stop()
    startserverItem.selected = false
    startserverItem.enabled = true
    stopserverItem.enabled = false
    resetDeviceItem.enabled = false
    serverLinkItem.enabled = false
  }

  def resetDevice(): Unit = {
    val tempsensor = BuildHost.BuildHost.sensors.get(0)
    val tempdata = tempsensor.get(0)
    for (i <- 0 until 7){
      tempdata(i) = "0"
    }
    tempsensor.clear()
    tempsensor.add(0, tempdata)
    BuildHost.BuildHost.sensors.clear()
    BuildHost.BuildHost.sensors.add(0, tempsensor)
    BuildHost.BuildHost.Device_counter = 0
  }

  /* ----- events handling ---- */
  
  var state: State = Stopped

  def selectMenuItemFromSemantics() {
    Main.defaultSemantics match {
      case S.Ref2012 => bar.semantics.ref2013.selected = true
      case S.Opt2013 => bar.semantics.opt2013.selected = true
      case S.Ref2013 => bar.semantics.ref2012.selected = true
      case S.Ref2014 => bar.semantics.ref2014.selected = true
      case S.Ref2015 => bar.semantics.ref2015.selected = true
      case S.Opt2014 => bar.semantics.opt2014.selected = true
      case S.Opt2015 => bar.semantics.opt2015.selected = true
      case S.Opt2012 => bar.semantics.opt2012.selected = true
      case _:S.Parallel2012  => bar.semantics.par2012.selected = true
      case S.Enclosure(S.PWL, c) => contraction = c; bar.semantics.encPWL.selected = true
      case S.Enclosure(S.EVT, c) => contraction = c; bar.semantics.encEVT.selected = true
      case S.Enclosure2015(c)    => contraction = c; bar.semantics.enc2015.selected = true
      case _ => /* Other semantics not selectable from the menu selected */
    }
    bar.semantics.ls.selected = contraction
  }
  selectMenuItemFromSemantics()

  def dumpParms() = {
    Logger.log("Using the \"" + interpreter.semantics.descr + "\" semantics.")
    if (Main.commandLineParms) {
      Logger.log("Passes: " + Main.extraPasses.mkString(","))
    }
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
          Logger.status(true, "Stopped.", false)
        case Paused =>
          Logger.status(true, "Paused.", false)
        case Starting =>
          Logger.separator()
          dumpParms()
          Logger.status(false, "Starting", true)
        case Resuming if state != Starting =>
          Logger.status(false, "Resuming", true)
        case _ =>
      }
      state = st
  }
  // enable 3d if required
  reactions += {
    case Stopped =>
      if (controller.threeDData.modelContains3D()) {
        codeArea.editedSinceLastRun = false
        if (modelFinished && !threeDtab.checkBoxState("realTime")) {
          views.selectThreeDView()
          threeDtab.play()
        }
      } else if (views.threeDViewSelected) {
        views.selectPlotView()
      }
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
  
  /** Exports simulation data (corresponding to the table view) to a file. */
  def exportTable() = {
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
        while (i < model.getRowCount) {
          var j = 0;
          while (j < model.getColumnCount) {
            out.write(model.getValueAt(i, j).toString() + "\t");
            j += 1;
          }
          out.write("\n");
          i += 1;
        }
        out.close();
      }
    }
  }
  
  /** Everything that needs to be done to start a simulation. */
  def runSimulation() {
    controller.threeDData.reset()
    threeDtab.reset()
    codeArea.autoSave
    threeDtab.setCheckBoxes(false)
    threeDtab.disableButtons()
    controller ! Play
     // Clear the hash table every time a new simulation runs
    SD.clear()
  }
  
  /** Everything that needs to compute one simulation step. */
  def stepSimulation() {
    codeArea.autoSave
    threeDtab.setCheckBoxes(false)
    threeDtab.disableButtons()
    controller ! Step
  }

  /** Everything that needs to be done after simulation finished. */
  def stopSimulation() {
    threeDtab.setCheckBoxes(true)
    threeDtab.enableButtons()
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
        else e.getKeyCode match {
          case VK_ESCAPE if codeArea.findReplaceToolBar.isVisible => 
            toggleFindReplaceToolbar; true
          case _ => false
        }
    })

  /* ----- initialisation ----- */

  actor.start
  codeArea.listenDocument
  // Acumen console logger. Ignores TRACE and DEBUG log levels.
  Logger.attach(new Logger.Appender { def apply(instr: Logger.Instruction) {
    instr match {
      case Logger.Message(Logger.TRACE | Logger.DEBUG,_,_) =>
        // Ignore Logger.TRACE and Logger.DEBUG messages
      case _ => actor ! ConsoleMsg(instr) 
    }
  }})
  // Command line logger. Accepts messages with log levels lower than minLevel 
  // but ignores HypothesisReports.
  Main.printLogLevel.map(minLevel =>
    Logger.attach(new Logger.Appender{
      def apply(i: Logger.Instruction) = i match {
        case Logger.Message(l, m, _) if l.orderNum >= minLevel.orderNum =>
          println(s"$l: ${m.msg}")
        case _ => // Ignore messages with lower Level and HypothesisReports
      }
    }))
  console.append(Logger.Message(Logger.INFO, 
                                Logger.TextMsg("<html>Welcome to Acumen.<br/>" +
                                               "Please see Help/License file for licensing details.<html>")))

  actor.publish(Stopped)
  actor.publish(ViewChanged(views.selection.index))

  if (Main.autoPlay)
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

