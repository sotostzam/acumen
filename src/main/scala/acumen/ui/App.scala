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
import java.io._
import javax.swing.JOptionPane
import javax.swing.SwingUtilities
import javax.swing.undo._
import javax.swing.text._
import javax.swing.KeyStroke
import javax.swing.event.DocumentListener
import javax.swing.event.DocumentEvent
import swing.{Action, BorderPanel, BoxPanel, ButtonGroup, CheckMenuItem, 
			  Component, Dialog, Dimension, FlowPanel,
			  Label, Menu, MainFrame, MenuBar, MenuItem, Orientation, Publisher, 
			  RadioMenuItem, ScrollPane, SimpleSwingApplication, SplitPane, Swing,
			  TabbedPane, Table}
import swing.event._
import java.awt.KeyboardFocusManager
import java.awt.KeyEventDispatcher
import java.awt.event.KeyEvent
import java.awt.event.KeyEvent._
import java.awt.event.InputEvent._
import scala.Boolean
import java.awt.Toolkit
import org.fife.ui.rtextarea.RTextScrollPane
import scala.swing.Separator
import acumen.ui.tl.ControlButtons

// class Acumen = Everything that use to be GraphicalMain.  Graphical
// Main can't be an object it will cause Swing components to be
// initialized outside of the Event Dispatcher Thread (EDT).  Thus 
// we make it a seperate class and have GraphicalMain top() method
// initialize it

class App extends SimpleSwingApplication {
  import App._

  App.ui = this

  // Create a special actor to listen to events from other threads

  case object EXIT
  val actor = new Actor with Publisher {
    override val scheduler = new SchedulerAdapter {
      def execute(fun: => Unit) { Swing.onEDT(fun) }
    }
    
    def act() {
      Supervisor.watch(this, "Main UI", {restart})
      loop {
        react {
          case Error(e)       => reportError(e)
          case Progress(p)    => statusZone.setProgress(p)
          case ProgressMsg(m) => console.log(m); console.newLine
          case Progress3d(p)  => threeDtab.setProgress(p)

          case EXIT => exit

          case SendInit => controller ! Init(codeArea.textArea.getText, interpreter)

          case msg : Event => 
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

  /* 1. left pane */
  /* 1.1 upper pane */
  val upperButtons = new ControlButtons

  val codeArea = new CodeArea
  val codeAreaScrollPane = new RTextScrollPane(codeArea.textArea,false)
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

  val lowerPane = new BorderPanel {
    add(new Label("Console"), BorderPanel.Position.North)
    add(new ScrollPane(console), BorderPanel.Position.Center)
  }

  val leftPane = 
    new SplitPane(Orientation.Horizontal, upperPane, lowerPane) { 
      oneTouchExpandable = true
      resizeWeight = 0.9
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
  var newPlotView : plot.JFreePlotTab = null
  var newPlotTab : BorderPanel = null
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

  val enabledWhenStopped = scala.collection.mutable.Buffer[MenuItem]()
  
  /** Same as mkActionAccelMask, but with a default accelerator mask CTRL_MASK. */
  private def mkAction(name: String, m: Int, a: Int, act: => Unit) = mkActionAccelMask(name, m, a, CTRL_MASK, act)
  
  /** 
   * Used to construct actions for MenuItems. Both m and a should be some VK from KeyEvent. 
   * m:     Mnemonic, used to select item from menu.
   * a:     Accelerator, used to launch action directly from keyboard, in combination with the aMask key.
   * aMask: Key to be pressed together with accelerator key to launch the action.
   * act:   Action to be performed when item is selected. 
   **/
  private def mkActionAccelMask(name: String, m: Int, a: Int, aMask: Int, act: => Unit) = new Action(name) { 
    mnemonic = m; accelerator = Some(KeyStroke.getKeyStroke(a, aMask))
    def apply = act
  } 
  
  private val playAction   = mkAction("Run",  VK_R, VK_G, upperButtons.bPlay.doClick)
  private val pauseAction  = mkAction("Pause",  VK_R, VK_G, upperButtons.bPlay.doClick)
  private val playMenuItem = new MenuItem(playAction) 
  private val stepMenuItem = new MenuItem(mkAction("Step", VK_T, VK_B, upperButtons.bStep.doClick))
  private val stopMenuItem = new MenuItem(mkAction("Stop", VK_S, VK_T, upperButtons.bStop.doClick))
 
  val bar = new MenuBar {
    contents += new Menu("File") {
      mnemonic = Key.F
      contents += new MenuItem(mkAction("New", VK_N, VK_N, codeArea.newFile )) 
                      { enabledWhenStopped += this }
      contents += new MenuItem(mkAction("Open", VK_O, VK_O, codeArea.openFile(codeArea.currentDir))) 
                      { enabledWhenStopped += this }
      contents += new MenuItem(mkAction("Save", VK_S, VK_S, codeArea.saveFile))
      contents += new MenuItem(mkActionAccelMask("Save As", VK_A, VK_S, CTRL_MASK | SHIFT_MASK, codeArea.saveFileAs))
      contents += new MenuItem(mkAction("Recover", VK_R, VK_R, codeArea.openFile(Files.autoSavedDir)))
                      { enabledWhenStopped += this }
      contents += new MenuItem(mkAction("Exit", VK_E, VK_Q, exit))
    }
    
    contents += new Menu("Edit") {
      mnemonic = Key.E
      contents += new MenuItem(mkAction("Cut",        VK_T, VK_X, codeArea.textArea.cut)) 
      contents += new MenuItem(mkAction("Copy",       VK_C, VK_C, codeArea.textArea.copyAsRtf))
      contents += new MenuItem(mkAction("Paste",      VK_P, VK_V, codeArea.textArea.paste))
      contents += new Separator
      contents += new MenuItem(mkAction("Select all", VK_A, VK_A, codeArea.textArea.selectAll))
    }
    
    contents += new Menu("View") {
      mnemonic = Key.V
      contents += new MenuItem(mkAction("Increase font size", VK_I, VK_PLUS, codeArea increaseFontSize)) 
      contents += new MenuItem(mkAction("Decrease font size", VK_D, VK_MINUS,  codeArea decreaseFontSize))
      contents += new MenuItem(mkAction("Reset font size",    VK_R, VK_0,      codeArea resetFontSize))
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
      contents += new CheckMenuItem("") { action = mkAction("Show line numbers", VK_L, VK_L, toggleLineNumbers) }
    }

    contents += new Menu("Plotting") {
      mnemonic = Key.P
      contents += new Menu("Style") {
        mnemonic = Key.S
        val rb1 = new RadioMenuItem("") { selected = true
          action = new Action("Lines") { mnemonic = VK_L; def apply = plotView.setPlotStyle(plot.Lines()) }
        }
        val rb2 = new RadioMenuItem("") { selected = false
          action = new Action("Dots") { mnemonic = VK_D; def apply = plotView.setPlotStyle(plot.Dots()) }
        }
        val rb3 = new RadioMenuItem("") { selected = false
          action = new Action("Both") { mnemonic = VK_B; def apply = plotView.setPlotStyle(plot.Both()) }
        }
        contents ++= Seq(rb1,rb2,rb3)
        new ButtonGroup(rb1,rb2,rb3)
      }
      contents += new CheckMenuItem("") { selected = false
        action = new Action("Automatically plot simulator fields") 
        		 { mnemonic = KeyEvent.VK_S; def apply = plotView.toggleSimulator(selected) }
      }
      contents += new CheckMenuItem("") { selected = false
        action = new Action("Automatically plot child counter fields") 
        		 { mnemonic = KeyEvent.VK_C; def apply = plotView.toggleNextChild(selected) }
      }
      contents += new CheckMenuItem("") { selected = false
        action = new Action("Automatically random number generator seeds") 
        		 { mnemonic = KeyEvent.VK_R; def apply = plotView.toggleSeeds(selected) }
      }
    }
    
    contents += new Menu("Simulator") {
      mnemonic = Key.S
      contents ++= Seq(playMenuItem, stepMenuItem, stopMenuItem)
      contents += new Menu("Semantics") {
        mnemonic = Key.S
        val rb1 = new RadioMenuItem("") {
          selected = !GraphicalMain.useEnclosures
   		  enabledWhenStopped += this
          action = mkAction("Purely Functional", VK_F, VK_1, 
                            { setInterpreter(new CStoreCntrl(interpreters.reference.Interpreter)) })
        }
        val rb2 = new RadioMenuItem("") {
          selected = false
          enabledWhenStopped += this
          action = mkAction("Imperative (Parallel)", VK_P, VK_2, {
            def diag = Dialog.showInput(
              body, "Choose a number of threads",
              "Parallel Interpreter", Dialog.Message.Question,
              Swing.EmptyIcon, Seq(), lastNumberOfThreads.toString)
            def go: Unit = try {
              def n: String = diag.getOrElse(n)
              lastNumberOfThreads = Integer.parseInt(n)
              setInterpreter(new CStoreCntrl(new interpreters.parallel.Interpreter(lastNumberOfThreads)))
              console.log("Number of threads set to " + lastNumberOfThreads + ".")
            } catch {
              case _ =>
                console.logError("Bad number of threads.")
                go
            }
            go
          })
        }
        val rb3 = new RadioMenuItem("") {
          selected = GraphicalMain.useEnclosures
   		  enabledWhenStopped += this
          action = mkAction("Enclosure", VK_E, VK_3, 
                            { setInterpreter(new EnclosureCntrl(interpreters.enclosure.Interpreter)) })
        }
        contents ++= Seq(rb1,rb2,rb3)
        new ButtonGroup(rb1,rb2,rb3)
      }
    }
   
    contents += new Menu("Help") {
      mnemonic = Key.H
      contents += new MenuItem(mkActionAccelMask("Tutorial", VK_T, VK_F1, 0, tutorial))
      contents += new MenuItem(new Action("About") { mnemonic = VK_A; def apply = about }) 
    }
  }
 
  /* gluing everything together */
  def top = new MainFrame {
    title = "Acumen"
    contents = body
    menuBar = bar
    size = new Dimension(1024,768)
    // XXX: consider deleting
    override def closeOperation() {     
    exit
     }
    
  }

  def exit {  
    //timer3d.destroy=true
    //receiver.destroy=true
    //threeDView.exit
    
    if (!codeArea.editedSinceLastSave || codeArea.confirmContinue(body.peer)) {
      controller ! Stop
      actor ! EXIT				
      quit
    }
  }

  def withErrorReporting(action: => Unit) : Unit = {
    try action 
    catch { case e => reportError(e) }
  }

  def reportError(e:Throwable) {
    val em = e.getMessage
    console.logError(if (em!=null) em else e.toString)
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
      catch { case e:IOException => reportError(e) }
    } else reportError(new Exception("Could not find a web browser."))
  }

  /* ----- events handling ---- */
  
  var state : State = Stopped
  var interpreter : InterpreterCntrl = 
    if (GraphicalMain.useEnclosures)
      new EnclosureCntrl(interpreters.enclosure.Interpreter)
    else
      new CStoreCntrl(interpreters.reference.Interpreter)
  def setInterpreter(i : InterpreterCntrl) = {
    interpreter = i
  }

  controller.start()

  listenTo(actor)
  // disable and enable menu items
  reactions += {
    case st:State =>
      playMenuItem.enabled = st match {case _:App.Playing => false; case _ => true}
      stopMenuItem.enabled = st match {case   App.Stopped => false; case _ => true}
      stepMenuItem.enabled = st match {case _:App.Ready   => true;  case _ => false}
      for (el <- enabledWhenStopped) el.enabled = st == Stopped
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
    case st:State => 
      //println("GM: New State: " + st)
      st match {
        case Stopped =>
          console.log("Stopped.")
          console.newLine
        case Paused =>
          console.log("Paused. ")
          console.newLine
        case Starting =>
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
    case st:State => 
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
  
  // Add application-wide keyboard shortcuts
  
  KeyboardFocusManager.getCurrentKeyboardFocusManager.addKeyEventDispatcher(new KeyEventDispatcher {
      def dispatchKeyEvent(e: KeyEvent): Boolean =
        if (e.getModifiers == CTRL_MASK && e.getID == KEY_PRESSED)
            e.getKeyCode match {
              case VK_EQUALS => codeArea increaseFontSize ; true
              case _         => false 
            }
        else false 
    })

  /* ----- initialisation ----- */

  actor.start
  codeArea.listenDocument
  console.log("<html>Welcome to Acumen.<br/>"+
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

  var ui : App = null

  var actor : Actor = null
  var pub : Publisher = null

  def ! (e: Any) = actor ! e
  def publish(e: Event) = pub.publish(e)

  sealed abstract class State extends Event
  sealed abstract class Ready extends State
  sealed abstract class Playing extends State
  case object Starting extends Playing
  case object Resuming extends Playing
  case object Stopped extends Ready
  case object Paused extends Ready

  case class ViewChanged(idx: Int) extends Event
}
	