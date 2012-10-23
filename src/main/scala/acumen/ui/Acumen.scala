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

import swing._
import swing.event._

//import tl

// class Acumen = Everything that use to be GraphicalMain.  Graphical
// Main can't be an object it will cause Swing components to be
// initialized outside of the Event Dispatcher Thread (EDT).  Thus 
// we make it a seperate class and have GraphicalMain top() method
// initialize it

class Acumen extends SimpleSwingApplication {

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

  val statusZone = new StatusZone
  val upperBottomPane = new BoxPanel(Orientation.Horizontal) {
    contents += upperButtons
    contents += statusZone
  }
  val upperPane = new BorderPanel {
    add(codeArea.filenameLabel, BorderPanel.Position.North)
    add(new ScrollPane(codeArea), BorderPanel.Position.Center)
    add(upperBottomPane,  BorderPanel.Position.South) 
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

  val traceView = new plot.PlotTab
  val pointedView = new plot.PointedView(traceView)

  val tab1 = new BorderPanel {
    add(new FlowPanel(FlowPanel.Alignment.Leading)(pointedView), 
        BorderPanel.Position.North)
    add(traceView, BorderPanel.Position.Center)
  }
  val tab2 = new ScrollPane(traceTable) 
  var threeDtab = try {
    new threeD.ThreeDTab(controller)
  } catch {
    case e:UnsatisfiedLinkError => 
      console.log("Error loading Java3D: " + e)
      console.newLine
      console.log("Disabling 3D Tab.")
      console.newLine
      new threeD.DisabledThreeDTab
  }
  
  val rightPane = new TabbedPane {
    assert(pages.size == 0)
    pages ++= List(new TabbedPane.Page("Plot", tab1), 
                   new TabbedPane.Page("Trace", tab2),
                   new TabbedPane.Page("3D",threeDtab))
    if (!threeDtab.enableTab)
      pages(2).enabled = false
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
      contents += new MenuItem(Action("New")({ codeArea.newFile })) 
                      { mnemonic = Key.N }
      contents += new MenuItem(Action("Open")({ codeArea.openFile(codeArea.currentDir) })) 
                      { mnemonic = Key.O }
      contents += new MenuItem(Action("Save")(codeArea.saveFile))
                      { mnemonic = Key.S }
      contents += new MenuItem(Action("Save As")(codeArea.saveFileAs))
                      { mnemonic = Key.A }
      contents += new MenuItem(Action("Recover")({ codeArea.openFile(Files.autoSavedDir) }))
                      { mnemonic = Key.R }
      contents += new MenuItem(Action("Exit")(exit))
                      { mnemonic = Key.E }
    }

    contents += new Menu("Plotting") {
      
      mnemonic = Key.P
      contents += new Menu("Style") {
        val rb1 = new RadioMenuItem("") {
          selected = true
          action = Action("Lines") { traceView.setPlotStyle(plot.Lines()) }
        }
        val rb2 = new RadioMenuItem("") {
          selected = false
          action = Action("Dots") { traceView.setPlotStyle(plot.Dots()) }
        }
        val rb3 = new RadioMenuItem("") {
          selected = false
          action = Action("Both") { traceView.setPlotStyle(plot.Both()) }
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
        action = Action("Purely Functional") { setInterpreter(new CStoreCntrl(interpreters.reference.Interpreter)) }
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
            setInterpreter(new CStoreCntrl(new interpreters.parallel.Interpreter(lastNumberOfThreads)))
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
        action = Action("Enclosure") { setInterpreter(new EnclosureCntrl(interpreters.enclosure.Interpreter)) }
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
      println("Exiting...")
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
  
  var state : AppState = AppState.Stopped
  var interpreter : InterpreterCntrl = new CStoreCntrl(interpreters.reference.Interpreter)
  def setInterpreter(i : InterpreterCntrl) = {
    interpreter = i
  }

  controller.start()

  // Create a special actor to listen to events from other threads

  case object EXIT
  val actor = new Actor with Publisher {
    override val scheduler = new SchedulerAdapter {
      def execute(fun: => Unit) { Swing.onEDT(fun) }
    }
    start()
    
    def act() {
      trapExit = true
      link(controller)
      loop {
        react {
          case msg@StateChanged(st) => println("GM: State Changed!"); publish(msg)
          case Error(e)       => reportError(e)
          case Progress(p)    => statusZone.setProgress(p)
          case ProgressMsg(m) => console.log(m); console.newLine
          case Progress3d(p)  => threeDtab.setProgress(p)

          case EXIT => println("...Exiting UI Actor."); exit

          case SendInit => controller ! Init(codeArea.text, interpreter)

          case Exit(_,ue:UncaughtException) =>
            System.err.println("Actor Died Unexpected!")
            ue.cause.printStackTrace()

          case msg => println("Unknown Msg in GM: " + msg)
        }
      }
    }
  }

  listenTo(actor)
  reactions += {
    case StateChanged(st) => 
      println("GM: New State: " + st)
      st match {
        case AppState.Stopped =>
          console.log("Stopped.")
          console.newLine
        case AppState.Paused =>
          console.log("Paused. ")
          console.newLine
        case AppState.Starting =>
          console.log("Starting...")
        case AppState.Resuming if state != AppState.Starting =>
          console.log("Resuming...")
        case _ =>
      }
      state = st
  }
  
  // FIXME: Possible Move me.
  upperButtons.listenTo(actor)
  codeArea.listenTo(actor)
 
  val defTableModel = traceTable.model
  traceTable.listenTo(actor)
  traceTable.reactions += {
    case StateChanged(st) => 
      st match {
        case AppState.Starting => 
          traceTable.model = defTableModel
        case AppState.Resuming =>
          traceTable.enabled = false
        case _:AppState.Ready if controller.model != null => 
          val tm = controller.model.getTraceModel
          traceTable.model = tm
          tm.fireTableStructureChanged()
        case _ => 
      }
  }

  /* ----- initialisation ----- */
  
  codeArea.listenDocument
  console.log("<html>Welcome to Acumen.<br/>"+
              "Please see LICENSE file for licensing details.</html>")
  console.newLine
  actor.publish(StateChanged(AppState.Stopped))
}

object Acumen {

  def init = {
    ui = new Acumen
    actor = ui.actor
  }
  
  var ui : Acumen = null
  var actor : Actor = null

}
