
package acumen
package ui

import tl._
import interpreter._
import util.System.shortcutMask

import scala.actors._
import java.awt.Desktop
import java.awt.KeyboardFocusManager
import java.awt.KeyEventDispatcher
import java.awt.Toolkit
import java.awt.event.KeyEvent
import java.awt.event.KeyEvent._
import java.io._
import java.net.InetAddress
import javax.swing._

import swing.{Action, BorderPanel, ButtonGroup, Dialog, Dimension, FlowPanel, MainFrame, Menu, MenuBar, MenuItem, Orientation, Publisher, RadioMenuItem, ScrollPane, SimpleSwingApplication, SplitPane, Swing, TabbedPane}
import swing.event._
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
    Logger.log("Selected the \"" + si.descr + "\" semantics.")
  }

  // Create a special actor to listen to events Acumen's web interface
  scala.actors.Actor.actor {
    while (true) {
      if (Main.webInterface.inputStream.ready()) {
        val input = Main.webInterface.inputStream.readLine
        deserializeSocketInput(input)
      }
      Thread.sleep(100)
    }
  }

  def deserializeSocketInput(input: String): Unit = {
    try {
      val jsonString = ujson.read(input)
      jsonString(0)("type").str match {
        case "action" =>
          jsonString(0)("action").str match {
            case "Play" =>
              runSimulation()
            case "Pause" =>
              controller ! Pause
            case "Step" =>
              stepSimulation()
            case "Stop" =>
              controller ! Stop
              stopSimulation()
            case "SelectFile" =>
              codeArea.loadFile(filetree.getFile(jsonString(0)("file").num.toInt))
            case "newFile" =>
              codeArea.newFile
            case "openFile" =>
              codeArea.openFile(codeArea.currentDir)
            case "saveFile" =>
              codeArea.saveFile(updateCurrentFile = jsonString(0)("updateCurrent").bool)
            case "saveFileAs" =>
              codeArea.saveFileAs(updateCurrentFile = jsonString(0)("updateCurrent").bool)
            case "recover" =>
              codeArea.openFile(Files.autoSavedDir)
            case "Exit" =>
              exit()
            case "simulatorFields" =>
              if (jsonString(0)("value").str.equals("true")) plotView.toggleSimulator(true)
              else plotView.toggleSimulator(false)
            case "childCount" =>
              if (jsonString(0)("value").str.equals("true")) plotView.toggleNextChild(true)
              else plotView.toggleNextChild(false)
            case "rngSeeds" =>
              if (jsonString(0)("value").str.equals("true")) plotView.toggleSeeds(true)
              else plotView.toggleSeeds(false)
            case "normalization" =>
              toggleNormalization()
            /* Semantics Changes */
            case "setSemantics" =>
              jsonString(0)("semantics").str match {
                case "reference2015"          => setSemantics(S.Ref2015)
                case "reference2014"          => setSemantics(S.Ref2014)
                case "reference2013"          => setSemantics(S.Ref2013)
                case "reference2012"          => setSemantics(S.Ref2012)
                case "optimized2015"          => setSemantics(S.Opt2015)
                case "optimized2014"          => setSemantics(S.Opt2014)
                case "optimized2013"          => setSemantics(S.Opt2013)
                case "optimized2012"          => setSemantics(S.Opt2012)
                case "parallel2012"           => promptForNumberOfThreads
                case "pwlHybridSolver"        => setSemantics(S.Enclosure(S.PWL,contraction))
                case "eventTreeHybridSolver"  => setSemantics(S.Enclosure(S.EVT,contraction))
                case "enclosure2015"          => setSemantics(S.Enclosure2015(contraction))
              }
            case "contraction" =>
              toggleContraction()
            /* Server Actions */
            case "startServer" =>
              startServer()
            case "stopServer" =>
              stopServer()
            case "resetDevice" =>
              resetDevice()
          }
        case "btnAction" =>
          jsonString(0)("action").str match {
            case "traceTab" =>
              selectedView = viewsCollection(1)
              if (selectedView.equals(viewsCollection(1)) && App.ui.controller.model != null) {
                plotView.plotPanel.tableI.enabled = true
                plotView.plotPanel.plotter ! plot.Refresh
              }
              else {
                plotView.plotPanel.tableI.enabled = false
              }
          }
        case "event" =>
          jsonString(0)("event").str match {
            case "jsReady" =>
              filetree.serializeFileTree(Files.currentDir)      // Serialize fileTree
              codeArea.sendInitCodeArea()                       // Serialize codeArea
              constructSemanticsItems()                         // Serialize semantics menu
              if (Main.extraPasses.contains("normalize")) { Main.webInterface.socketSend(ujson.write(ujson.Obj("event" -> "enableNormalize"))) }
              Main.webInterface.socketSend(ujson.write(ujson.Obj("event" -> "state", "state" -> "appReady")))
          }
        case "codeUpdate" =>
          codeArea.updateCodeText(jsonString(0)("text").str)
          codeArea.setEdited
        case _ =>
          println("Unrecognized json")
      }
    }
    catch {
      case x: Exception =>  //println(x.getCause.toString)
        println("Json error: " + input)
    }
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
          case ConsoleMsg(instr) => console.append(instr)

          case EXIT => exit

          case SendInit => 
            withErrorReporting {
              interpreter = InterpreterCntrl(Main.defaultSemantics,Some(codeArea.codeText))
              if (Main.defaultSemantics != interpreter.semantics) {
                warnSemanticsChange(Main.defaultSemantics, interpreter.semantics)
                setSemantics(interpreter.semantics)
              }
              controller ! Init(codeArea.codeText, codeArea.currentDir, interpreter)
            }

          case msg: Event =>
            //println("Publishing This Msg: " + msg)
            publish(msg)
            msg match {
              case st: State =>
                st match {
                  case _:App.Ready =>
                    Main.webInterface.socketSend(ujson.write(ujson.Obj("event" -> "state", "state" -> st.toString)))
                  case _:App.Playing =>
                    Main.webInterface.socketSend(ujson.write(ujson.Obj("event" -> "state", "state" -> st.toString)))
                  case _ =>
                }
              case _ =>
            }

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

  val viewsCollection = Array("PLOT_IDX", "TABLE_IDX", "THREED_IDX")
  var selectedView = viewsCollection(0)

  /* Semantics Items */
  private val traditionalSemantics = Seq(
    SemanticsItem("reference2015", "2015 Reference", isEnclosure = false, selected = false, enabled = true),
    SemanticsItem("optimized2015", "2015 Optimized", isEnclosure = false, selected = true, enabled = true))
  private val enclosureSemantics = Seq(
    SemanticsItem("enclosure2015"        , "2015 Enclosure", isEnclosure = true, selected = false, enabled = true),
    SemanticsItemSeparator,
    SemanticsItem("pwlHybridSolver"      , "2013 PWL"      , isEnclosure = true, selected = false, enabled =Main.enableOldSemantics),
    SemanticsItem("eventTreeHybridSolver", "2013 EVT"      , isEnclosure = true, selected = false, enabled =Main.enableOldSemantics))
  private val deprecatedSemantics = Seq(
    SemanticsItem("reference2014", "2014 Reference", isEnclosure = false, selected = false, enabled = Main.enableOldSemantics),
    SemanticsItem("optimized2014", "2014 Optimized", isEnclosure = false, selected = false, enabled = Main.enableOldSemantics),
    SemanticsItemSeparator,
    SemanticsItem("reference2013", "2013 Reference", isEnclosure = false, selected = false, enabled = Main.enableOldSemantics),
    SemanticsItem("optimized2013", "2013 Optimized", isEnclosure = false, selected = false, enabled = Main.enableOldSemantics),
    SemanticsItemSeparator,
    SemanticsItem("reference2012", "2012 Reference", isEnclosure = false, selected = false, enabled = Main.enableOldSemantics),
    SemanticsItem("optimized2012", "2012 Optimized", isEnclosure = false, selected = false, enabled = Main.enableOldSemantics),
    SemanticsItem("parallel2012" , "2012 Parallel" , isEnclosure = false, selected = false, enabled = Main.enableOldSemantics))
  private val semantics = List(traditionalSemantics, enclosureSemantics, deprecatedSemantics)
  //**************************************

  private def constructSemanticsItems(): Unit = {
    val semanticsJson = ujson.Arr(ujson.Obj("action" -> "populateSemantics"))
    for (group <- semantics.indices) {
      val semanticsGroup = ujson.Obj()
      val semanticsData = ujson.Arr()
      for (item <- semantics(group)) {
        item match {
          case SemanticsItem(a, b, c, d, e) =>
            if (e) semanticsData.arr.append(ujson.Obj("id" -> a, "name" -> b, "isEnclosure" -> c, "selected" -> d))
          case SemanticsItemSeparator =>
            semanticsData.arr.append(ujson.Obj("id" -> "separator"))
        }
      }
      if (group==0) semanticsGroup.obj.put("traditional", semanticsData)
      else if (group==1) semanticsGroup.obj.put("enclosure", semanticsData)
      else semanticsGroup.obj.put("deprecated", semanticsData)
      semanticsJson.arr.append(semanticsGroup.value)
    }
    Main.webInterface.socketSend(ujson.write(semanticsJson))
  }

  //**************************************

  /* ----- UI setup ------- */
  
  private val NONE = VK_UNDEFINED
  /* Reusable actions */

  private val plotStyleLinesAction            = new Action(  "Lines")       { mnemonic =            VK_L; def apply = plotView.setPlotStyle(plot.Lines()) }
  private val plotStyleDotsAction             = new Action(  "Dots")        { mnemonic =            VK_D; def apply = plotView.setPlotStyle(plot.Dots()) }
  private val plotStyleBothAction             = new Action(  "Both")        { mnemonic =            VK_B; def apply = plotView.setPlotStyle(plot.Both()) }
  private val manualAction                    = mkAction(    "Reference Manual",                    VK_M, VK_F1,      manual())
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

  val codeArea = new CodeArea
  def fixed3DRatio = disable3DRatioItem.selected

  /* 1.2 lower pane */
  val console = new tl.Console
  val filetree = new tl.FileTree

  /* 2 right pane */
  val plotView = new plot.PlotTab
  val pointedView = new plot.PointedView(plotView)

  val plotTab = new BorderPanel {
    add(new FlowPanel(FlowPanel.Alignment.Leading)(pointedView),
      BorderPanel.Position.North)
    add(plotView, BorderPanel.Position.Center)
  }

  var threeDtab = new ThreeDTab(controller)

  val views = new TabbedPane {
    assert(pages.size == 0)
    pages += new TabbedPane.Page("Plot", plotTab)
    val PLOT_IDX = pages.last.index

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
  val body = views

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
    contents += new Menu("View") {
      mnemonic = Key.V
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
    }

    val bta = new RadioMenuItem("") {
      import Main.extraPasses
      toggleBTA()
      selected = true
      enableWhenStopped(this)     
    }

    contents += new Menu("Help") {
      mnemonic = Key.H
      contents += new MenuItem(manualAction)
      contents += new MenuItem(aboutAction)
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

    controller ! Stop
    actor ! EXIT
    quit
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
    Main.webInterface.socketSend(ujson.write(ujson.Obj("event" -> "serverStarted", "link" -> (IPADDRESS + ":8000/index"))))
  }

  def stopServer(): Unit = {
    resetDevice()
    BuildHost.BuildHost.stop()
    Main.webInterface.socketSend(ujson.write(ujson.Obj("event" -> "serverStopped")))
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

  def dumpParms() = {
    Logger.log("Using the \"" + interpreter.semantics.descr + "\" semantics.")
    if (Main.commandLineParms) {
      Logger.log("Passes: " + Main.extraPasses.mkString(","))
    }
  }

  controller.start()

  listenTo(actor)
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

  /** Serializes and sends the table data as json */
  def serializeTable (model: TraceModel) = {
    val jsonFormat = ujson.Arr(ujson.Obj("event" -> "traceTable"))
    val tableArr = ujson.Arr()
    val rowArr = ujson.Arr()
    for (i <- 0 until model.getColumnCount) {
      rowArr.arr.append(model.getColumnName(i))
    }
    tableArr.arr.append(rowArr.value)
    rowArr.arr.clear()
    for (i <- 0 until model.getRowCount) {
      for (j <- 0 until model.getColumnCount) {
        rowArr.arr.append(model.getValueAt(i, j).toString)
      }
      tableArr.arr.append(rowArr.value)
      rowArr.arr.clear()
    }
    jsonFormat.arr.append(tableArr)
    Main.webInterface.socketSend(ujson.write(jsonFormat))
  }

  def confirmSave(c: java.awt.Component, f:File) = {
    val message = 
      "File " + f.toString + 
      " already exists.\nAre you sure you want to overwrite it?"
    JOptionPane.showConfirmDialog(c, message,
      "Really?", JOptionPane.YES_NO_OPTION) == JOptionPane.YES_OPTION
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
              case VK_S      => codeArea.saveFile() ; true
              case VK_O      => codeArea.openFile(codeArea.currentDir) ; true
              case _         => false 
            }
        else e.getKeyCode match {
          case _ => false
        }
    })

  /* ----- initialisation ----- */

  actor.start
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
  console.append(Logger.Message(Logger.INFO, Logger.TextMsg("Welcome to Acumen.\nPlease see Help/License file for licensing details.")))

  actor.publish(Stopped)
  actor.publish(ViewChanged(views.selection.index))

  if (Main.autoPlay)
    runSimulation()
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
  case class SemanticsItem(id: String, name: String, isEnclosure: Boolean, selected: Boolean, enabled: Boolean)
  case class SemanticsItemSeparator(id: String = "separator")
}

