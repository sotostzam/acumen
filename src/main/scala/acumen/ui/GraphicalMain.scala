package acumen
package ui

import java.lang.Thread

import scala.actors._
import scala.math._
import collection.JavaConversions._
import collection.immutable.Queue
import scala.collection.mutable.Map
import scala.collection.mutable.Buffer

import java.awt.Font
import java.awt.BorderLayout
import java.awt.Color
import java.awt.RenderingHints
import java.awt.GraphicsEnvironment
import java.awt.Desktop
import java.io._
import javax.swing.JOptionPane
import javax.swing.JTabbedPane
import javax.swing.SwingUtilities
import javax.swing.undo._
import javax.swing.text._
import javax.swing.KeyStroke
import javax.swing.event.DocumentListener
import javax.swing.event.DocumentEvent
import javax.vecmath.{AxisAngle4d, Color3f, Point3d, Vector3d,Point3f,Vector3f}

import swing._
import swing.event._

import com.sun.j3d.utils.geometry.ColorCube
import com.sun.j3d.utils.geometry.Sphere
import com.sun.j3d.utils.behaviors.mouse._
import javax.media.j3d.{Appearance, AmbientLight, Background, BoundingBox, 
  BoundingSphere, Bounds, BranchGroup, Canvas3D, DirectionalLight, GeometryArray,
  Group, Geometry, Material, LineAttributes, LineArray, PolygonAttributes, 
  QuadArray, Screen3D, Shape3D, Transform3D, TransformGroup, TransparencyAttributes,
  View, ViewPlatform}



object GraphicalMain extends SimpleSwingApplication {

  /* ---- definitions ------ */

  val monospaced = new Font("Monospaced", Font.PLAIN, 12) 
  var threeDView  = new ThreeDView()
  
  val play = new Action("play") {
    icon = Icons.play
    def apply = {appModel.data.reset;                
                 receiver.stop; played = false;receiver.destroy=true;
								 check.selected = true;	timer3d.destroy=true;
								 autoSave;  appModel.play;threeDView.reset}
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
  var playSpeed = 1.0;
  val faster = new Action("faster"){
    icon = Icons.faster
    def apply = {        
			playSpeed = playSpeed * 2
			if (playSpeed > 4) playSpeed = 4   // maximum *4 speed
				timer3d.sleepTime =  timer3d.initSpeed/playSpeed 
			// Extra nano seconds	
			timer3d.extraTime = ((timer3d.sleepTime-timer3d.sleepTime.toLong)*1000000).toInt 
			statusZone3d.setSpeed(playSpeed.toString) // Show the speed 
    }  
  }
  val slower = new Action("slower"){
    icon = Icons.slower
    def apply = {      
			playSpeed = playSpeed / 2   
			timer3d.sleepTime = (1/playSpeed)*timer3d.initSpeed
			// Extra nano seconds		
			timer3d.extraTime = ((timer3d.sleepTime-timer3d.sleepTime.toLong)*1000000).toInt 
			statusZone3d.setSpeed(playSpeed.toString) // show the speed
    }  
  }
  val threedpause=new Action("pause"){
    icon = Icons.pause
    toolTip = "pause"
    def apply = {
    if (toolTip == "pause"){
      timer3d.pause  = false
      receiver.pause = true
      icon    = Icons.play
      toolTip = "resume"
    }
    else{
      timer3d.pause  = true
      receiver.pause = false
      icon    = Icons.pause
      toolTip = "pause"
     }   
    }               
  }
  val stop3d = new Action("stop"){
    threeDView.canvas.stopRenderer
    threeDView.canvas.startRenderer
    icon = Icons.stop
    def apply= {
			threedpause.toolTip="pause"
			threedpause.icon = Icons.pause
			receiver.destroy = true
			timer3d.destroy  = true
			threeDView.reset
			check.selected   = true
   }
    toolTip = "Stop visualizing"
  }
  /*----3D-Visulization----*/
  var played = false;
	
  val threedplay = new Action("play"){
    icon = Icons.play
    def apply = {
			threedpause.toolTip = "pause"
			threedpause.icon    = Icons.pause
			endTime = appModel.data.endTime
			if(played){    
				receiver.stop;
			timer3d.destroy = true;
			statusZone3d.setSpeed(playSpeed.toString)
			if(check.selected == true)
				threeDView.axisOn
    }
	// First time press "3D play" button, 
	// copy the data from list to buffer to speed up
    if(!played){
	    _3DDataBuffer.clear 
			lastFrame = 0;
			statusZone3d.setSpeed("1.0")
        for((id,map) <- appModel.data._3DData){
          var temp = Map[Int,Buffer[List[_]]]()
          for((objectNumber,l)<-map){
             temp += (objectNumber->l.reverse.toBuffer)
             temp(objectNumber).last(5) match {
			   // The animation's length
               case n:Int => if (n>lastFrame) {lastFrame=n}  
               case _=>               
             }                      
            }
          _3DDataBuffer += id->temp              
         }
			appModel.data.reset;
    }        
     threeDView.branches.clear  
     threeDView.trans.clear         
     receiver = new _3DDisplay(threeDView,statusZone3d,_3DDataBuffer,lastFrame,
															appModel.data.endTime)
     timer3d  = new ScalaTimer(receiver,appModel.data.endTime,playSpeed)
     receiver.start()
     timer3d.start()
     listenTo(receiver)
     receiver.listenTo(statusZone3d.bar.mouse.moves)
     receiver.listenTo(timer3d)
     timer3d.listenTo(statusZone3d.bar.mouse.clicks)
     timer3d.listenTo(statusZone3d.bar.mouse.moves)
		 played = true;                    
    }
    toolTip = "play"  
  }
    
  /* ---- state variables ---- */
  val appModel = new AppModel(codeArea.text,console)
  var currentFile : Option[File] = None
  var editedSinceLastSave : Boolean = false
  var editedSinceLastAutoSave : Boolean = false
  var _3DDataBuffer = Map[CId,Map[Int,scala.collection.mutable.Buffer[List[_]]]]()
  var lastNumberOfThreads = 2
  var lastFrame = 2.0
  var endTime = 10.0
  
//**************************************
//**************************************
   

  /* ----- UI setup ------- */

  /* 1. left pane */
  /* 1.1 upper pane */
  val s = new Dimension(50, 40)
  val bPlay = new Button(play) { peer.setHideActionText(true) }
  val bStep = new Button(step) { peer.setHideActionText(true) }
  var bStop = new Button(stop) { peer.setHideActionText(true); }
  val b3dplay = new Button(threedplay) {
                peer.setHideActionText(true);preferredSize = s }
  val b3dpause = new Button(threedpause) { 
                peer.setHideActionText(true);preferredSize = s }
  val b3dstop = new Button(stop3d) {
                peer.setHideActionText(true);preferredSize = s }
  val b3dfaster = new Button(faster) { 
                peer.setHideActionText(true);preferredSize = s }
  val b3dslower = new Button(slower) { 
                peer.setHideActionText(true);preferredSize = s }
  val check = new CheckBox("") { 
      action = Action("Axis-On") { 
        if (selected) threeDView.axisOn 
        else threeDView.axisOff
      }
    }
  check.selected = true	
  def hide(button:Button){button.peer.setEnabled(false)}
  val upperButtons = 
    new FlowPanel(FlowPanel.Alignment.Leading)(bPlay, bStep, bStop)
  val threeDButtons = 
    new FlowPanel(FlowPanel.Alignment.Leading)(b3dplay, 
	                    b3dpause,b3dslower,b3dfaster,b3dstop,check)    
  val codeArea = new EditorPane {
    font = monospaced
  }
   // New text utilities, e.g., undo, redo
  val undo = new UndoManager();
  var doc  = codeArea.peer.getDocument() 
  // Create a undo action and add it to the text component
  codeArea.peer.getActionMap().put("Undo",
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
   codeArea.peer.getActionMap().put("Redo",
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
  codeArea.peer.getInputMap().put(KeyStroke.getKeyStroke("control Z"), "Undo");
  // Bind the redo action to ctl-Y
  codeArea.peer.getInputMap().put(KeyStroke.getKeyStroke("control Y"), "Redo");		
  val statusZone = new StatusZone
  val statusZone3d= new Slider3d
  val upperBottomPane = new BoxPanel(Orientation.Horizontal) {
    contents += upperButtons
    contents += statusZone
  }
  val threeDBottomPane = new BoxPanel(Orientation.Horizontal) {
    contents += threeDButtons
    contents += statusZone3d
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
  
  /* 2 right pane */
  val traceTable = new Table { 
    model = appModel.tmodel 
    autoResizeMode = Table.AutoResizeMode.Off
  }

  val traceView = new TraceView(false, false, false, appModel.tmodel)
  val pointedView = new PointedView(traceView)
 
  var receiver   =  new _3DDisplay(threeDView,statusZone3d,
                               _3DDataBuffer,lastFrame,appModel.data.endTime)
  
  var timer3d      = new  ScalaTimer(receiver,appModel.data.endTime,playSpeed)
  val tab1 = new BorderPanel {
    add(new FlowPanel(FlowPanel.Alignment.Leading)(pointedView), 
        BorderPanel.Position.North)
    add(traceView, BorderPanel.Position.Center)
  }
  val tab2 = new ScrollPane(traceTable) 
  var tab3 = new BorderPanel{
    add(threeDView.init(),BorderPanel.Position.Center)
    add(threeDBottomPane, BorderPanel.Position.South)  
  }    
  
  val rightPane = new TabbedPane {
    pages ++= List(new TabbedPane.Page("Plot", tab1), 
                   new TabbedPane.Page("Trace", tab2),
                   new TabbedPane.Page("3D",tab3))
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
        selected = true
        action = Action("Purely Functional") 
                  { appModel.setInterpreterType(Pure()) }
      }
      val rb2 = new RadioMenuItem("") {
        selected = false
        action = Action("Imperative (Parallel)") {
                    def diag = Dialog.showInput(
                        body, "Choose a number of threads", 
                        "Parallel Interpreter", Dialog.Message.Question, 
                        Swing.EmptyIcon, Seq(), lastNumberOfThreads.toString)
                    def go : Unit = try {
                        def n : String = diag.getOrElse(n)
                        lastNumberOfThreads = Integer.parseInt(n)
                        appModel.setInterpreterType(Impure(lastNumberOfThreads))
                        console.log("Number of threads set to " + lastNumberOfThreads + ".")
                    } catch { case _ => 
                        console.logError("Bad number of threads.")
                        go 
                    }
                    go
                }
      }
      contents ++= Seq(rb1,rb2)
      new ButtonGroup(rb1,rb2)
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
    size = new Dimension(1024,700)
    override def closeOperation() {     
    exit
     }
    
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
    timer3d.destroy=true
    receiver.destroy=true
    threeDView.exit
    if (!editedSinceLastSave || confirmContinue(body.peer))
	      quit
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
  listenTo(receiver)
  

  def reflectState = {
    play.enabled = appModel.playEnabled
    pause.enabled = appModel.pauseEnabled
    stop.enabled = appModel.stopEnabled
    step.enabled = appModel.stepEnabled
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
    case StateChanged() => reflectState
    case Error(e)       => reportError(e)
    case Progress(p)    => statusZone.setProgress(p)
    case Progress3d(p)  => {statusZone3d.setProgress(p);
														statusZone3d.setTime((p.toFloat/100)*endTime.toFloat)}
  }

  /* ----- initialisation ----- */
  
  reflectState
  listenDocument
  console.log("<html>Welcome to Acumen.<br/>"+
              "Please see LICENSE file for licensing details.</html>")
  console.newLine
  appModel.reset
}





  

