package acumen.ui.threeD
import java.awt.BorderLayout
import javax.swing.{JLabel, JOptionPane, JPanel, JTextField}

import acumen.{CId, Main}

import scala.collection.mutable
import acumen.ui.{App, Controller, Icons}

import scala.swing._
import java.awt.event.MouseAdapter
import java.awt.event.MouseEvent

import com.threed.jpct.SimpleVector

case class ThreeDTab (appModel: Controller) extends BorderPanel {
  val canvasPanel = new JPanel
  val threeDView = new ThreeDView

  def createCanvas() = {
    if (check.selected)
      threeDView.axisOn()
    threeDView.init()
    threeDView.defaultView()
    canvasPanel.setLayout(new BorderLayout())
    canvasPanel.add(threeDView, BorderLayout.CENTER)
    peer.add(canvasPanel, BorderLayout.CENTER)
    peer.setVisible(true)
  }
  var playSpeed = 1.0
  val faster = new Action("faster") {
    icon = Icons.faster
    def apply() = {
      playSpeed = playSpeed * 2
      if (playSpeed > 4) playSpeed = 4 // maximum *4 speed
      timer3d.sleepTime = timer3d.initSpeed / playSpeed
      // Recalculate sleep time
      timer3d.extraTime = ((timer3d.sleepTime - timer3d.sleepTime.toLong) * 1000000).toInt
      statusZone3d.setSpeed(playSpeed.toString) // Show the speed
    }
  }
  val slower = new Action("slower") {
    icon = Icons.slower
    def apply() = {
      playSpeed = playSpeed / 2
      timer3d.sleepTime = (1 / playSpeed) * timer3d.initSpeed
      // Recalculate sleep time
      timer3d.extraTime = ((timer3d.sleepTime - timer3d.sleepTime.toLong) * 1000000).toInt
      statusZone3d.setSpeed(playSpeed.toString) // show the speed
    }
  }
  val stop3d = new Action("stop") {
    icon = Icons.stop
    def apply() = stop()
    toolTip = "Stop visualizing"
  }
  /* ----3D-Visualization---- */
  var played = false
  var realTimePlayed = false

  // play/pause button
  val threedplay = new Action("play") {
    icon = Icons.play
    toolTip = "play"
    def apply() = {
      if (toolTip == "play" || realTimePlayed) {
        play()
        // after click play, this button become pause
        toolTip = "pause"
        icon = Icons.pause
      } else if (toolTip == "pause") {
        pause()
      } else if (toolTip == "resume") {
        pause()
      }
    }

  }

  // _3DDataBuffer: Where all the state is stored
  private var _3DDataBuffer = mutable.Map[Int,mutable.Map[(CId, Int),List[_]]]()
  var lastFrame = 0
  var endTime = 10.0

  val viewButtonSize = new Dimension(55, 55)
  val defaultView = new Action("defaultView") {
    icon = Icons.defaultView
    toolTip = "default view"
    def apply() = {
      threeDView.defaultView()
    }
  }
  val defaultViewButton = new Button(defaultView) {
    peer.setHideActionText(true)
    preferredSize = viewButtonSize
  }
  val frontView = new Action("frontView") {
    icon = Icons.frontView
    toolTip = "front view"
    def apply() = {
      threeDView.frontView()
    }
  }
  val frontViewButton = new Button(frontView) {
    peer.setHideActionText(true)
    preferredSize = viewButtonSize
  }
  val topView = new Action("topView") {
    icon = Icons.topView
    toolTip = "top view"
    def apply() = {
      threeDView.topView()
    }
  }
  val topViewButton = new Button(topView) {
    peer.setHideActionText(true)
    preferredSize = viewButtonSize
  }
  val rightView = new Action("rightView") {
    icon = Icons.rightView
    toolTip = "right view"
    def apply() = {
      threeDView.rightView()
    }
  }
  val rightViewButton = new Button(rightView) {
    peer.setHideActionText(true)
    preferredSize = viewButtonSize
  }

  val s = new Dimension(50, 40)
  val b3dplay = new Button(threedplay) {
    peer.setHideActionText(true)
    preferredSize = s
  }
  val b3dstop = new Button(stop3d) {
    peer.setHideActionText(true)
    preferredSize = s
  }
  val b3dfaster = new Button(faster) {
    peer.setHideActionText(true)
    preferredSize = s
  }
  val b3dslower = new Button(slower) {
    peer.setHideActionText(true)
    preferredSize = s
  }
  val check = new CheckBox("") {
    action = Action("Axis") {
      if (selected) threeDView.axisOn()
      else threeDView.axisOff()
    }
  }
  check.selected = false

  val missedDeadLine = new Label("   Missed deadlines:%.2f".format(threeDView.percentagemissDL * 100) + "%    ")
  missedDeadLine.border = Swing.EmptyBorder(2,0,0,0)
  missedDeadLine.visible = false
  val slackTime = new Label("Slack:%.2f".format(threeDView.averageSlack * 100) + "%  ")
  slackTime.border = Swing.EmptyBorder(2,0,0,0)
  slackTime.visible = false
  val checkMatchTime = new CheckBox("") {
    action = Action("Match Wall Clock") {
      if (selected) {
        missedDeadLine.visible = true
        slackTime.visible = true
        border = Swing.EmptyBorder(0,0,0,0)
      } else {
        missedDeadLine.visible = false
        slackTime.visible = false
        border = Swing.EmptyBorder(0,0,0,259)
      }
    }
  }
  checkMatchTime.border = Swing.EmptyBorder(0,0,0,259)
  checkMatchTime.selected = false
  checkMatchTime.enabled = false
  val checkRTAnimation = new CheckBox(""){
    action = Action("Stream Animation") {
      if (selected) {
        checkMatchTime.enabled = true
      } else {
        checkMatchTime.selected = false
        checkMatchTime.enabled = false
      }
    }
  }
  if (Main.enableRealTime) checkRTAnimation.doClick()

  val statusZone3d = new Slider3D
  statusZone3d.bar.peer.addMouseListener(new MouseAdapter{
    var wasPlayingBeforeMousePressed = threedplay.toolTip == "pause"
    override def mousePressed(e: MouseEvent) = {
      wasPlayingBeforeMousePressed = threedplay.toolTip == "pause"
      pauseOn()
    }
    override def mouseReleased(e: MouseEvent) =
      if (wasPlayingBeforeMousePressed && statusZone3d.bar.enabled)
        pauseOff()
  })

  val setCameraButton = new Button("Set") {
    action = new Action("Set") {
      def apply () = {
        threeDView.setPositionButton(true)
      }
    }
  }
  val setLookAtButton = new Button("Set") {
    action = new Action("Set") {
      def apply () = {
        threeDView.setPositionButton(false)
      }
    }
  }

  val cameraInfoPane = new BoxPanel(Orientation.Horizontal) {
    contents ++= Seq(threeDView.cameraPos, setCameraButton)
  }

  val lookAtInfoPane = new BoxPanel(Orientation.Horizontal) {
    contents ++= Seq(threeDView.lookAtPosition, setLookAtButton)
  }

  val positionInfoPane = new BoxPanel(Orientation.Vertical) {
    contents ++= Seq(cameraInfoPane, lookAtInfoPane)
  }

  val threeDViewPane = new BoxPanel(Orientation.Horizontal) {
    contents ++= Seq(defaultViewButton, frontViewButton, topViewButton, rightViewButton, positionInfoPane)
  }

  val threeDControlPane = new BoxPanel(Orientation.Horizontal) {
    contents ++= Seq(check, b3dplay, b3dstop, b3dslower, b3dfaster, statusZone3d)
  }

  val threeDInfoPane = new BoxPanel(Orientation.Horizontal) {
    contents ++= Seq(checkRTAnimation, checkMatchTime, missedDeadLine, slackTime)
  }

  val threeDBottomPane = new BoxPanel(Orientation.Vertical) {
    contents ++= Seq(threeDViewPane, threeDInfoPane, threeDControlPane)
  }

  var _receiver = new _3DDisplay(threeDView, statusZone3d, playSpeed,
    _3DDataBuffer, lastFrame, appModel.threeDData.endTime, appModel.threeDData._3DView)

  var timer3d = new ScalaTimer(receiver, appModel.threeDData.endTime, playSpeed)

  def receiver: _3DDisplay = _receiver

  def reset() = {
    receiver.stop()
    played = false
    receiver.destroy = true
    timer3d.destroy = true
    threeDView.reset()
    if (check.selected)
      threeDView.axisOn()
  }

  def stop(): Unit = {
    threedplay.toolTip = "play"
    threedplay.icon = Icons.play
    timer3d.destroy = true
    threeDView.reset()
    statusZone3d.bar.value = 0
    if (check.selected)
      threeDView.axisOn()
  }

  def pauseOn(): Unit = {
    timer3d.pause = false
    threedplay.icon = Icons.play
    threedplay.toolTip = "resume"
  }
  
  def pauseOff(): Unit = {
    timer3d.pause = true
    threedplay.icon = Icons.pause
    threedplay.toolTip = "pause"
  }

  def pause(): Unit =
    if (threedplay.toolTip == "pause")
      pauseOn()
    else if (threedplay.toolTip == "resume")
      pauseOff()

  def play(): Unit = {
    if (App.ui.codeArea.editedSinceLastRun) {
      stop()
      App.ui.runSimulation()
    }
    else {
      threedplay.toolTip = "pause"
      threedplay.icon = Icons.pause
      endTime = appModel.threeDData.endTime
      if (played) {
        receiver.stop()
        timer3d.destroy = true
        statusZone3d.setSpeed(playSpeed.toString)
      }
      // First time press "3D play" button,
      // copy the data from list to buffer to speed up
      if (!played) {
        _3DDataBuffer.clear()
        lastFrame = 0
        for ((frameNo, map) <- appModel.threeDData._3DData) {
          val temp = if (map != null) mutable.Map[(CId, Int), List[_]]()
                     else null
          if (temp != null) {
            for ((objectKey, valueList) <- map)
              temp += objectKey -> valueList
          }
          _3DDataBuffer += frameNo -> temp
        }
        /* The frame start from 0, and end up at the last index of buffer */
        lastFrame = appModel.threeDData._3DData.size - 1
      }
      threeDView.viewStateMachine("deleteAllObjects")
      threeDView.objects.clear()
      threeDView.objectsCopy.clear()
      threeDView.scaleFactors.clear()
      threeDView.axisArray(0) = null
      if (check.selected)
        threeDView.axisOn()
      _receiver = new _3DDisplay(threeDView, statusZone3d, playSpeed,
                          _3DDataBuffer, lastFrame, appModel.threeDData.endTime,
                          appModel.threeDData._3DView)
      timer3d = new ScalaTimer(receiver, appModel.threeDData.endTime, playSpeed)
      receiver.start()
      timer3d.start()
      listenTo(receiver)
      receiver.listenTo(statusZone3d.bar.mouse.moves)
      receiver.listenTo(timer3d)
      timer3d.listenTo(statusZone3d.bar.mouse.clicks)
      timer3d.listenTo(statusZone3d.bar.mouse.moves)
      played = true
      realTimePlayed = false
    }
  }

  def playinRealTime() = {
    threedplay.toolTip = "play"
    threedplay.icon = Icons.play
    endTime = appModel.threeDData.endTime
    statusZone3d.setSpeed(playSpeed.toString)
    lastFrame = appModel.threeDData._3DData.size - 1
    _receiver = new _3DDisplay(threeDView, statusZone3d, playSpeed,
      appModel.threeDData._3DData, lastFrame, appModel.threeDData.endTime,
      appModel.threeDData._3DView)
    receiver.start()
    receiver ! "real time render"
    listenTo(receiver)
    receiver.listenTo(statusZone3d.bar.mouse.moves)
    receiver.listenTo(statusZone3d.bar.mouse.clicks)
    played = false
    realTimePlayed = true
  }

  def enableButtons() = setButtons(true)
  def disableButtons() = setButtons(false)
  def setButtons(targetState: Boolean): Unit = {
    threeDView.barEnabled = targetState
    b3dplay.enabled = targetState
    b3dstop.enabled = targetState
    statusZone3d.bar.enabled = targetState
    setCameraButton.enabled = targetState
    setLookAtButton.enabled = targetState
  }

  def checkBoxState(boxType: String): Boolean = {
    if (boxType == "realTime") {
      if (checkRTAnimation.selected) true
      else false
    } else {
      if (checkMatchTime.selected) true
      else false
    }
  }

  def setCheckBoxState(targetState: Boolean, boxType: String) = {
    if (boxType == "realTime") {
      checkRTAnimation.selected = targetState
    } else {
      checkMatchTime.selected = targetState
    }
  }

  def setCheckBoxes(targetState: Boolean) = {
    checkRTAnimation.enabled = targetState
    checkMatchTime.enabled = targetState
  }
  
  // Final Init
  createCanvas()
  add(threeDBottomPane, BorderPanel.Position.South)
}


