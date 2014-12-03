package acumen.ui.threeD
import java.awt.BorderLayout
import javax.swing.JPanel
import acumen.CId

import scala.collection.mutable
import acumen.ui.{App, Controller, Icons}
import scala.swing._
import java.awt.event.MouseAdapter
import java.awt.event.MouseEvent

abstract class AbstractEditorTab extends BorderPanel{
  def receiver: Publisher
  def reset(): Unit
  def play(): Unit
  def pause(): Unit
  def setAnaglyph(flag: Boolean): Unit
  def setRealTimeRender(flag: Boolean): Unit
}

class ThreeDTab (val appModel: Controller) extends AbstractEditorTab{
  val canvasPanel = new JPanel
  val threeDView = new ThreeDView

  def createCanvas() = {
    if (check.selected)
      threeDView.axisOn()
    threeDView.init()
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

  // play/pause button
  val threedplay = new Action("play") {
    icon = Icons.play
    toolTip = "play"
    def apply() = {
      if (toolTip == "play") {
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
  check.selected = true

  def hide(button: Button) {
    button.peer.setEnabled(false)
  }

//  val threeDButtons =
//    new FlowPanel(FlowPanel.Alignment.Leading)(check, b3dplay,
//      b3dstop, b3dslower, b3dfaster)

  val statusZone3d = new Slider3D
  statusZone3d.bar.peer.addMouseListener(new MouseAdapter{
    var wasPlayingBeforeMousePressed = threedplay.toolTip == "pause"
    override def mousePressed(e: MouseEvent) = {
      wasPlayingBeforeMousePressed = threedplay.toolTip == "pause"
      pauseOn()
    }
    override def mouseReleased(e: MouseEvent) =
      if (wasPlayingBeforeMousePressed)
        pauseOff()
  })

  val threeDBottomPane = new BoxPanel(Orientation.Horizontal) {
    contents ++= Seq(check, b3dplay, b3dstop, b3dslower, b3dfaster, statusZone3d)
  }

  var _receiver = new _3DDisplay(threeDView, statusZone3d,
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
    if (!threeDView.customView)
      threeDView.customView = true
    if (check.selected)
      threeDView.axisOn()
  }

  def pauseOn(): Unit = {
    timer3d.pause = false
    threedplay.icon = Icons.play
    threedplay.toolTip = "resume"
    if (!threeDView.customView && !threeDView.preCustomView)
      threeDView.customView = true
  }
  
  def pauseOff(): Unit = {
    timer3d.pause = true
    threedplay.icon = Icons.pause
    threedplay.toolTip = "pause"
    if (threeDView.customView && !threeDView.preCustomView)
      threeDView.customView = false
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
      threeDView.preCustomView = true
      threeDView.customView = true
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
        statusZone3d.setSpeed("1.0")
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
      if (appModel.threeDData._3DView.size != 0) {
        threeDView.customView = false
        threeDView.preCustomView = threeDView.customView
      }
      threeDView.viewStateMachine("deleteAllObjects")
      threeDView.repaint()
      repaint()
      threeDView.objects.clear()
      threeDView.scaleFactors.clear()
      threeDView.axisArray(0) = null
      if (check.selected)
        threeDView.axisOn()
      _receiver = new _3DDisplay(threeDView, statusZone3d, _3DDataBuffer, lastFrame,
                                 appModel.threeDData.endTime, appModel.threeDData._3DView)
      timer3d = new ScalaTimer(receiver, appModel.threeDData.endTime, playSpeed)
      receiver.start()
      timer3d.start()
      listenTo(receiver)
      receiver.listenTo(statusZone3d.bar.mouse.moves)
      receiver.listenTo(timer3d)
      timer3d.listenTo(statusZone3d.bar.mouse.clicks)
      timer3d.listenTo(statusZone3d.bar.mouse.moves)
      played = true
    }
  }

  def setAnaglyph(flag: Boolean) = {
    threeDView.enableAnaglyph = flag
  }

  def setRealTimeRender(flag: Boolean) = {
    threeDView.enableRealTime = flag
  }

  // Final Init
  createCanvas()
  add(threeDBottomPane, BorderPanel.Position.South)
}

class DisabledEditorTab(msg: String) extends AbstractEditorTab {
  def receiver = null
  def reset() = {}
  def play() = {}
  def setAnaglyph(flag: Boolean) = {}
  def setRealTimeRender(flag: Boolean) = {}
  def pause() = {}
  val msgBox = new TextArea("\n" + msg)
  msgBox.editable = false
  msgBox.lineWrap = true
  msgBox.peer.setWrapStyleWord(true)
  add(msgBox,BorderPanel.Position.Center)
}


