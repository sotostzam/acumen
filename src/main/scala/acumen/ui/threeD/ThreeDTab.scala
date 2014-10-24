package acumen.ui.threeD
import java.awt.BorderLayout
import javax.swing.JPanel

import acumen.CId
import acumen.ui.{App, Controller, Icons}

import scala.swing._

abstract class AbstractEditorTab extends BorderPanel{
  def receiver: Publisher
  def reset(): Unit
  def play(): Unit
  def pause(): Unit
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
  val threedpause = new Action("pause") {
    icon = Icons.pause
    toolTip = "pause"
    def apply() = pause()
  }
  val stop3d = new Action("stop") {
    icon = Icons.stop
    def apply() = stop()
    toolTip = "Stop visualizing"
  }
  /* ----3D-Visualization---- */
  var played = false

  val threedplay = new Action("play") {
    icon = Icons.play
    def apply() = play()
    toolTip = "play"
  }

  // _3DDataBuffer: Where all the state is stored
  var _3DDataBuffer = scala.collection.mutable.Map[CId, scala.collection.mutable.Map[Int, scala.collection.mutable.Buffer[List[_]]]]()
  var _3DView = List[(Array[Double], Array[Double])]()
  var lastFrame = 2.0
  var endTime = 10.0

  val s = new Dimension(50, 40)
  val b3dplay = new Button(threedplay) {
    peer.setHideActionText(true)
    preferredSize = s
  }
  val b3dpause = new Button(threedpause) {
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

  val threeDButtons =
    new FlowPanel(FlowPanel.Alignment.Leading)(check, b3dplay,
      b3dpause, b3dstop, b3dslower, b3dfaster)

  val statusZone3d = new Slider3D

  val threeDBottomPane = new BoxPanel(Orientation.Horizontal) {
    contents += threeDButtons
    contents += statusZone3d
  }

  var _receiver = new _3DDisplay(threeDView, statusZone3d,
    _3DDataBuffer, lastFrame, appModel.threeDData.endTime, appModel.threeDData._3DView.reverse)

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
    threedpause.toolTip = "pause"
    threedpause.icon = Icons.pause
    receiver.destroy = true
    timer3d.destroy = true
    threeDView.reset()
    if (!threeDView.customView)
      threeDView.customView = true
    if (statusZone3d.firstPlayed) {
      statusZone3d.bar.enabled = true
      statusZone3d.firstPlayed = false
    }
    if (check.selected)
      threeDView.axisOn()
  }

  def pause(): Unit = {
    if (threedpause.toolTip == "pause") {
      // un-pause
      timer3d.pause = false
      receiver.pause = true
      threedpause.icon = Icons.play
      threedpause.toolTip = "resume"
      if (!threeDView.customView && !threeDView.preCustomView)
        threeDView.customView = true
      if (statusZone3d.firstPlayed) {
        statusZone3d.bar.enabled = true
        statusZone3d.firstPlayed = false
      }
    } else {
      // pause
      timer3d.pause = true
      receiver.pause = false
      threedpause.icon = Icons.pause
      threedpause.toolTip = "pause"
      if (threeDView.customView && !threeDView.preCustomView)
        threeDView.customView = false
      if (statusZone3d.firstPlayed) {
        statusZone3d.bar.enabled = true
        statusZone3d.firstPlayed = false
      }
    }
  }


  def play(): Unit = {
    if (App.ui.codeArea.editedSinceLastRun) {
      stop()
      App.ui.runSimulation()
    }
    else {
      threedpause.toolTip = "pause"
      threedpause.icon = Icons.pause
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
        statusZone3d.setSpeed("1.0")
        statusZone3d.bar.enabled = false
        statusZone3d.firstPlayed = true
        for ((id, map) <- appModel.threeDData._3DData) {
          var temp = scala.collection.mutable.Map[Int, scala.collection.mutable.Buffer[List[_]]]()
          for ((objectNumber, l) <- map) {
            temp += (objectNumber -> l.reverse.toBuffer)
            temp(objectNumber).last(5) match {
              // The animation's length
              case n: Int => if (n > lastFrame) lastFrame = n
              case _ =>
                val n = temp(objectNumber).last(6).asInstanceOf[Int]
                if (n > lastFrame) lastFrame = n
            }
          }
          _3DDataBuffer += id -> temp
        }
        _3DView = appModel.threeDData._3DView.reverse
        appModel.threeDData.reset()
      }
      if (_3DView.size != 0) {
        threeDView.customView = false
        threeDView.preCustomView = threeDView.customView
        threeDView.reset()
        if (check.selected)
          threeDView.axisOn()
      }
      threeDView.objects.clear()
      threeDView.world.removeAllObjects()
      threeDView.scaleFactors.clear()
      threeDView.axisArray(0) = null
      if (check.selected)
        threeDView.axisOn()
      _receiver = new _3DDisplay(threeDView, statusZone3d, _3DDataBuffer, lastFrame,
                                 appModel.threeDData.endTime, _3DView)
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

  // Final Init
  createCanvas()
  add(threeDBottomPane, BorderPanel.Position.South)
}

class DisabledEditorTab(msg: String) extends AbstractEditorTab {
  def receiver = null
  def reset() = {}
  def play() = {}
  def pause() = {}
  val msgBox = new TextArea("\n" + msg)
  msgBox.editable = false
  msgBox.lineWrap = true
  msgBox.peer.setWrapStyleWord(true)
  add(msgBox,BorderPanel.Position.Center)
}


