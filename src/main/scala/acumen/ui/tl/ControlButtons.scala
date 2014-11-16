package acumen
package ui
package tl

import scala.swing._

class ControlButtons extends FlowPanel {

  /* ---- definitions ------ */

  val play = new Action("play") {
    icon = Icons.record
    def apply() = App.ui.runSimulation()
    toolTip = "Run Simulation"
  }
  val step = new Action("step") {
    icon = Icons.step
    def apply() = App.ui.stepSimulation()
    toolTip = "Compute one simulation step"
  }
  val pause = new Action("pause") {
    icon = Icons.pause
    def apply() = App.ui.controller ! Pause
    toolTip = "Pause simulation"
	
  }
  val stop = new Action("stop") {
    icon = Icons.stop
    def apply() = {
      App.ui.controller ! Stop
      App.ui.stopSimulation()
    }
    toolTip = "Stop simulation (cannot resume)"
  }

  val bPlay = new Button(play) { peer.setHideActionText(true) }
  val bStep = new Button(step) { peer.setHideActionText(true) }
  var bStop = new Button(stop) { peer.setHideActionText(true) }
  
  contents += bPlay
  contents += bStep
  contents += bStop
  
  listenTo(App.pub)
  reactions += {
    case st:App.State => 
      play.enabled  = st match {case _:App.Playing => false; case _ => true}
      stop.enabled  = st match {case App.Stopped => false; case _ => true}
      pause.enabled = st match {case _:App.Playing => true; case _ => false}
      step.enabled  = st match {case _:App.Ready => true; case _ => false}

      st match {
        case _:App.Ready =>
          bPlay.action = play
        case _:App.Playing =>
          bPlay.action = pause
      }
  }

}
