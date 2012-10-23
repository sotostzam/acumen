package acumen
package ui
package tl

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

class ControlButtons extends FlowPanel {

  /* ---- definitions ------ */

  val play = new Action("play") {
    icon = Icons.play
    def apply = {Acumen.ui.controller.threeDData.reset; Acumen.ui.threeDtab.reset; Acumen.ui.codeArea.autoSave; Acumen.ui.controller ! Play}
    toolTip = "Run Simulation"
  }
  val step = new Action("step") {
    icon = Icons.step
    def apply = { Acumen.ui.codeArea.autoSave; Acumen.ui.controller ! Step }
    toolTip = "Compute one simulation step"
  }
  val pause = new Action("pause") {
    icon = Icons.pause
    def apply = Acumen.ui.controller ! Pause
    toolTip = "Pause simulation"
	
  }
  val stop = new Action("stop") {
    icon = Icons.stop    
    def apply = { println("Controller state = " + Acumen.ui.controller.getState); Acumen.ui.controller ! Stop; }
    toolTip = "Stop simulation (cannot resume)"
  }

  val bPlay = new Button(play) { peer.setHideActionText(true) }
  val bStep = new Button(step) { peer.setHideActionText(true) }
  var bStop = new Button(stop) { peer.setHideActionText(true) }
  
  contents += bPlay
  contents += bStep
  contents += bStop

  // Fixme: consider elimiting, maybe should listen to a StateChange
  // event instead --kevina
  def setState(state: AppState) {
    play.enabled = state.playEnabled
    stop.enabled = state.stopEnabled 
    pause.enabled = state.pauseEnabled 
    step.enabled = state.stepEnabled 

    state.state match {
      case _:AppState.Ready =>
        bPlay.action = play
      case _:AppState.Playing =>
        bPlay.action = pause
    }
  }

}
