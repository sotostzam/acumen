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
    def apply = {App.ui.controller.threeDData.reset; App.ui.threeDtab.reset; App.ui.codeArea.autoSave; App.ui.controller ! Play}
    toolTip = "Run Simulation"
  }
  val step = new Action("step") {
    icon = Icons.step
    def apply = { App.ui.codeArea.autoSave; App.ui.controller ! Step }
    toolTip = "Compute one simulation step"
  }
  val pause = new Action("pause") {
    icon = Icons.pause
    def apply = App.ui.controller ! Pause
    toolTip = "Pause simulation"
	
  }
  val stop = new Action("stop") {
    icon = Icons.stop    
    def apply = { println("Controller state = " + App.ui.controller.getState); App.ui.controller ! Stop; }
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
