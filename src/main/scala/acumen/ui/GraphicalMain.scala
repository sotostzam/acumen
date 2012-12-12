package acumen
package ui

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

object GraphicalMain extends SimpleSwingApplication {
  // Swing debugging
  javax.swing.RepaintManager.setCurrentManager(new debug.CheckThreadViolationRepaintManager)
  debug.EventDispatchThreadHangMonitor.initMonitoring()

  var disable3D = true
  var disableNewPlot = true
  var openFile: File = null
  var autoPlay = false
  var useEnclosures = false
  var useCompletion = false

  def parseOpts(args: List[String]) {
    args match {
      case Nil =>
      case ("--enable-3d" | "--3d") :: tail => 
        disable3D = false; parseOpts(tail)
      case ("--disable-3d" | "--no-3d") :: tail => 
        disable3D = true; parseOpts(tail)
      case ("--enable-newplot" | "--newplot") :: tail => 
        disableNewPlot = false; parseOpts(tail)
      case ("--disable-newplot" | "--no-newplot") :: tail => 
        disableNewPlot = true; parseOpts(tail)
      case "--example" :: name :: tail =>
        openFile = new File("examples", name + ".acm"); parseOpts(tail)
      case "--play" :: tail =>
        autoPlay = true; parseOpts(tail)
      case "--enclosures" :: tail =>
        useEnclosures = true; parseOpts(tail)
      case "--no-enclosures" :: tail =>
        useEnclosures = false; parseOpts(tail)
      case "--non-localizing" :: tail =>
        interpreters.enclosure.Interpreter.localizing = false; parseOpts(tail)
      case "--completion" :: tail =>
        useCompletion = true; parseOpts(tail)
      case opt ::  tail if opt.startsWith("-") =>
        System.err.println("Unrecognized Option: " + opt)
        exit(1)
      case fn :: tail => 
        openFile = new File(fn)
        if (!openFile.exists) {
          System.err.println("File not found: " + openFile)
          exit(1)
        }
        parseOpts(tail)
    }
  }

  override def main(args: Array[String]) {
    parseOpts(args.toList)
    super.main(args)
  }

  def top = {
    App.init
    val ret = App.ui.top

    ret
  }
}

