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

  var disable3D = true;

  override def main(args: Array[String]) {
    if (args.size > 1) {
      usage
    } else if (args.size == 1) {
      if (args(0) == "--enable-3d")
        disable3D = false
      else if (args(0) == "--disable-3d")
        disable3D = true
      else
        usage
    }
    super.main(args)
  }

  def usage() {
    System.err.println("Error: Only accepted options are --enable-3d or --disable-3d.")
    exit
  }

  def top = {
    App.init
    val ret = App.ui.top

    ret
  }
}

