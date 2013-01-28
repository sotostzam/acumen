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

object ThreeDState extends Enumeration {
  val ERROR,DISABLE,LAZY,ENABLE = Value
}

object GraphicalMain extends SimpleSwingApplication {
  // Swing debugging
  javax.swing.RepaintManager.setCurrentManager(new debug.CheckThreadViolationRepaintManager)
  debug.EventDispatchThreadHangMonitor.initMonitoring()

  val osname = System.getProperty("os.name").toLowerCase
  var need_quartz = false
  // ThreeDStates default to lazy as loading it can cause performance
  // problems on some platforms (mainly Linux).  On Windows load it
  // eagerly as the 3D code was tested the most on Windows.  On OS X
  // load it eagerly as loading as loading lazily can cause a crash
  // if the libraries are not available.
  var threeDState = ThreeDState.LAZY
  if (osname.startsWith("windows ")) {
    threeDState = ThreeDState.ENABLE
  } else if (osname.contains("os x")) {
    if (!(System.getProperty("apple.awt.graphics.UseQuartz") == "true")) {
      threeDState = ThreeDState.DISABLE
      need_quartz = true
    } else {
      threeDState = ThreeDState.ENABLE
    }
  }
  var disableNewPlot = true
  var openFile: File = null
  var autoPlay = false
  var useEnclosures = false
  var useCompletion = false
  var useTemplates = false
  var dontFork = false

  def parseOpts(args: List[String]) {
    args match {
      case Nil =>
      case ("--enable-3d" | "--3d") :: tail => 
        threeDState = ThreeDState.ENABLE; parseOpts(tail)
      case ("--lazy-3d") :: tail => 
        threeDState = ThreeDState.LAZY; parseOpts(tail)
      case ("--disable-3d" | "--no-3d") :: tail => 
        need_quartz = false
        threeDState = ThreeDState.DISABLE; parseOpts(tail)
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
      case "--templates" :: tail =>
        useTemplates = true; parseOpts(tail)
      case "--dont-fork" :: tail =>
        dontFork = true; parseOpts(tail)
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

  // Approximation of the amount of heap available when setting heap size to 1g
  val MIN_MAX_MEM = (1024*7/8)*1024*1024

  def maybeFork(args: Array[String]) {
    val maxMem = Runtime.getRuntime().maxMemory()
    val shouldFork = maxMem < MIN_MAX_MEM || need_quartz
    val mayFork = !dontFork
    if (shouldFork && mayFork) {
      val separator = System.getProperty("file.separator");
      val classpath = System.getProperty("java.class.path");
      val path = System.getProperty("java.home") + separator + "bin" + separator + "java";
      val realArgs = new java.util.ArrayList[String]();
      val bean = java.lang.management.ManagementFactory.getRuntimeMXBean();
      realArgs.add(path)
      realArgs.addAll(bean.getInputArguments())
      realArgs.add("-Xmx1g")
      val quartz = if (need_quartz) "-Dapple.awt.graphics.UseQuartz=true" else ""
      if (need_quartz) realArgs.add(quartz)
      realArgs.add("-cp")
      realArgs.add(classpath)
      realArgs.add("acumen.ui.GraphicalMain")
      realArgs.addAll(args.toList)
      realArgs.add("--dont-fork")
      val processBuilder = new ProcessBuilder(realArgs)
      System.err.println("Forking new JVM with: " + processBuilder.command.mkString(" "))
      System.err.println("  to avoid use --dont-fork or start java with -Xmx1g " + quartz)
      try {
        val process = processBuilder.start();
        inheritIO(process.getInputStream(), System.out)
        inheritIO(process.getErrorStream(), System.err)
        val rv = process.waitFor()
        if (rv == 0)
          exit(0);
        System.err.println("Fork failed with exit code: " + rv);
      } catch {
        case e => System.err.println("Fork failed with error: " + e.getMessage())
      }
      System.err.println("Continuing anyway, acumen may be slow...")
    }
  }

  def inheritIO(src:InputStream, dest:PrintStream) {
    new Thread(new Runnable() {
      def run() {
        val buf = new Array[Byte](1024)
        var c = 0
        while ({c = src.read(buf); c > 0}) {
          dest.write(buf,0,c)
        }
      }
    }).start();
  }

  override def main(args: Array[String]) {
    parseOpts(args.toList)
    maybeFork(args)
    super.main(args)
  }

  def top = {
    App.init
    val ret = App.ui.top

    ret
  }
}

