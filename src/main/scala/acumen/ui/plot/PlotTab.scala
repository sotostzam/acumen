package acumen
package ui
package plot

import scala.io._
import collection.mutable.ArrayBuffer
import collection.mutable.Stack
import collection.JavaConversions._
import swing._
import swing.event._
import java.lang.Thread
import java.util.TimerTask
import java.util.Timer
import java.io.File
import java.awt.Color
import java.awt.Transparency
import java.awt.BasicStroke
import java.awt.RenderingHints
import java.awt.GraphicsEnvironment
import java.awt.Shape
import java.awt.AlphaComposite
import java.awt.geom.Area
import java.awt.geom.Point2D
import java.awt.geom.Line2D
import java.awt.geom.Rectangle2D
import java.awt.geom.Ellipse2D
import java.awt.geom.Path2D
import java.awt.geom.AffineTransform
import java.awt.image.BufferedImage
import javax.swing.event.TableModelEvent
import javax.swing.event.TableModelListener
import javax.swing.JSpinner
import javax.swing.JLabel
import javax.imageio.ImageIO
import Errors._
import util.Canonical._
import util.Conversions._
import interpreter._


sealed abstract class PlotStyle
case class Lines() extends PlotStyle
case class Dots() extends PlotStyle
case class Both() extends PlotStyle

class PlotTab extends BorderPanel
{
  /* for some reason forwarding events causes stack overflows, so we pass
     "this" to Plotter, which will ask this to publish events ... */
  val plotPanel = new PlotPanel(this)

  val resetZoom = new Action("Reset Zoom") {
    icon = Icons.home
    def apply = {plotPanel.plotter ! Repaint }
    toolTip = "Reset View"
  }
  val zoomIn = new Action("Zoom In") {
    icon = Icons.zoomIn
    def apply = plotPanel.zoom(1.5)
    toolTip = "Zoom In"
  }
  val zoomOut = new Action("Zoom Out") {
    icon = Icons.zoomOut
    def apply = plotPanel.zoom(1/1.5) 
    toolTip = "Zoom Out"
  }
  val undo = new Action("Undo") {
    icon = Icons.undo
    def apply = plotPanel.undo
    toolTip = "Undo Previous Action"
  }
  val saveAs = new Action("Save As") {
    icon = Icons.save
    tooltip = "Save As"
    private var currentDir = new File(".")
    private def fresh = Files.getFreshFile(currentDir, "png")
    private var currentWidth = 640
    private var currentHeight = 480

    def apply : Unit = {
      val cp = new SaveAsDailog
      cp.pack
      cp.open
    }

    class SaveAsDailog extends Dialog(null) {
      modal = true
      val widthSpin = new JSpinner() { setValue(currentWidth) }
      val heightSpin = new JSpinner() { setValue(currentHeight) }
      val inputField = new TextField(fresh.getCanonicalPath,20)
      val openButton = Button("Browse") {
        val fc = new FileChooser(currentDir) {
          selectedFile = new File(inputField.text)
        }
        val returnVal = fc.showOpenDialog(PlotTab.this)
        if (returnVal == FileChooser.Result.Approve) {
          val file = fc.selectedFile
          inputField.text = file.getAbsolutePath
        }
      }
      val upperPane = new GridPanel(2,2) {
        border = javax.swing.BorderFactory.createEmptyBorder(5,5,5,5)
        vGap = 5
        peer.add(new JLabel("Width"))
        peer.add(widthSpin)
        peer.add(new JLabel("Height"))
        peer.add(heightSpin)
      }
      val lowerPane =
        new FlowPanel(FlowPanel.Alignment.Leading)(inputField,openButton)
      val options = new BoxPanel(Orientation.Vertical) {
        contents += (upperPane, lowerPane)
      }
      val cancel = Button("Cancel")(dispose)
      val save = Button("Save") {
        val f = new File(inputField.text)
        currentDir = f.getParentFile
        currentHeight  = heightSpin.getValue.asInstanceOf[Int]
        currentWidth = widthSpin.getValue.asInstanceOf[Int]
        plotPanel.render(f, currentWidth, currentHeight)
        dispose
      }
      val buttons = new FlowPanel(FlowPanel.Alignment.Trailing)(cancel,save)
      contents = new BorderPanel {
        add(options, BorderPanel.Position.Center)
        add(buttons, BorderPanel.Position.South)
      }
    }
  }

  private val b1 = new Button(resetZoom) { peer.setHideActionText(true) }
  private val b2 = new Button(undo) { peer.setHideActionText(true) }
  private val b3 = new Button(zoomIn) { peer.setHideActionText(true) }
  private val b4 = new Button(zoomOut) { peer.setHideActionText(true) }
  private val b5 = new Button(saveAs) { peer.setHideActionText(true) }
  buttonsEnabled(false)
  private val hint = new Label("Hint: Right click on image & drag to move")
  def updatePlotEnabled : Unit =
     plotPanel.plotI.enabled = tabSelected && check.selected && App.ui.controller.model != null
  var userDisabled = false
  def resetCheckBox {
    if (!userDisabled)
      check.selected = true
    plotPanel.plotI.disableThreshold = plotPanel.plotI.DISABLE_THRESHOLD
  }
  /*private*/ val check = new CheckBox("") { 
    action = Action("Plot") {
      updatePlotEnabled
      if (selected)
        plotPanel.plotI.disableThreshold = Integer.MAX_VALUE
      plotPanel.plotter ! Replot
      userDisabled = !selected
    }
  }
  private val rightBottomButtons =
    new FlowPanel(FlowPanel.Alignment.Leading)(check, b5, b1, b2, b3, b4, hint)
  def buttonsEnabled(v: Boolean) = {
    b1.enabled = v; b2.enabled = v; b3.enabled = v; b4.enabled = v; b5.enabled = v
  }
  listenTo(App.pub) 
  var appState : App.State = null
  var plotState : Plotter.State = null
  var panelState : PlotPanel.State = null
  var tabSelected = false
  reactions += {
    case st:App.State       => appState = st; stateUpdateResponse
    case st:Plotter.State   => plotState = st; stateUpdateResponse
    case st:PlotPanel.State => panelState = st; stateUpdateResponse

    case App.ViewChanged(idx) => 
      if (idx == App.ui.views.PLOT_IDX) {
        tabSelected = true
        updatePlotEnabled
        plotPanel.plotter ! Replot
      } else {
        tabSelected = false
        updatePlotEnabled
      }

    case m:PlotReady =>
      if (m.data.disableThreshold == Integer.MAX_VALUE)
        plotPanel.plotI.disableThreshold = m.data.plottables.size + plotPanel.plotI.DISABLE_THRESHOLD
      else if (m.data.disabled) 
        check.selected = false
  }
  def stateUpdateResponse = {
    if (appState == App.Starting) resetCheckBox
    updatePlotEnabled
    (plotState,appState) match {
      case _ if panelState == PlotPanel.Disabled => 
        buttonsEnabled(false)
      case (Plotter.Busy,_:App.Playing) => 
        buttonsEnabled(false)
      case (Plotter.Ready,_:App.Ready) if panelState == PlotPanel.Enabled => 
        buttonsEnabled(true)
      case _ => 
    }
  }

  add(plotPanel, BorderPanel.Position.Center)
  add(rightBottomButtons, BorderPanel.Position.South)
  check.selected = true

  //tmodel.addTableModelListener(this)

  //private def fit = plotter.fit
  //override def tableChanged(e: TableModelEvent) = 
  //  if (check.selected) redraw

  def toggleSimulator(b:Boolean) = plotPanel.toggleSimulator(b)
  def toggleNextChild(b:Boolean) = plotPanel.toggleNextChild(b)
  def toggleSeeds(b:Boolean) = plotPanel.toggleSeeds(b)
  def setPlotStyle(ps:PlotStyle) = plotPanel.setPlotStyle(ps)
}

