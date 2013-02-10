package acumen
package ui
package plot

import java.io.File
import javax.swing.{JComponent,JLabel,JSpinner}
import org.jfree.chart.{ChartPanel,JFreeChart}
import scala.collection.JavaConversions._
import scala.swing._
import scala.swing.event._
import util.Canonical._
import util.Conversions._

import Errors._
import PlotData._
import interpreter._

class JFreePlotTab extends BorderPanel
{
  var plotter : JFreePlotter = null

  var tabSelected = false
  var userDisabled = false
  var selfDisabled = false

  //val plotPanel = plotter.chartPanel

  // reasons for disabled
  // need to keep track of each one and handle the logic correctly
  //   too slow  // enable when stopped
  //   other tab // enable when active
  //   too many plots // enabled by user option
  //   user disabled  // enabled by user option


  // Need to handle the siteration when the plot was updating in the
  //  background, and the plot got reset.... 

  val resetZoom = new Action("Reset Zoom") {
    icon = Icons.home
    def apply = null
    //def apply = { plotPanel.restoreAutoBounds() }
    toolTip = "Reset View"
  }
  val zoomIn = new Action("Zoom In") {
    icon = Icons.zoomIn
    def apply = null
    //def apply = plotPanel.setZoomInFactor(plotPanel.getZoomInFactor() * 1.5) 
    toolTip = "Zoom In"
  }
  val zoomOut = new Action("Zoom Out") {
    icon = Icons.zoomOut
    def apply = null
    //def apply = plotPanel.setZoomInFactor(plotPanel.getZoomInFactor() / 1.5) 
    toolTip = "Zoom Out"
  }
  val undo = new Action("Undo") {
    icon = Icons.undo
    def apply = null
    //def apply = plotPanel.restoreAutoBounds() //TODO Implement undo for zooming
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
//      val cp = new SaveAsDailog
//      cp.pack
//      cp.open
    }

    class SaveAsDailog (parent: Component, chart:JFreeChart) extends Dialog(null) {
      modal = true
      val widthSpin = new JSpinner() { setValue(currentWidth) }
      val heightSpin = new JSpinner() { setValue(currentHeight) }
      val inputField = new TextField(fresh.getCanonicalPath,20)
      val openButton = Button("Browse") {
        val fc = new FileChooser(currentDir) {
          selectedFile = new File(inputField.text)
        }
        val returnVal = fc.showOpenDialog(JFreePlotTab.this)
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
//        ChartUtilities.saveChartAsPNG(f, chart, currentWidth, currentHeight)
        
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
  private val hint = new Label("Hint: Right click on image & drag to move") //TODO Make sure that this works
  /*private*/ val check = new CheckBox("") { 
    action = Action("Plot") {
      userDisabled = !selected
      fixPlotState
    }
  }
  private val rightBottomButtons =
    new FlowPanel(FlowPanel.Alignment.Leading)(check, b5, b1, b2, b3, b4, hint)
  def buttonsEnabled(v: Boolean) = {
    b1.enabled = v; b2.enabled = v; b3.enabled = v; b4.enabled = v; b5.enabled = v
  }
  def setPlotState = 
    App.ui.jPlotI.enabled = tabSelected && check.selected && !App.ui.jPlotI.tooSlow && !App.ui.jPlotI.forsedDisable
  def fixPlotState : Unit = {
    setPlotState
    if (App.ui.jPlotI.enabled)
      App.ui.plotView.plotPanel.plotter ! Refresh
    else if (plotter != null)
      plotter.detachChart
  }

  listenTo(App.pub) 
  reactions += {
    case App.Starting => 
      reset

    case _:App.Ready => 
      App.ui.jPlotI.tooSlow = false
      fixPlotState

    case App.ViewChanged(idx) => 
      if (idx == App.ui.views.NEW_PLOT_IDX) {
        tabSelected = true
        fixPlotState
      } else {
        tabSelected = false
        fixPlotState
      }
  }

  def reset = {
    App.ui.jPlotI.tooSlow = false
    App.ui.jPlotI.forsedDisable = false
    setPlotState
    plotter = App.ui.controller.model.getPlotter
    plotter.initPlot
    peer.removeAll
    add(Component.wrap(plotter.chartPanel), BorderPanel.Position.Center)
    add(check, BorderPanel.Position.South)
    check.selected = !userDisabled
    peer.revalidate()
  }

  //check.horizontalAlignment = Alignment.Right
  add(check, BorderPanel.Position.South)
  check.selected = true

  //Don't display for now, may do something better
  //add(rightBottomButtons, BorderPanel.Position.South)

  //TODO Check if the below need to be re-enabled
//  def toggleSimulator(b:Boolean) = plotPanel.toggleSimulator(b)
//  def toggleNextChild(b:Boolean) = plotPanel.toggleNextChild(b)
//  def toggleSeeds(b:Boolean) = plotPanel.toggleSeeds(b)
//  def setPlotStyle(ps:PlotStyle) = plotPanel.setPlotStyle(ps)
  
}
