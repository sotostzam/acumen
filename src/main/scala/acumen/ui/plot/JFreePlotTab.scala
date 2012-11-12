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
import org.jfree.chart.plot.CombinedDomainXYPlot
import org.jfree.chart.axis.NumberAxis
import org.jfree.chart.plot.XYPlot
import org.jfree.chart.JFreeChart
import org.jfree.chart.ChartPanel
import org.jfree.chart.renderer.xy.XYDifferenceRenderer
import org.jfree.chart.ChartUtilities
import javax.swing.JComponent
import javax.swing.JPanel
import java.awt.BorderLayout
import org.jfree.data.xy.XYSeries
import org.jfree.data.xy.DefaultTableXYDataset
import scala.collection.mutable.Map
import org.jfree.ui.ApplicationFrame
import javax.swing.JFrame
import PlotData._
import scala.collection.mutable.LinkedHashSet
import com.itextpdf.text.Document
import com.itextpdf.text.pdf.PdfWriter
import java.io.FileOutputStream
import com.itextpdf.awt.DefaultFontMapper
import interpreters.enclosure.UnivariateAffineEnclosure
import scala.collection.Seq
import interpreters.enclosure.AbstractFrame
import interpreters.enclosure.Rounding

class JFreePlotTab extends BorderPanel
{
  // val combinedPlot = new CombinedDomainXYPlot(new NumberAxis("Time"))
  // val subPlots: Map[String, (XYPlot, Int)] = Map[String, (XYPlot, Int)]()
  // val chart = new JFreeChart("",JFreeChart.DEFAULT_TITLE_FONT, combinedPlot, false);
  // val plotPanel = new ChartPanel(chart, true, true, true, true, false)
  // plotPanel.setBackground(Color.white)
  // val enclosureRen = enclosureRenderer(Color.red)

  val plotPanel = new BoxPanel(Orientation.Vertical)

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
    action = Action("Draw") {
      //chart.setNotify(selected)
      //if (selected)
      //  chart.fireChartChanged()
    }
  }
  private val rightBottomButtons =
    new FlowPanel(FlowPanel.Alignment.Leading)(check, b5, b1, b2, b3, b4, hint)
  def buttonsEnabled(v: Boolean) = {
    b1.enabled = v; b2.enabled = v; b3.enabled = v; b4.enabled = v; b5.enabled = v
  }
  listenTo(App.pub) 
  // var appState : App.State = null
  // var plotState : Plotter.State = null
  // var panelState : PlotPanel.State = null
  // reactions += {
  //   case st:App.State       => appState = st; stateUpdateResponse
  //   case st:Plotter.State   => plotState = st; stateUpdateResponse
  //   case st:PlotPanel.State => panelState = st; stateUpdateResponse
  // }
  // def stateUpdateResponse = (plotState,appState) match {
  //   case _ if panelState == PlotPanel.Disabled => 
  //     buttonsEnabled(false)
  //   case (Plotter.Busy,_:App.Playing) => 
  //     buttonsEnabled(false)
  //   case (Plotter.Ready,_:App.Ready) if panelState == PlotPanel.Enabled => 
  //     buttonsEnabled(true)
  //   case _ => 
  // }
  reactions += {
    case st:App.Ready => plot
  }

  add(plotPanel, BorderPanel.Position.Center)
  //Don't display for now, may do something better
  //add(rightBottomButtons, BorderPanel.Position.South)
  check.selected = true

  def getModel() = try {
    App.ui.controller.model.getPlotModel 
  } catch {
    case _:java.lang.IllegalStateException => null
  }
  
  private def plot: Unit = {
    plotPanel.peer.removeAll()
    UnivariateAffineEnclosure.chartPanels.clear
    if (App.ui.controller.model == null) return
    val m = getModel
    if (m == null) return
    val es = m.getNewPlottables.asInstanceOf[Seq[UnivariateAffineEnclosure]]
    if (es == null) return
    println("New Plot Working!")
    def wrapper = new AbstractFrame {
      def add(c: JComponent) = plotPanel.peer.add(c)
      def invalidate = plotPanel.peer.invalidate
    }
    for (e <- es) UnivariateAffineEnclosure.plotUAE(e, wrapper, null)(new Rounding(10))
    plotPanel.peer.validate
    println("New Plot Done!")
  }

  
  
  //TODO Check if the below need to be re-enabled
//  def toggleSimulator(b:Boolean) = plotPanel.toggleSimulator(b)
//  def toggleNextChild(b:Boolean) = plotPanel.toggleNextChild(b)
//  def toggleSeeds(b:Boolean) = plotPanel.toggleSeeds(b)
//  def setPlotStyle(ps:PlotStyle) = plotPanel.setPlotStyle(ps)
  
}
