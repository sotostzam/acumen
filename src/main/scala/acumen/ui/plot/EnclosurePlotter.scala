package acumen
package ui
package plot

import acumen.interpreters.enclosure._
import java.awt.{ BasicStroke, Color }
import org.jfree.chart._
import org.jfree.chart.plot._
import org.jfree.chart.renderer.xy.XYDifferenceRenderer
import org.jfree.data.xy._
import org.jfree.ui.ApplicationFrame
import scala.collection.JavaConversions._
import scala.collection.mutable.{ Buffer, Map }
import acumen.interpreters.enclosure.affine.UnivariateAffineEnclosure

class EnclosurePlotter extends JFreePlotter {

  val enclosureRen = renderer(Color.red)

  def createEnclosureDataset(intervalStart: Double,
                             intervalEnd: Double,
                             upperApproximation: Double => Double,
                             lowerApproximation: Double => Double,
                             //stepSize: Double,
                             extraSamples: Int,
                             legendLabel: String,
                             fun: Double => Double): XYSeriesCollection = {
    val lower: XYSeries = new XYSeries(legendLabel, false, false)
    val upper: XYSeries = new XYSeries("HIDE_ME", false, false)
    lower.add(intervalStart, lowerApproximation(intervalStart))
    upper.add(intervalStart, upperApproximation(intervalStart))
    for (i <- 1 to extraSamples) {
      val x = (intervalStart + intervalEnd) * (i.doubleValue) / (extraSamples + 1)
      lower.add(x, lowerApproximation(x))
      upper.add(x, upperApproximation(x))
    }
    lower.add(intervalEnd, lowerApproximation(intervalEnd))
    upper.add(intervalEnd, upperApproximation(intervalEnd))
    val res = new XYSeriesCollection()
    res.addSeries(lower)
    res.addSeries(upper)
    res.setIntervalWidth(0.01)
    res
  }

  def addEnclosure(intervalStart: Double,
                   intervalEnd: Double,
                   upperApproximation: Double => Double,
                   lowerApproximation: Double => Double,
                   //stepSize: Double,
                   extraSamples: Int,
                   color: Color,
                   legendLabel: String,
                   fun: Double => Double) {

    val (subPlot, numberOfDatasets) = subPlots.get(legendLabel) match {
      case Some(t) => t
      case None => {
        val p = initXYPlot(legendLabel)
        combinedPlot.add(p, 1)
        subPlotsList += p
        (p, 0)
      }
    }

    val e = createEnclosureDataset(intervalStart, intervalEnd,
      upperApproximation, lowerApproximation, extraSamples, //stepSize,
      legendLabel, fun)

    subPlot.setDataset(numberOfDatasets, e)
    subPlot.setRenderer(numberOfDatasets, enclosureRen)
    subPlots(legendLabel) = (subPlot, numberOfDatasets + 1)

    chartPanel.invalidate
  }

  def renderer(color: Color) = enclosureRenderer(color)

  //TODO Make this visually merge overlapping enclosures into a single area with one outline.
  def enclosureRenderer(color: Color) = {
    val semiTransparentColor = new Color(color.getRed(), color.getGreen(), color.getBlue(), 20)
    val ren = new XYDifferenceRenderer(Color.red, semiTransparentColor, false)
    ren.setStroke(new BasicStroke(1.0f))
    ren.setSeriesPaint(0, color)
    ren.setSeriesPaint(1, color)
    ren
  }

  def plotUAE(e: UnivariateAffineEnclosure, fun: Double => Double)(implicit rnd: Rounding) = {
    val color = Color.red
    for ((varName, it) <- e.components) {
      def low(t: Double) = it.low(t) match { case Interval(lo, _) => lo.doubleValue }
      def high(t: Double) = it.high(t) match { case Interval(_, hi) => hi.doubleValue }
      val dom = it.domain
      val (lo, hi) = dom match { case Interval(l, h) => (l.doubleValue, h.doubleValue) }
      addEnclosure(lo, hi, high, low, 0, color, varName, fun)
    }
  }

  def plot(es: Seq[UnivariateAffineEnclosure], fun: Double => Double)(implicit rnd: Rounding): Unit = {
    combinedPlot.setNotify(false)
    for (e <- es) plotUAE(e, fun)
    combinedPlot.setNotify(true)
  }

  // Plot into a new frame
  def plot(frametitle: String)(fun: Double => Double)(es: Seq[UnivariateAffineEnclosure])(implicit rnd: Rounding): Unit = {
    val frame = createFrame(frametitle)
    plot(es, fun)
  }

  def plotForSaving(es: Seq[UnivariateAffineEnclosure])(implicit rnd: Rounding): Unit = {
    initPlot
    plot(es, null)
  }

  val defRnd = new Rounding(Parameters.default)
  def addToPlot(d: Object) = {
    plot(d.asInstanceOf[Seq[UnivariateAffineEnclosure]], null)(defRnd)
  }

}

