package acumen
package ui
package interpreter

import scala.collection.immutable._
import java.awt.Color

sealed abstract class Plottable(val simulator: Boolean, 
                                val key: ResultKey,
                                val startFrame: Int, // fixme: rename to offset?
                                val column: Int, /* column in trace table */
                                val palette: Palette = Palette() )
{
  def values : IndexedSeq[Any]
}

case class Palette(val value: Color = Color.red, val yAxis: Color = Color.blue, val boundingBox: Color = Color.white, val title: Color = Color.gray)

class PlotDoubles(simulator: Boolean, key: ResultKey, startFrame: Int, column: Int,
                  val v: IndexedSeq[Double]) extends Plottable(simulator,key,startFrame,column)
{
  override def values : IndexedSeq[Double] = v;
}
class PlotDiscrete(simulator: Boolean, key: ResultKey, startFrame: Int, column: Int,
                   val v: IndexedSeq[GValue]) extends Plottable(simulator,key,startFrame,column,Palette(yAxis = Color.white))
{
  override def values = v;
}
case class Enclosure(loLeft:Double, hiLeft:Double, loRight:Double, hiRight:Double)
class PlotEnclosure(simulator: Boolean, key: ResultKey, startFrame: Int, column: Int,
                    val v: IndexedSeq[Enclosure]) extends Plottable(simulator,key,startFrame,column)
{
  override def values : IndexedSeq[Enclosure] = v;
}

abstract class TraceData(val curTime : Double, val endTime : Double) extends collection.Iterable[Object]

case class PlotParms(plotSimulator: Boolean = false, 
		     plotNextChild: Boolean = false, 
		     plotSeeds: Boolean = false)

trait PlotModel {
  //There is two versions for each accessor :
  //  - The first with a tag is the one which respect the data structure
  //  - The second without a tag acces the set of stores as id they were concatenated next to each other to form an only Store
  def getRowCount : Int
  def getRowCount(tag: Tag) : Int
  def getValueAt(row:Int, column:Int) : String
  def getValueAt(tag: Tag, row:Int, column:Int) : String
  def getPlotTitle(col:Int) : String
  def getPlotTitle(tag: Tag, col:Int) : String
  def getDoubleAt(row:Int, column:Int): Option[Double]
  def getDoubleAt(tag: Tag, row:Int, column:Int): Option[Double]
  def getBoundingAt(row:Int, column:Int) = {
    getDoubleAt(row, column) match {
      case Some(v) => Some(v, v)
      case None => None
    }
  }
  def getBoundingAt(tag: Tag, row:Int, column:Int) = {
    getDoubleAt(tag, row, column) match {
      case Some(v) => Some(v, v)
      case None => None
    }
  }
  def isEmpty: Boolean
  def isEmpty(tag: Tag): Boolean
  def getTags: Set[Tag]
  def getDeadTags: Set[Tag]
  /** The time accessor needs a tag to work because there is no unified time.
    * getTimes() is expected to have one more element than the data
    * that is being plotted when the plottables are enclosures */
  def getTimes(t: Tag): IndexedSeq[Double]
  /* The optional set of tags allows to select the plottables to be displayed */
  def getPlottables(parms: PlotParms, tags: Set[Tag] = Set.empty): Iterable[Plottable]
  def getProba: Option[ProbaData]
  // FIXME Not a good place for a writing accessor.
  def setProba(pd: Option[ProbaData]): Unit
}

// Follows the same guidelines as the PlotModel
abstract class TraceModel extends javax.swing.table.AbstractTableModel {}

trait InterpreterModel
{
  // used temporary
  @volatile protected var _lastSeqNum : Int = 0
  def lastSeqNum = _lastSeqNum
  def incSeqNum() = {_lastSeqNum += 1; _lastSeqNum}

  // Add data to eventually be added to the models, Will be called by
  // the Consumer actor, should be fast.
  // Should use any necessary locks to prevent problems if
  // getPlotModel or getTraceModel is called at the same time by a
  // different thread.
  def addData(t: Tag, deadTag: Boolean, d: TraceData): Unit

  // Returns an updated plot model for plotting, the object returned
  // should be considered immutable.
  def getPlotModel : PlotModel

  // Returns new data to plot, the object returned should be
  // considered immutable.
  // (new way) -- FIXME: eliminate the need, the new plotter should
  // just the the plot model
  def getNewData : Object

  // Return an updated table model for the trace table.
  def getTraceModel : TraceModel

  //
  def getPlotter : acumen.ui.plot.JFreePlotter

  // flush pending data to avoid using too much memory if
  // no other method is called to trigger it
  def flush() = {}
}
