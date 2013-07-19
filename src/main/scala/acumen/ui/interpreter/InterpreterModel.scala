package acumen
package ui
package interpreter

import scala.collection.immutable._

sealed abstract class Plottable(val simulator: Boolean, 
                                val fn: Name,  // fixme: rename
                                val startFrame: Int, // fixme: rename to offset?
                                val column: Int /* column in trace table */ )
{
  def values : IndexedSeq[Any]
}

class PlotDoubles(simulator: Boolean, fn: Name, startFrame: Int, column: Int,
                  val v: IndexedSeq[Double]) extends Plottable(simulator,fn,startFrame,column)
{
  override def values : IndexedSeq[Double] = v;
}

case class Enclosure(loLeft:Double, hiLeft:Double, loRight:Double, hiRight:Double)
class PlotEnclosure(simulator: Boolean, fn: Name, startFrame: Int, column: Int,
                    val v: IndexedSeq[Enclosure]) extends Plottable(simulator,fn,startFrame,column)
{
  override def values : IndexedSeq[Enclosure] = v;
}

abstract class TraceData(val curTime : Double, val endTime : Double) extends collection.Iterable[Object]

// PlotModel is immutable.  Access to members of this class can happen
// at unpredictable times and possible on the EDT; thus, all accesses
// should be fast and if at all possible lock-free.
trait PlotModel {
  def getRowCount() : Int
  def getValueAt(row:Int, column:Int) : String
  def getPlotTitle(col:Int) : String
  def getDouble(row:Int, column:Int): Option[Double]
  def isEmpty(): Boolean
  // getTimes() is expected to have one more element than the data
  // that is being plotted when the plottables are enclosures
  def getTimes(): IndexedSeq[Double] 
  def getTraceViewTimes() = getTimes()
  def getPlottables(): Iterable[Plottable]
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
  def addData(d: TraceData): Unit

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

}
