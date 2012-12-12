package acumen
package ui
package interpreter

import collection.{Iterable, IndexedSeq}

sealed abstract class Plottable(val simulator: Boolean, 
                                val fn: Name, 
                                val startFrame: Int,
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

abstract class TraceData(val curTime : Double, val endTime : Double) extends Iterable[Object]

// Accesses to certain members, such as getValueAt, can happen at
// unpredictable times and on the EDT.  These members should not
// block, see TraceModel for more info.  Accesses to the other
// members, such as getPlottable, will not be called while the model
// is being updated.  If for some reason these members are called as
// the model is being updated than it is acceptable to throw an
// exception.
trait PlotModel {
  // These mebers may be called on the EDT:
  def getRowCount() : Int
  def getValueAt(row:Int, column:Int) : String
  def getColumnName(col:Int) : String
  def getDouble(row:Int, column:Int): Option[Double]
  def isEmpty(): Boolean
  // These members will not get called when the model is updating:
  // getTimes() is expected to have one more element than the data
  // that is being plotted
  def getTimes(): IndexedSeq[Double]
  def getTraceViewTimes() = getTimes()
  def getPlottables(): Iterable[Plottable]
}

// Accesses to members of this class can happen at unpredictable times
// and always on the EDT.  Methods should never block waiting for a
// lock.  To avoid data contention issues values should be cached when
// possible and when that is not possible a dummy value should be
// returned if the model is being updated.
abstract class TraceModel extends javax.swing.table.AbstractTableModel {}

trait InterpreterModel
{
  // used temporary
  @volatile protected var _lastSeqNum : Int = 0
  def lastSeqNum = _lastSeqNum
  def incSeqNum() = {_lastSeqNum += 1; _lastSeqNum}

  // Add data to eventually be added to the models, Will be called by
  // the Consumer actor, should be fast.  Should not touch the data
  // used by the PlotModel or TraceModel in an thread unsafe way.
  // Should use any necessary locks to prevent problems if
  // getPlotModel or getTraceModel is called at the same time by a
  // different thread.
  // (new way: use necessary locks to avoid data contention issues
  // with getNewData)
  def addData(d: TraceData): Unit

  // Returns an updated plot model for plotting.
  // (old way)
  def getPlotModel : PlotModel

  // Returns new data to plot, the object returned should be
  // considered immutable.
  // (new way)
  def getNewData : Object

  // Return an updated table model for the trace table.
  def getTraceModel : TraceModel

  //
  def getPlotter : acumen.ui.plot.JFreePlotter

}
