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

trait PlotModel {
  def getRowCount() : Int
  def getColumnCount() : Int
  def getValueAt(row:Int, column:Int) : String
  def getColumnName(col:Int) : String
  def getDouble(row:Int, column:Int): Option[Double]
  def isEmpty(): Boolean
  // getTimes() is expected to have one more element than the data
  // that is being plotted
  def getTimes(): IndexedSeq[Double]
  def getTraceViewTimes() = getTimes()
  def getPlottables(): Iterable[Plottable]
}

abstract class TraceModel extends javax.swing.table.AbstractTableModel {}

trait InterpreterModel {
  // used temporary
  @volatile protected var _lastSeqNum : Int = 0
  def lastSeqNum = _lastSeqNum
  def incSeqNum() = {_lastSeqNum += 1; _lastSeqNum}

  // Add data from the interpreter.  Must not invalid any live Plot
  // models, which will be accessed in another thread.  Will be
  // called with a lock on this.
  def addData(d: TraceData): Unit

  // Returns an updated plot model for plotting.  If a new object is
  // returned than any previous models are invalided.  Will be called
  // with a lock on this.
  def getPlotModel : PlotModel

  // Return a new table model for the trace table.  Will be called on
  // the EDT, so it should be fast.  Also, addData will not get called
  // while the returned object is live.
  def getTraceModel : TraceModel
}

