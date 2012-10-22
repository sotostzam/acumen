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

trait TraceModelData {
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
  def addData(d: TraceData, updateCache: Boolean): Unit
}

// FIXME: Eventually Eliminate

object FakeTraceModelData extends TraceModelData {
  def getRowCount() = 0
  def getColumnCount() = 0
  def getValueAt(row:Int, column:Int) = ""
  def getColumnName(col:Int) = ""
  def getDouble(row:Int, column:Int) = None
  def isEmpty() = true
  def getTimes() = IndexedSeq()
  override def getTraceViewTimes() = IndexedSeq()
  def getPlottables(): Iterable[Plottable] = Iterable()
  def addData(d: TraceData, updateCache: Boolean) = {}
}
