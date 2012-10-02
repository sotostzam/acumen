package acumen
package ui

import Errors._

import collection.mutable.ArrayBuffer
import javax.swing.table.TableModel
import javax.swing.table.AbstractTableModel

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

trait AbstractTraceModel extends AbstractTableModel {
  
  def getDouble(row:Int, column:Int): Option[Double]

  def isEmpty(): Boolean

  def getTimes(): IndexedSeq[Double]

  def getTraceViewTimes() = getTimes()

  def getPlottables(): Iterable[Plottable]
  
  def addData(d: TraceData): Unit = throw ShouldNeverHappen()

  def reset: Unit = {}
}
