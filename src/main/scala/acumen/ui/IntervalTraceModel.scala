package acumen
package ui

import Errors._
import Pretty._

import collection.mutable.ArrayBuffer
import javax.swing.event.TableModelListener

class IntervalTraceModel(tm: TraceModel) extends AbstractTraceModel {

  override def addTableModelListener(obj:TableModelListener) = tm.addTableModelListener(obj)
 
  override def getRowCount() = tm.getRowCount()
  override def getColumnCount() = tm.getColumnCount()

  override def getValueAt(row:Int, column:Int) = {
    getDouble(row, column) match {
      case Some(v) => "[%f,%f]".format(v*0.90,v*1.10)
      case None    => tm.getValueAt(row, column)
    }
  }

  override def getDouble(row:Int, column:Int) = tm.getDouble(row,column)

  override def getColumnName(col:Int) = tm.getColumnName(col)

  override def isEmpty() = tm.isEmpty()

  override def getTimes() = tm.getTimes()
  
  override def getPlottables() = {
    val plts = tm.getPlottables()
    var ret = new ArrayBuffer[Plottable]
    for (pl <- plts) {
      var vls = new IndexedSeq[Interval] {
        override def apply(idx: Int) = {
          val num = pl.values(idx)
          Interval(num*0.90,num*1.10)
        }
        override def length = pl.values.length
      }
      ret += new PlotIntervals(pl.simulator, pl.fn, pl.startFrame, pl.column, vls)
    }
    ret
  }
  
  def addStore(st:CStore) = {}
  
  def addStores(sts:Iterable[CStore]) = {}
  
  def reset = {}
}
