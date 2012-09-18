package acumen
package ui

import Errors._;

import collection.mutable.ArrayBuffer
import javax.swing.event.TableModelListener

class IntervalTraceModel(tm: TraceModel) extends AbstractTraceModel {

  override def addTableModelListener(obj:TableModelListener) = tm.addTableModelListener(obj)

  override def getRowCount() = tm.getRowCount()
  override def getColumnCount() = tm.getColumnCount()

  override def getValueAt(row:Int, column:Int) = tm.getValueAt(row,column)

  override def getDouble(row:Int, column:Int) = tm.getDouble(row,column)

  override def getColumnName(col:Int) = tm.getColumnName(col)

  override def isEmpty() = tm.isEmpty()

  override def getTimes() = tm.getTimes()
  
  override def getPlottables() = {
    val plts = tm.getPlottables()
    var ret = new ArrayBuffer[Plottable]
    for (pl <- plts) {
      var vls = new ArrayBuffer[CValue]
      for (v <- pl.values) {
        v match {
          case VLit(x@(GDouble(num))) => 
            vls += VLit(GInterval(num*0.90,num*1.10))
          case VLit(x@(GInt(num))) => 
            vls += VLit(GInterval(num*0.90,num*1.10))
          case _ =>
            vls += v
        }
      }
      ret += Plottable(pl.simulator, pl.fn, pl.startFrame, vls)
    }
    ret
  }
  
}
