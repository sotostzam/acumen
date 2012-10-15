package acumen
package ui

import Errors._
import Pretty._

import collection.mutable.ArrayBuffer
import javax.swing.event.TableModelListener

class TraceModelProxy(private var tm: AbstractTraceModel) extends AbstractTraceModel {

  private var listeners = new ArrayBuffer[TableModelListener]

  override def incSeqNum() = tm.incSeqNum()

  override def addTableModelListener(obj:TableModelListener) = {
    tm.addTableModelListener(obj)
    listeners += obj
  }

  override def fireTableStructureChanged() = {
    super.fireTableStructureChanged()
    tm.fireTableStructureChanged()
  }

  def setTraceModel(tm0: AbstractTraceModel) {
    tm = tm0; 
    for (l <- listeners) tm.addTableModelListener(l)
  }
 
  override def getRowCount() = tm.getRowCount()
  override def getColumnCount() = tm.getColumnCount()

  override def getValueAt(row:Int, column:Int) = tm.getValueAt(row,column)

  override def getDouble(row:Int, column:Int) = tm.getDouble(row,column)

  override def getColumnName(col:Int) = tm.getColumnName(col)

  override def isEmpty() = tm.isEmpty()

  override def getTimes() = tm.getTimes()

  override def getTraceViewTimes() = tm.getTraceViewTimes()
  
  override def getPlottables() = tm.getPlottables()
  
  override def addData(d: TraceData, seqNum: Int): Unit = tm.addData(d, seqNum)
		  
  override def reset = tm.reset
}
