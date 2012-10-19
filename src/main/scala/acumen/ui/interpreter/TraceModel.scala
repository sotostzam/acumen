package acumen
package ui
package interpreter

import Errors._
import Pretty._

import javax.swing.table.AbstractTableModel

class TraceModel(var data: TraceModelData) extends AbstractTableModel {

  @volatile protected var _lastSeqNum : Int = 0
  def lastSeqNum = _lastSeqNum
  def incSeqNum() = {_lastSeqNum += 1; _lastSeqNum}

  def getRowCount() = data.getRowCount()
  def getColumnCount() = data.getColumnCount()

  def getValueAt(row:Int, column:Int) = data.getValueAt(row,column)

  def getDouble(row:Int, column:Int) = data.getDouble(row,column)

  override def getColumnName(col:Int) = data.getColumnName(col)

  def isEmpty() = data.isEmpty()

  def getTimes() = data.getTimes()

  def getTraceViewTimes() = data.getTraceViewTimes()
  
  def getPlottables() = data.getPlottables()
  
  def addData(d: TraceData, seqNum: Int): Unit = {
    data.addData(d, seqNum == lastSeqNum)
  }
}
