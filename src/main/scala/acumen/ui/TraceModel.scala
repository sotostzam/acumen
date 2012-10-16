package acumen
package ui

import Errors._
import Pretty._

import javax.swing.table.AbstractTableModel

class TraceModel(var data: TraceModelData) extends AbstractTableModel {

  @volatile protected var lastSeqNum : Int = 0
  def incSeqNum() = {lastSeqNum += 1; lastSeqNum}

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

    // FIXME: Do This here?
    // FIXME: be more precise ?
    if (seqNum == lastSeqNum)  
      fireTableStructureChanged()
    else 
      println("SKIPPING THIS ROUND!")
    //fireTableDataChanged()
  }
}
