package acumen
package ui

import Errors._
import Pretty._

import collection.mutable.ArrayBuffer
import javax.swing.event.TableModelListener

class EnclosureTraceModel(tm: TraceModel) extends AbstractTraceModel {

  override def addTableModelListener(obj:TableModelListener) = tm.addTableModelListener(obj)
 
  override def getRowCount() = tm.getRowCount()
  override def getColumnCount() = tm.getColumnCount()

  override def getValueAt(row:Int, column:Int) = tm.getValueAt(row, column)

  override def getDouble(row:Int, column:Int) = tm.getDouble(row,column)

  override def getColumnName(col:Int) = tm.getColumnName(col)

  override def isEmpty() = tm.isEmpty()

  override def getTimes() = tm.getTimes()
  
  override def getPlottables() = {
    val plts = tm.getPlottables()
    var ret = new ArrayBuffer[Plottable]
    for (pl <- plts) {
      var vls = new IndexedSeq[Enclosure] {
        override def apply(idx: Int) = {
          if (idx == 0) {
            null
          } else {
            val num0 = pl.values(idx-1)
            val num1 = pl.values(idx)
            Enclosure(num0*0.90,num0*1.10,num1*0.80,num1*1.2)
          }
        }
        override def length = pl.values.length
      }
      ret += new PlotEnclosure(pl.simulator, pl.fn, pl.startFrame, pl.column, vls)
    }
    ret
  }
  
}
