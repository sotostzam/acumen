package acumen
package ui

import collection.mutable.ArrayBuffer

import javax.swing.table.TableModel
import javax.swing.table.AbstractTableModel

case class Plottable (simulator: Boolean, fn: Name,
                      startFrame: Int, values: ArrayBuffer[CValue])

trait AbstractTraceModel extends AbstractTableModel {

  def getDouble(row:Int, column:Int) : Option[Double]

  def isEmpty() : Boolean;

  def getTimes() : ArrayBuffer[Double]
  
  def getPlottables() : Iterable[Plottable]
}

