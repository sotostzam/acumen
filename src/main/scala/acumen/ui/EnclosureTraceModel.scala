package acumen
package ui

import Errors._
import Pretty._
import collection.mutable.ArrayBuffer
import javax.swing.event.TableModelListener
import acumen.interpreters.enclosure.UnivariateAffineEnclosure
import acumen.interpreters.enclosure.UnivariateAffineScalarEnclosure

class EnclosureTraceModel(es: Seq[UnivariateAffineEnclosure]) extends AbstractTraceModel {

  lazy val times = es.map(_.domain.hiDouble).foldLeft(ArrayBuffer(es.head.domain.loDouble)) { case (res, t) => res += t }

  lazy val enclSeqs = es.head.varNames.zipWithIndex.map{case (name,idx) => (name, idx, null +: es.map(_(name).toEnclosure))}

  lazy val plottables = enclSeqs.map {
    case (name, idx, enclosures) =>
      new PlotEnclosure(false, Name(name, 0), 0, idx, enclosures.toIndexedSeq)
  }

  override def getRowCount() = es.size + 1
  override def getColumnCount() = es.head.varNames.size

  override def getValueAt(row: Int, column: Int) = ""

  override def getDouble(row: Int, column: Int) = Some(0) 

  override def getColumnName(col: Int) = es.head.varNames.toIndexedSeq(col)

  override def isEmpty() = es.isEmpty

  override def getTimes() = times

  override def getPlottables() = plottables
}
