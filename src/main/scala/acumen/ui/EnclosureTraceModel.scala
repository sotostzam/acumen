package acumen
package ui

import Errors._
import Pretty._
import collection.mutable.ArrayBuffer
import javax.swing.event.TableModelListener
import acumen.interpreters.enclosure.UnivariateAffineEnclosure
import acumen.interpreters.enclosure.UnivariateAffineScalarEnclosure

class EnclosureTraceModel(es: Seq[UnivariateAffineEnclosure]) extends AbstractTraceModel {

  override def getRowCount() = es.size + 1
  override def getColumnCount() = es.head.varNames.size

  override def getValueAt(row: Int, column: Int) = "" 
//  {
//    getDouble(row, column) match {
//      case Some(v) => "[%f,%f]".format(v * 0.90, v * 1.10)
//      case None => tm.getValueAt(row, column)
//    }
//  }

  override def getDouble(row: Int, column: Int) = Some(0) 

  override def getColumnName(col: Int) = es.head.varNames.toIndexedSeq(col) 

  override def isEmpty() = es.isEmpty

  override def getTimes() =
    es.map(_.domain.hiDouble).foldLeft(ArrayBuffer(es.head.domain.loDouble)) { case (res, t) => res += t }

  override def getPlottables() = {
    // FIXME horrible solution, fix a.s.a.p.
    val enclSeqs = es.head.varNames.zipWithIndex.map{case (name,idx) => (name, idx, null +: es.map(_(name).toEnclosure))}
    enclSeqs.map {
      case (name, idx, enclosures) =>
        new PlotEnclosure(false, Name(name, 0), 0, idx, enclosures.toIndexedSeq)
    }
  }

}
