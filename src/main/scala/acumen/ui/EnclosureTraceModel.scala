package acumen
package ui

import Errors._
import Pretty._
import collection.mutable.ArrayBuffer
import collection.mutable.WrappedArray
import javax.swing.event.TableModelListener
import acumen.interpreters.enclosure.UnivariateAffineEnclosure
import acumen.interpreters.enclosure.UnivariateAffineScalarEnclosure

class EnclosureTraceModel(es: Seq[UnivariateAffineEnclosure]) extends AbstractTraceModel {

  lazy val times = es.map(_.domain.hiDouble).foldLeft(ArrayBuffer(es.head.domain.loDouble)) { case (res, t) => res += t }

  lazy val enclSeqs = es.head.varNames.zipWithIndex.map{case (name,idx) => (name, idx + 1, null +: es.map(_(name).toEnclosure))}

  lazy val plottables = enclSeqs.map {
    case (name, idx, enclosures) =>
      new PlotEnclosure(false, Name(name, 0), 0, idx, enclosures.toIndexedSeq)
  }

  lazy val (tableData,tableTimes) = {
    // first group duplicate times together
    
    var timeGrouping = ArrayBuffer[Tuple3[Double,Int,Int]]()
    var start = 0
    do {
      val time = times(start)
      var stop = start + 1
      while (stop < times.size && time == times(stop)) 
        stop += 1
      timeGrouping += ((time, start, stop))
      start = stop
    } while (start < times.size)

    // prep the table array
    val res = Array.fill(1+plottables.size){null:Array[String]}
    
    // fill in the first column with the time value
    res(0) = timeGrouping.map {case (t,_,_) => "%f".format(t)}.toArray

    // fill in the other columns
    for ((_, idx, enclosures) <- enclSeqs) {
      def getLeft(idx: Int) = {
        val (_,start,stop) = timeGrouping(idx)
        val data = enclosures.slice(start,stop)
        val lo = data.map {encl => encl.loLeft}.min
        val hi = data.map {encl => encl.hiLeft}.max
        "[%f,%f]".format(lo,hi)
      }
      def getRight(idx: Int) = {
        val (_,start,stop) = timeGrouping(idx)
        val data = enclosures.slice(start,stop)
        val lo = data.map {encl => encl.loRight}.min
        val hi = data.map {encl => encl.hiRight}.max
        "[%f,%f]".format(lo,hi)
      }
      val r = Array.fill(times.size){null:String}
      var i = 0
      r(i) = "(-,%s)".format(getRight(i+1))
      i += 1
      while (i < timeGrouping.size - 1) {
        r(i) = "(%s,%s)".format(getLeft(i),getRight(i+1))
        i += 1
      }
      r(i) = "(%s,-)".format(getLeft(i))
      res(idx) = r
    }
    
    // return the final results
    (res, timeGrouping.map {case (t,_,_) => t})
  }

  override def getRowCount() = tableData(0).size
  override def getColumnCount() = tableData.size

  override def getValueAt(row: Int, column: Int) = tableData(column)(row)

  override def getDouble(row: Int, column: Int) = Some(0) 

  override def getColumnName(col: Int) = 
    if (col == 0) "time"
    else es.head.varNames.toIndexedSeq(col-1)

  override def isEmpty() = es.isEmpty

  override def getTimes() = times

  override def getTraceViewTimes() = tableTimes

  override def getPlottables() = plottables
}
