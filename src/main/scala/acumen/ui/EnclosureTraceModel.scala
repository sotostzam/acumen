package acumen
package ui

import Errors._
import Pretty._
import collection.mutable.ArrayBuffer
import collection.mutable.WrappedArray
import javax.swing.event.TableModelListener
import acumen.interpreters.enclosure.UnivariateAffineEnclosure
import acumen.interpreters.enclosure.UnivariateAffineScalarEnclosure

class EnclosureTraceModel extends AbstractTraceModel {

  var es = new ArrayBuffer[UnivariateAffineEnclosure]

  class Data {
    var times  = new ArrayBuffer[Double] 
    var plottables : Iterable[PlotEnclosure] = Iterable.empty[PlotEnclosure]
    var tableTimes = Array.ofDim[Double](0)
    var tableData  = Array.ofDim[String](0,0)
    var columnNames : IndexedSeq[String] = IndexedSeq.empty[String]

    def recompute() = {

      times = es.map(_.domain.hiDouble).foldLeft(ArrayBuffer(es.head.domain.loDouble)) { case (res, t) => res += t }

      val enclSeqs = es.head.varNames.zipWithIndex.map{case (name,idx) => (name, idx + 1, null +: es.map(_(name).toEnclosure))}

      plottables = 
      enclSeqs.map {
        case (name, idx, enclosures) =>
          new PlotEnclosure(false, Name(name, 0), 0, idx, enclosures.toIndexedSeq)
      }

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

      // now create an array of times in the format we need
      tableTimes = new Array[Double](timeGrouping.size * 2 - 2)
      var i = 0; var j = 0
      tableTimes(i) = timeGrouping(j)._1
      i += 1; j += 1
      while (j < timeGrouping.size - 1) {
        tableTimes(i) = timeGrouping(j)._1
        i += 1
        tableTimes(i) = timeGrouping(j)._1
        i += 1; j += 1
      }
      tableTimes(i) = timeGrouping(j)._1

      // prep the table array
      tableData = Array.fill(plottables.size + 1){null:Array[String]}

      // fill in the first column with the time value
      tableData(0) = tableTimes.map {t => "%f".format(t)}

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
        val r = Array.fill(timeGrouping.size * 2 - 2){null:String}
        var i = 0; var j = 1
        while (j < timeGrouping.size) {
          r(i) = getLeft(j)
          i += 1
          r(i) = getRight(j)
          i += 1; j += 1
        }
        tableData(idx) = r
      }

      columnNames = IndexedSeq("time") ++ es.head.varNames
    }
  }

  var data = new Data

  override def getRowCount() = if (data.tableData.isEmpty) 0 else data.tableData(0).size
  override def getColumnCount() = data.tableData.size

  override def getValueAt(row: Int, column: Int) = data.tableData(column)(row)

  override def getDouble(row: Int, column: Int) = Some(0) 

  override def getColumnName(col: Int) = data.columnNames(col)

  override def isEmpty() = es.isEmpty

  override def getTimes() = data.times

  override def getTraceViewTimes() = data.tableTimes

  override def getPlottables() = data.plottables

  override def addData(d:TraceData) = {

    es ++= d.asInstanceOf[Iterable[UnivariateAffineEnclosure]]

    data.recompute()
    
    fireTableStructureChanged()
  }

  override def reset = {
    
    es.clear()

    data = new Data
    
  }

}

case class EnclosureTraceData(data: Iterable[UnivariateAffineEnclosure])
  extends TraceData(0,0) with Iterable[UnivariateAffineEnclosure] 
{
  def iterator = data.iterator
}

