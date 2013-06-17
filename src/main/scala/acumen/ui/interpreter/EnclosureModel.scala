package acumen
package ui
package interpreter

import Errors._
import Pretty._
import collection.mutable.ArrayBuffer
import collection.mutable.WrappedArray
import javax.swing.event.TableModelListener
import acumen.interpreters.enclosure.affine.UnivariateAffineEnclosure
import acumen.interpreters.enclosure.affine.UnivariateAffineScalarEnclosure
import scala.math.Ordering

class EnclosureModel extends TraceModel with PlotModel with InterpreterModel {

  var es = new ArrayBuffer[UnivariateAffineEnclosure]

  var es2: ArrayBuffer[UnivariateAffineEnclosure] = null

  class Data {
    var times  = new ArrayBuffer[Double] 
    var plottables : Iterable[PlotEnclosure] = Iterable.empty[PlotEnclosure]
    var tableTimes = Array.ofDim[Double](0)
    var tableData  = Array.ofDim[String](0,0)
    var columnNames : IndexedSeq[String] = IndexedSeq.empty[String]
    var plotTitles : IndexedSeq[String] = IndexedSeq.empty[String]

    def recompute() = {
      times = es.map(_.domain.hiDouble).foldLeft(ArrayBuffer(es.head.domain.loDouble)) { case (res, t) => res += t }

      // 

      def splitPrimePartOut(varn: String, primes: Int) : (String, Int) = {
        if (varn.last == '\'')
          splitPrimePartOut(varn.dropRight(1), primes + 1)
        else
          (varn, primes)
      }

      val enclSeqs = es.head.varNames.
        map{name => (name, null +: es.map(_(name).toEnclosure))}.toSeq.
        sortWith{(x, y) => Ordering[(String,Int)].lt(splitPrimePartOut(x._1, 0),splitPrimePartOut(y._1, 0))}.
        zipWithIndex.map{case ((name, data),(idx)) => (name, idx + 1, data)}

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
      tableData = Array.fill(plottables.size*2 + 1){null:Array[String]}

      // fill in the first column with the time value
      tableData(0) = tableTimes.map {t => "%f".format(t)}

      // fill in the other columns
      for ((_, idx, enclosures) <- enclSeqs) {
        def getLeft(idx: Int) = {
          val (_,start,stop) = timeGrouping(idx)
          val data = enclosures.slice(start,stop)
          val lo = data.map {encl => encl.loLeft}.min
          val hi = data.map {encl => encl.hiLeft}.max
          ("%f".format(lo), "%f".format(hi))
        }
        def getRight(idx: Int) = {
          val (_,start,stop) = timeGrouping(idx)
          val data = enclosures.slice(start,stop)
          val lo = data.map {encl => encl.loRight}.min
          val hi = data.map {encl => encl.hiRight}.max
          ("%f".format(lo), "%f".format(hi))
        }
        val rLo = Array.fill(timeGrouping.size * 2 - 2){null:String}
        val rHi = Array.fill(timeGrouping.size * 2 - 2){null:String}
        var i = 0; var j = 1
        while (j < timeGrouping.size) {
          val (a,b) = getLeft(j)
          rLo(i) = a
          rHi(i) = b
          i += 1;
          val (c,d) = getRight(j)
          rLo(i) = c
          rHi(i) = d
          i += 1; j += 1
        }
        tableData((idx-1)*2+1) = rLo
        tableData((idx-1)*2+2) = rHi
      }

      plotTitles = IndexedSeq("time") ++ enclSeqs.map{case(n,_,_) => n}
      columnNames = IndexedSeq("time") ++ enclSeqs.map{case(n,_,_) => List(n+".min", n+".max")}.flatten
    }
  }

  var data = new Data

  override def getRowCount() = if (data.tableData.isEmpty) 0 else data.tableData(0).size
  override def getColumnCount() = data.tableData.size

  override def getValueAt(row: Int, column: Int) = data.tableData(column)(row)

  override def getDouble(row: Int, column: Int) = None

  override def getPlotTitle(col: Int) = data.plotTitles(col)

  override def getColumnName(col: Int) = data.columnNames(col)

  override def isEmpty() = es.isEmpty

  override def getTimes() = data.times

  override def getTraceViewTimes() = data.tableTimes

  override def getPlottables() = data.plottables

  override def getNewData() = synchronized {
    val res = es2
    es2 = null
    res
  }

  var stale : Boolean = false;

  override def addData(d:TraceData) = synchronized {
    es ++= d.asInstanceOf[Iterable[UnivariateAffineEnclosure]]
    if (es2 == null)
      es2 = new ArrayBuffer[UnivariateAffineEnclosure]
    es2 ++= d.asInstanceOf[Iterable[UnivariateAffineEnclosure]]
    stale = true
  }

  def syncData() = synchronized {
    if (stale) {
      val d = new Data()
      d.recompute()
      data = d
    }
    stale = false
  }

  override def getPlotModel = {syncData(); this}
  override def getTraceModel = {syncData(); this}

  override def getPlotter = new acumen.ui.plot.EnclosurePlotter()
}

class EnclosureTraceData(val data: Iterable[UnivariateAffineEnclosure], endTime: Double)
  extends TraceData(if (data.isEmpty) endTime else data.last.domain.hiDouble,endTime) with Iterable[UnivariateAffineEnclosure] 
{
  def iterator = data.iterator
}

