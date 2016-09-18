package acumen
package ui
package interpreter

import Errors._
import Pretty._
import collection.mutable.ArrayBuffer
import collection.immutable.Vector
import collection.{immutable => im}
import javax.swing.event.TableModelListener
import acumen.interpreters.enclosure.affine.UnivariateAffineEnclosure
import acumen.interpreters.enclosure.affine.UnivariateAffineScalarEnclosure
import scala.math.Ordering

// Unsafe becuase we can't make an array constant, so it 
// is still possible to modify the contents even though
// we are suppose to be immutable
class UnsafeWrappedArray (val array: Array[Double]) extends im.IndexedSeq[Double] {
  def apply(i: Int) = array(i)
  def length = array.length
}

class EnclosureModel extends InterpreterModel {

  var es = new ArrayBuffer[UnivariateAffineEnclosure]

  var es2: ArrayBuffer[UnivariateAffineEnclosure] = null

  class Data {
    var tags = collection.mutable.Set.empty[Tag]
    var deadTags = collection.mutable.Set.empty[Tag]
    var times  = Vector.empty[Double]
    var plottables = im.Seq.empty[PlotEnclosure]
    var tableTimes = Array.empty[Double]
    var tableData  = Array.ofDim[String](0,0)
    var columnNames : IndexedSeq[String] = IndexedSeq.empty[String]
    var plotTitles : IndexedSeq[String] = IndexedSeq.empty[String]

    {
      times = es.map(_.domain.hiDouble).foldLeft(Vector(es.head.domain.loDouble)) { case (res, t) => res :+ t }

      // 

      def splitPrimePartOut(varn: String, primes: Int) : (String, Int) = {
        if (varn.last == '\'')
          splitPrimePartOut(varn.dropRight(1), primes + 1)
        else
          (varn, primes)
      }

      val enclSeqs = es.head.varNames.toList.
        map{name => (name, null +: es.map(_(name).toEnclosure))}.toSeq.
        sortWith{(x, y) => Ordering[(String,Int)].lt(splitPrimePartOut(x._1, 0),splitPrimePartOut(y._1, 0))}.
        zipWithIndex.map{case ((name, data),(idx)) => (name, idx + 1, data)}

      plottables = 
        enclSeqs.map {
          case (name, idx, enclosures) =>
            new PlotEnclosure(false, ResultKey(Tag.root, CId(0), Name(name, 0), None), 0, idx, enclosures.toIndexedSeq)
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

    val plotModel = new PlotModel {
      override def getRowCount() = times.length
      
      override def getValueAt(row: Int, column: Int) = {
        val encls = plottables(column-1).values
        if (row == 0)
          "(-,[%f,%f])".format(encls(row+1).loLeft, encls(row+1).hiLeft)
        else if (row == getRowCount() - 1)
          "([%f,%f],-)".format(encls(row).loRight, encls(row).hiRight)
        else
          "([%f,%f],[%f,%f])".format(encls(row).loRight, encls(row).hiRight,
                                     encls(row+1).loLeft, encls(row+1).hiLeft)
      }
      
      override def getRowCount(t: Tag): Int = getRowCount()

      override def getValueAt(t: Tag, row: Int, column: Int): String = getValueAt(row, column)

      override def getPlotTitle(t: Tag, col: Int): String = getPlotTitle(col)

      override def getDoubleAt(t: Tag, row: Int, column: Int): Option[Double] = getDoubleAt(row, column)

      override def isEmpty(t: Tag): Boolean = isEmpty()

      override def getDoubleAt(row: Int, column: Int) = None
      
      override def getPlotTitle(col: Int) = plotTitles(col)
      
      override def isEmpty() = tableData.isEmpty

      override def getTimes(t: Tag) = times
      
      override def getPlottables(p: PlotParms, tags: Set[Tag]) = plottables

      override def getTags = tags.toSet

      override def getDeadTags() = deadTags.toSet

      override def getProba: Option[ProbaData] = ???

      //FIXME Not a good place for a writing accessor.
      override def setProba(pd: Option[ProbaData]): Unit = ???
    }

    val traceModel = new TraceModel {
      override def getRowCount() = if (tableData.isEmpty) 0 else tableData(0).size
      override def getColumnCount() = tableData.size
      
      override def getValueAt(row: Int, column: Int) = tableData(column)(row)
      
      override def getColumnName(col: Int) = columnNames(col)
    }
  }

  var data : Data = null

  override def getNewData() = synchronized {
    val res = es2
    es2 = null
    res
  }

  var stale : Boolean = false;

  def addData(t: Tag, deadTag: Boolean, d:TraceData) = synchronized {
    es ++= d.asInstanceOf[Iterable[UnivariateAffineEnclosure]]
    if (es2 == null)
      es2 = new ArrayBuffer[UnivariateAffineEnclosure]
    es2 ++= d.asInstanceOf[Iterable[UnivariateAffineEnclosure]]
    stale = true
  }

  def syncData() = synchronized {
    if (stale) {
      data = new Data()
    }
    stale = false
  }

  override def getPlotModel = {syncData(); data.plotModel}
  override def getTraceModel = {syncData(); data.traceModel}

  override def getPlotter = new acumen.ui.plot.EnclosurePlotter()
}

class EnclosureTraceData(val data: Iterable[UnivariateAffineEnclosure], endTime: Double)
  extends TraceData(if (data.isEmpty) endTime else data.last.domain.hiDouble,endTime) with Iterable[UnivariateAffineEnclosure] 
{
  def iterator = data.iterator
}

