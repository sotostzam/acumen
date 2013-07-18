package acumen
package ui
package interpreter

import Pretty._
import util.Canonical._
import util.Conversions._

import scala.io._
import collection.mutable.ArrayBuffer
import collection.mutable.HashMap
import collection.mutable.HashSet
import collection.JavaConversions._
import scala.collection.immutable.Stream

import javax.swing.table.TableModel
import javax.swing.table.AbstractTableModel

import Errors._

case class CStoreTraceData(data: Iterable[GStore]) 
  extends TraceData(getTime(data.last), getEndTime(data.last)) with Iterable[GStore] 
{
  def iterator = data.iterator
}

case class ResultKey(objId: CId, fieldName: Name, vectorIdx: Option[Int])
abstract class Result[V](val key: ResultKey, val startFrame: Int) extends ArrayBuffer[V] {
  def getAsString(i: Int) : String
  def getAsDouble(i: Int) : Double
  def asDoubles : Seq[Double]
}
class DoubleResult(key: ResultKey, startFrame: Int) extends Result[Double](key, startFrame) {
  def getAsString(i: Int) = apply(i).toString
  def getAsDouble(i: Int) = apply(i)
  def asDoubles = this
}
class GenericResult(key: ResultKey, startFrame: Int) extends Result[GValue](key, startFrame) {
  def getAsString(i: Int) = pprint(apply(i))
  def getAsDouble(i: Int) = extractDoubleNoThrow(apply(i))
  def asDoubles = view.map{extractDoubleNoThrow(_)}
}

class CStoreModel extends TraceModel with InterpreterModel with PlotModel {
  // array of (id, field name, maybe index in vector, start frame, values)
  private var stores = new ArrayBuffer[Result[_]]
  private var classes = new HashMap[CId,ClassName]
  private var indexes = new HashMap[ResultKey,Int]
  private var ids = new HashSet[CId]
  private var frame = 0
  private var timeKey : ResultKey = null

  // package all variables that need to be updated Atomically into
  // a case class
  case class Data(columnNames: Array[String],
                  rowCount: Int,
                  updatingData: Boolean)
  @volatile var d : Data = Data(new Array[String](0), 0, false)

  val pending = new ArrayBuffer[TraceData]()

  def addData(sts:TraceData) = {
    pending.synchronized {
      pending += sts
    }
  }

  def flushPending() : Unit = {
    try {
      var pd : Array[TraceData] = null
      pending.synchronized {
        if (d.updatingData) 
          throw new java.lang.IllegalStateException
        if (pending.isEmpty())
          return
        d = d.copy(updatingData = true)
        pd = pending.toArray
        pending.clear()
      }
      for (sts <- pd)
        addDataHelper(sts)
      d = Data(columnNames = stores.indices map {i => getColumnNameLive(i)} toArray,
               rowCount = frame,
               updatingData = false)
    } catch {
      case e => 
        d = d.copy(updatingData = false)
        throw e
    }
  }

  def getColumnNameLive(col:Int) = {
    try {
      val ResultKey(id,x,i) = stores(col).key
      "(#" + id + " : " + classes(id).x + ")." + x.x + "'" * x.primes + 
      (i match { case Some(k) => "["+k+"]" case None => "" }) 
    } catch { case _ => "" }
  }


 
  //def ids = stores map { case (id,_,_,_,_) => id }
  private def addVal(id:CId, x:Name, v:GValue) = {
    def add(idx: Int, vectorIdx: Option[Int], v: GValue) = 
      (stores(idx), v) match {
        case (sts:DoubleResult, _) =>                 sts += extractDoubleNoThrow(v)
        case (sts:GenericResult, _) =>                sts += v
      }
    (x.x, v) match {
      case ("_3D"|"_3DView", _) => ()
      case (_,VVector(u)) =>
        for ((ui,i) <- u zipWithIndex) {
          val idx = indexes(ResultKey(id,x,Some(i)))
          add(idx, Some(i), ui)
        }
      case _ =>
        val idx = indexes(ResultKey(id,x,None))
        add(idx, None, v)
    }
  }

  private def addDataHelper(sts:TraceData) = {
    def compIds(ido1:(CId,_), ido2:(CId,_)) = ido1._1 < ido2._1
    def compFields(p1:(Name,GValue),p2:(Name,GValue)) = 
      Ordering[(String,Int)] lt ((p1._1.x, p1._1.primes),(p2._1.x, p2._1.primes))
    for (st <- sts) {
      for ((id,o) <- st.asInstanceOf[GStore].toList sortWith(compIds)) {
        if (ids contains id) 
          for ((x,v) <- o.toList) addVal(id,x,v)
        else {
          for ((x,v) <- o.toList sortWith(compFields)) {
            def newResultObj (vectorIdx: Option[Int], v: GValue) = v match {
              case VLit(GDouble(_)|GInt(_)) => 
                val ar = new DoubleResult(ResultKey(id,x,vectorIdx),frame)
                ar += extractDoubleNoThrow(v)
                ar 
              case _ =>
                val ar = new GenericResult(ResultKey(id,x,vectorIdx),frame)
                ar += v
                ar
            }
            v match {
              case VVector(u) =>
                for ((ui,i) <- u zipWithIndex) {
                  var ar = newResultObj(Some(i), ui)
                  stores += ar
                  indexes += ((ar.key, stores.size-1))
                  ids += id
                }
              case _ =>
                var ar = newResultObj(None, v)
                stores += ar
                indexes += ((ar.key, stores.size-1))
                ids += id
            } 
          }
          val className = classOf(o)
          classes += ((id, className))
          if (className == cmagic)
            timeKey = ResultKey(id, Name("time", 0), None)
        }
      }
      frame += 1
    }
  }

  override def getRowCount = d.rowCount
  override def getColumnCount = d.columnNames.length

  override def getValueAt(row:Int, column:Int) = {
    if (d.updatingData) 
      "???"
    else try {
      val col = stores(column)
      col.getAsString(row - col.startFrame)
    } catch {
      case _ => ""
    }
  }

  override def getDouble(row:Int, column:Int) = {
    if (d.updatingData)
      None
    else try {
      val col = stores(column)
      val x = col.getAsDouble(row - col.startFrame)
      Some(x)
    } catch { case _ => None }
  }

  override def getPlotTitle(col:Int) = d.columnNames(col)
  override def getColumnName(col:Int) = d.columnNames(col)

  override def isEmpty() = d.rowCount == 0

  override def getTimes() = {
    if (d.updatingData) 
      throw new java.lang.IllegalStateException
    if (timeKey == null)
      new Array[Double](0)
    else 
      stores(indexes(timeKey)).asInstanceOf[IndexedSeq[Double]]
  }

  override def getPlottables() : IndexedSeq[PlotDoubles] = {
    if (d.updatingData) 
      throw new java.lang.IllegalStateException
    val res = new ArrayBuffer[PlotDoubles]

    for ((a,idx) <- stores zipWithIndex) {
      (a,a.key.fieldName.x) match {
        case (_, "_3D"|"_3DView") => ()
        case (a0:DoubleResult, _) =>
          res += new PlotDoubles(classes(a.key.objId) == cmagic, a.key.fieldName, a.startFrame, idx, a0)
        case _ => ()
      }
    }
    res
  }

  override def getNewData() = {flushPending(); this}

  override def getPlotModel = {flushPending(); this}
  override def getTraceModel = {flushPending(); this}

  override def getPlotter = new acumen.ui.plot.CStorePlotter()
}
