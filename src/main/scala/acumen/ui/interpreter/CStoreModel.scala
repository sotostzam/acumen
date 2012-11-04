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

case class CStoreTraceData(data: Iterable[CStore]) 
  extends TraceData(getTime(data.last), getEndTime(data.last)) with Iterable[CStore] 
{
  def iterator = data.iterator
}

class CStoreModel extends TraceModel with InterpreterModel with PlotModel {
  // array of (id, field name, maybe index in vector, start frame, values)
  private var stores = new ArrayBuffer[(CId, Name, Option[Int], Int, ArrayBuffer[CValue])]
  private var classes = new HashMap[CId,ClassName]
  private var indexes = new HashMap[(CId,Name,Option[Int]),Int]
  private var ids = new HashSet[CId]
  private var frame = 0

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
      val (id,x,i,_,_) = stores(col)
      "(#" + id + " : " + classes(id).x + ")." + x.x + "'" * x.primes + 
      (i match { case Some(k) => "["+k+"]" case None => "" }) 
    } catch { case _ => "" }
  }


 
  //def ids = stores map { case (id,_,_,_,_) => id }
  private def addVal(id:CId, x:Name, v:CValue) = {
    v match {
      case VVector(u) =>
        for ((ui,i) <- u zipWithIndex) {
          val idx = indexes((id,x,Some(i)))
          stores(idx)._5 += ui
        }
      case _ =>
        val idx = indexes((id,x,None))
        stores(idx)._5 += v
    }
  }

  private def addDataHelper(sts:TraceData) = {
    def compIds(ido1:(CId,_), ido2:(CId,_)) = ido1._1 < ido2._1
    def compFields(p1:(Name,CValue),p2:(Name,CValue)) = 
      Ordering[(String,Int)] lt ((p1._1.x, p1._1.primes),(p2._1.x, p2._1.primes))
    for (st <- sts) {
      for ((id,o) <- st.asInstanceOf[CStore].toList sortWith(compIds)) {
        if (ids contains id) 
          for ((x,v) <- o.toList) addVal(id,x,v)
        else {
          for ((x,v) <- o.toList sortWith(compFields)) {
            v match {
              case VVector(u) =>
                for ((ui,i) <- u zipWithIndex) {
                  val ar = new ArrayBuffer[CValue]
                  ar += ui
                  stores += ((id,x,Some(i),frame,ar))
                  indexes += (((id,x,Some(i)), stores.size-1))
                  ids += id
                }
              case _ =>
                val ar = new ArrayBuffer[CValue]
                ar += v
                stores += ((id,x,None,frame,ar))
                indexes += (((id,x,None), stores.size-1))
                ids += id
            } 
          }
          classes += ((id, classOf(o)))
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
      val v = col._5(row - col._4)
      pprint(v)
    } catch { case e => "" } 
  }

  override def getDouble(row:Int, column:Int) = {
    if (d.updatingData)
      None
    else try {
      val col = stores(column)
      val x = extractDouble(col._5(row - col._4))
      Some(x)
    } catch { case _ => None }
  }

  override def getColumnName(col:Int) = d.columnNames(col)

  override def isEmpty() = d.rowCount == 0

  override def getTimes() = {
    if (d.updatingData) 
      throw new java.lang.IllegalStateException
    (stores find {
      case (id,Name(x, 0),None,_,_) => 
        x == "time" && classes(id) == cmagic
      case _ => false
    }) match {
      case Some((_,_,_,_,arr)) => arr map { 
        case VLit(GDouble(x)) => x 
        case _ => throw BadTimeType()
      }
      case None => throw NoInstanceFound(cmagic)
    }
  }

  override def getPlottables() : IndexedSeq[PlotDoubles] = {
    if (d.updatingData) 
      throw new java.lang.IllegalStateException
    val res = new ArrayBuffer[PlotDoubles]

    for (((id,fn,_,s,a),idx) <- stores zipWithIndex)
      a(0) match {
        case VLit(GDouble(_) | GInt(_)) | VLit(GInt(_)) =>
          val vls = new IndexedSeq[Double] {
            override def apply(idx: Int) = { 
              val x = extractDouble(a(idx));
              x
            }
            // store length in a local variable since by the time this
            // data structure is accesses the more data could already
            // have been added to the cstore
            val _length = a.length
            override def length = _length
          }
          res += new PlotDoubles(classes(id) == cmagic, fn, s, idx, vls)
        case _ => ()
      }
    res
  }

  override def getPlotModel = {flushPending(); this}
  override def getTraceModel = {flushPending(); this}
}
