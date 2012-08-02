package acumen
package ui

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

class TraceModel extends AbstractTableModel {

  // array of (id, field name, maybe index in vector, start frame, values)
  var stores = new ArrayBuffer[(CId, Name, Option[Int], Int, ArrayBuffer[CValue])]
  var classes = new HashMap[CId,ClassName]
  var indexes = new HashMap[(CId,Name,Option[Int]),Int]
  var ids = new HashSet[CId]
  var frame = 0
  
  //def ids = stores map { case (id,_,_,_,_) => id }
  def addVal(id:CId, x:Name, v:CValue) = {
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

  def addStore(st:CStore) = 
    addStores(List(st))

  def addStores(sts:Iterable[CStore]) = {
    def compIds(ido1:(CId,_), ido2:(CId,_)) = ido1._1 < ido2._1
    def compFields(p1:(Name,CValue),p2:(Name,CValue)) = 
      Ordering[(String,Int)] lt ((p1._1.x, p1._1.primes),(p2._1.x, p2._1.primes))
    for (st <- sts) {
      for ((id,o) <- st.toList sortWith(compIds)) {
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
    // FIXME: be more precise ?
    fireTableStructureChanged()
    //fireTableDataChanged()
  }

  def getRowCount = frame
  def getColumnCount = stores.size

  override def getValueAt(row:Int, column:Int) = {
    try {
      val col = stores(column)
      val v = col._5(row - col._4)
      pprint(v)
    } catch { case e => "" } 
  }

  def getDouble(row:Int, column:Int) = {
    try {
      val col = stores(column)
      val x = extractDouble(col._5(row - col._4))
      Some(x)
    } catch { case _ => None }
  }

  override def getColumnName(col:Int) = {
    try {
      val (id,x,i,_,_) = stores(col)
      "(#" + id + " : " + classes(id).x + ")." + x.x + "'" * x.primes + 
        (i match { case Some(k) => "["+k+"]" case None => "" }) 
    } catch { case _ => "" }
  }

  def reset = {
    stores = new ArrayBuffer[(CId,Name,Option[Int],Int,ArrayBuffer[CValue])]
    classes = new HashMap[CId,ClassName]
    indexes = new HashMap[(CId,Name,Option[Int]),Int]
    ids = new HashSet[CId]
    fireTableStructureChanged()
    frame = 0
  }
}


