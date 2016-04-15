package acumen
package ui
package interpreter

import scala.collection.JavaConversions._
import scala.collection.mutable.{ArrayBuffer, HashMap, HashSet, ListBuffer}
import scala.collection.{immutable => im}

import Errors._
import acumen.interpreters.Common
import Pretty._
import util.Canonical._
import util.Conversions._
import acumen.interpreters.Common.threeDField

case class CStoreTraceData(data: Iterable[GStore]) 
  extends TraceData(getTime(data.last), getEndTime(data.last)) with Iterable[GStore] 
{
  def iterator = data.iterator
}

// Note: This class is a prime candidate for specialization
class Collector[T : ClassManifest] extends IndexedSeq[T] {
  private[this] var _data = new Array[T](32)
  private[this] var _size = 0
  def array = _data
  def length = _size
  def apply(index: Int) = _data(index)
  def += (el: T) = {
    if (_data.size == _size) {
      val newData = new Array[T](_data.size * 2)
      Array.copy(_data, 0, newData, 0, _data.size)
      _data = newData
    }
    _data(size) = el
    _size += 1
  }
}
class Snapshot[T](coll: Collector[T]) extends im.IndexedSeq[T]  {
  val length = coll.length
  val array = coll.array
  def apply(index: Int) = array(index)
}

case class ResultKey(objId: CId, fieldName: Name, vectorIdx: Option[(Int,Option[Int])]){
  override def toString = {
    val prefix = vectorIdx match{
      case Some((n,Some(m))) => "(" + n.toString + "," + m.toString + ")"
      case Some((n,None)) => "(" + n.toString  + ")"
      case None => ""
    }
    fieldName.x +  prefix
  }
}

abstract class ResultCollector[T : ClassManifest](val key: ResultKey, val isSimulator: Boolean, val startFrame: Int) extends Collector[T] {
  def snapshot : Result[T]
}
class DoubleResultCollector(key: ResultKey, simulator: Boolean, startFrame: Int) extends ResultCollector[Double](key, simulator, startFrame) {
  override def snapshot = new DoubleResult(this)
}
class GenericResultCollector(key: ResultKey, simulator: Boolean, startFrame: Int) extends ResultCollector[GValue](key, simulator, startFrame) {
  override def snapshot = new GenericResult(this)
}

abstract class Result[T](coll: ResultCollector[T]) extends Snapshot[T](coll) {
  val key = coll.key
  val isSimulator = coll.isSimulator
  val startFrame = coll.startFrame
  def getAsString(i: Int) : String
  def getAsDouble(i: Int) : Double
  def asDoubles : Seq[Double]
}
class DoubleResult(coll: ResultCollector[Double]) extends Result[Double](coll) 
{
  def getAsString(i: Int) = apply(i).toString
  def getAsDouble(i: Int) = apply(i)
  def asDoubles = this
}
class GenericResult(val coll: ResultCollector[GValue]) extends Result[GValue](coll) {
  def getAsString(i: Int) = pprint(apply(i))
  def getAsDouble(i: Int) = extractDoubleNoThrow(apply(i))
  def asDoubles = view.map{extractDoubleNoThrow(_)}
}

case class DataModel(columnNames: im.IndexedSeq[String], rowCount: Int, 
                     times: im.IndexedSeq[Double], stores: im.IndexedSeq[Result[_]])
     extends TraceModel with PlotModel 
{
  override def getRowCount = rowCount
  override def getColumnCount = columnNames.length

  override def getValueAt(row:Int, column:Int) = {
    try {
      val col = stores(column)
      col.getAsString(row - col.startFrame)
    } catch {
      case _ => ""
    }
  }

  override def getDouble(row:Int, column:Int) = {
    try {
      val col = stores(column)
      val x = col.getAsDouble(row - col.startFrame)
      Some(x)
    } catch { case _ => None }
  }

  override def getPlotTitle(col:Int) = columnNames(col)
  override def getColumnName(col:Int) = columnNames(col)

  override def isEmpty() = rowCount == 0

  override def getTimes() = times

  override def getPlottables(parms: PlotParms) : im.Iterable[Plottable] = {
    val res = new ListBuffer[Plottable]
    for ((a,idx) <- stores zipWithIndex) {
      (a,a.key.fieldName.x) match {
        case (a0:DoubleResult, fn) if !threeDField(fn) &&
                                      (parms.plotSimulator || !a.isSimulator) && 
                                      (parms.plotNextChild || fn != "nextChild") && 
                                      (parms.plotSeeds || (fn != "seed1" && fn != "seed2")) =>
          res += new PlotDoubles(a.isSimulator, a.key.fieldName, a.startFrame, idx, a0)
        case (a0: GenericResult, fn) => a0.coll match {
          case collGV: ResultCollector[GValue] =>
            if (collGV nonEmpty)
              collGV(0) match {
                case VLit(_: GInterval | _: GRealEnclosure) =>
                  val collE: scala.collection.immutable.IndexedSeq[Enclosure] = (collGV.map {
                    case VLit(GInterval(i)) =>
                      Enclosure(i.loDouble, i.hiDouble, i.loDouble, i.hiDouble)
                    case VLit(ge: GRealEnclosure) =>
                      val i = ge.range
                      Enclosure(i.loDouble, i.hiDouble, i.loDouble, i.hiDouble)
                  }).toIndexedSeq
                  res += new PlotEnclosure(a.isSimulator, a.key.fieldName, a.startFrame, idx, collE)
                case VLit(_: GStr | _: GDiscreteEnclosure[_])  => 
                  res += new PlotDiscrete(a.isSimulator, a.key.fieldName, a.startFrame, idx, a0)
                case VVector(n) =>
                case _ => ()
              }
          case _ => ()
        }
        case _ => ()
      }
    }
    res.toList
  }
}

class CStoreModel(ops: CStoreOpts) extends InterpreterModel {
  private var keep3D = ops.keep3D
  private var stores = new ArrayBuffer[ResultCollector[_]]
  private var classes = new HashMap[CId,ClassName]
  private var indexes = new HashMap[ResultKey,Int]
  private var ids = new HashSet[CId]
  private var frame = 0
  private var timeKey : ResultKey = null

  // package all variables that need to be updated Atomically into
  // a case class
  case class Data(model: DataModel,
                  updatingData: Boolean)
  @volatile var d : Data = Data(DataModel(im.IndexedSeq.empty,0,im.IndexedSeq.empty,im.IndexedSeq.empty), false)

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
      d = Data(model = DataModel(columnNames = stores.indices.map{i => getColumnNameLive(i)},
                                 rowCount = frame,
                                 times = stores(indexes(timeKey)).asInstanceOf[DoubleResultCollector].snapshot,
                                 stores = stores.view.map{st => st.snapshot}.toIndexedSeq),
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
      (i match { 
        case Some(k) => k._2 match{
          case Some(j) => "("+k._1+ "," + j +")" 
          case None => "("+k._1+")"
        }
        case None => "" }) 
    } catch { case _ => "" }
  }
 
  //def ids = stores map { case (id,_,_,_,_) => id }
  private def addVal(id:CId, x:Name, v:GValue) = {
    def add(idx: Int, vectorIdx: Option[(Int,Option[Int])], v: GValue) = 
  //  Add value to the resultCollectors
  private def addVal(id: CId, name: Name, value: GValue) = {
    def add(idx: Int, vectorIdx: Option[(Int,Option[Int])], v: GValue) =
      (stores(idx), v) match {
        case (sts:DoubleResultCollector, _) =>  sts += extractDoubleNoThrow(v)
        case (sts:GenericResultCollector, _) => sts += v
      }
    if(name.x.split("__")(0) == "pattern") ()
    else insertValue(value, None)

    def insertValue(addedValue: GValue, vectorIdx: Option[(Int,Option[Int])]): Unit = {
      (name.x, addedValue) match {
        case ("_3D"|"_3DView"|"_plot", _) => ()
        case (_, VVector(values)) => // the value is a VVector, extract each value out
          for ((ui,i) <- values.zipWithIndex) {
            ui match{
              case VVector(columns) =>  //  the value is a VVector, extract again
                for((uii,ii) <- columns.zipWithIndex){
                  insertValue(uii, Some(i,Some(ii)))
                }
              case _ =>
                insertValue(ui, Some(i, None))
            }
          }
        case _ => // single GValue, such as VLit
          // the variable is already exist in the indexes, add the value to the stores
          if (indexes.contains(ResultKey(id, name, vectorIdx))) {
            val idx = indexes(ResultKey(id, name, vectorIdx))
            add(idx, vectorIdx, addedValue)
          } else {  // the variable is filtered out before, need to be inserted into the store
            val ar = newResultObj(addedValue, id, name, false, vectorIdx)
            stores += ar
            indexes += ((ar.key, stores.size-1))
          }
      }
    }
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
          val className = classOf(o)
          val isSimulator = className == cmagic
          if (isSimulator)
            timeKey = ResultKey(id, Name("time", 0), None)
          classes += ((id, className))
          for ((x,v) <- o.toList sortWith(compFields)) {
            def newResultObj (vectorIdx: Option[(Int, Option[Int])], v: GValue) = {
              val key = ResultKey(id,x,vectorIdx)
              v match {
                case VLit(GDouble(_)|GInt(_)) => 
                  val ar = new DoubleResultCollector(ResultKey(id,x,vectorIdx),isSimulator,frame)
                  ar += extractDoubleNoThrow(v)
                  ar 
                case _ =>
                  val ar = new GenericResultCollector(ResultKey(id,x,vectorIdx),isSimulator,frame)
                  ar += v
                  ar
              }
            }
            if ( !threeDField(x.x) && x.x.split("__")(0) != "pattern" ) v match {
              case VVector(u) =>
                for ((ui,i) <- u zipWithIndex) {
                  ui match{
                    case VVector(columns) => 
                      for((uii,ii) <- columns zipWithIndex){
                        var ar = newResultObj(Some(i,Some(ii)), uii)
                        stores += ar
                        indexes += ((ar.key, stores.size-1))
                        ids += id
                      }
                    case _ => 
                      var ar = newResultObj(Some(i,None), ui)
                      stores += ar
                      indexes += ((ar.key, stores.size-1))
                      ids += id                 
                  }
                  
                }
              case _ =>
                var ar = newResultObj(None, v)
                stores += ar
                indexes += ((ar.key, stores.size-1))
                ids += id
            } 
          }
        }
      }
      frame += 1
    }
  }

  override def getNewData() = {flushPending(); d.model}

  override def getPlotModel = {flushPending(); d.model}
  override def getTraceModel = {flushPending(); d.model}

  override def getPlotter = new acumen.ui.plot.CStorePlotter()

  override def flush() = {flushPending()}

}
