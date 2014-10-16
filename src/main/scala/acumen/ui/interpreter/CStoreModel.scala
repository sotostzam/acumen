package acumen
package ui
package interpreter

import scala.collection.JavaConversions._
import scala.collection.mutable.{ArrayBuffer, HashMap, HashSet, ListBuffer}
import scala.collection.{immutable => im}

import Errors._
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
class Collector[T : ClassManifest](initSize: Int, initVal: T) extends IndexedSeq[T] {
  private[this] var _data = Array.fill(math.max(initSize, 32))(initVal)
  private[this] var _size = initSize
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

case class ResultKey(objId: CId, fieldName: Name, vectorIdx: Option[Int])

abstract class ResultCollector[T : ClassManifest](val key: ResultKey, val isSimulator: Boolean, val startFrame: Int, initSize: Int, initVal: T) extends Collector[T](initSize,initVal) {
  def snapshot : Result[T]
}
class DoubleResultCollector(key: ResultKey, simulator: Boolean, startFrame: Int, initSize: Int) extends ResultCollector[Double](key, simulator, startFrame, initSize, Double.NaN) {
  override def snapshot = new DoubleResult(this)
}
class GenericResultCollector(key: ResultKey, simulator: Boolean, startFrame: Int, initSize: Int) extends ResultCollector[GValue](key, simulator, startFrame, initSize, null /* FIXME */) {
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
                case VLit(_: GStr | _: GDiscreteEnclosure[String])  => 
                  res += new PlotDiscrete(a.isSimulator, a.key.fieldName, a.startFrame, idx, a0)
                
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
  class ObjInfo(val startFrame: Int, var arraySize: Int) {val fields = new ArrayBuffer[Int]}
  private var objInfo = new HashMap[CId,ObjInfo] 
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
      (i match { case Some(k) => "["+k+"]" case None => "" }) 
    } catch { case _ => "" }
  }
 
  //def ids = stores map { case (id,_,_,_,_) => id }
  private def addVal(key: ResultKey, v:GValue) = {
    (key.fieldName.x, v) match {
      case ("_3D"|"_3DView", _) => ()
      case _ =>
        val idx = indexes.getOrElse(key, initResultObj(key, v))
      (stores(idx), v) match {
        case (sts:DoubleResultCollector, _) =>  sts += extractDoubleNoThrow(v)
        case (sts:GenericResultCollector, _) => sts += v
      }
    }
  }

  /**
   * Initialize a result object and register it in stores, indexes and ids.
   * Returns the id after adding it to ids. 
   */
  private def initResultObj(key: ResultKey, v:GValue) = {
    val className = classes(key.objId)
    val isSimulator = className == cmagic
    val oi = objInfo(key.objId)
    val ar = v match {
      case VLit(GDouble(_) | GInt(_)) =>
        new DoubleResultCollector(key, isSimulator, oi.startFrame, oi.arraySize)
      case _ =>
        new GenericResultCollector(key, isSimulator, oi.startFrame, oi.arraySize)
    }
    stores += ar
    val ridx = stores.size-1
    indexes += ((ar.key, ridx))
    oi.fields += ridx
    ridx
  } 

  private def addDataHelper(sts:TraceData) = {
    def compIds(ido1:(CId,_), ido2:(CId,_)) = ido1._1 < ido2._1
    def compFields(p1:(Name,GValue),p2:(Name,GValue)) = 
      Ordering[(String,Int)] lt ((p1._1.x, p1._1.primes),(p2._1.x, p2._1.primes))
    for (st <- sts) {
      for ((id,o) <- st.asInstanceOf[GStore].toList sortWith(compIds)) {

        if (!objInfo.contains(id)) {
          objInfo += ((id, new ObjInfo(frame, 0)))
          val className = classOf(o)
          if (className == cmagic)
            timeKey = ResultKey(id, Name("time", 0), None)
          classes += ((id, className))
        }
        val oi = objInfo(id)

        // iterate over values in object, if a field does not exist addValue will create it
        for ((x,v) <- o.toList sortWith(compFields)) {
          if (keep3D || !threeDField(x.x)) v match {
            case VVector(u) =>
              for ((ui,i) <- u zipWithIndex)
                addVal(ResultKey(id, x, Some(i)), ui)
            case _ =>
              addVal(ResultKey(id, x, None), v)
          } 
        }
        // increment the arraySize counter
        oi.arraySize += 1

        // interate over all vectors for object, if a field was not updated
        // add a dummy value
        for (idx <- oi.fields) {
          if (stores(idx).size < oi.arraySize) {
            stores(idx) match {
              case sts:DoubleResultCollector  => sts += Double.NaN
              case sts:GenericResultCollector => sts += null
            }
          }
          assert(stores(idx).size == oi.arraySize)
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
