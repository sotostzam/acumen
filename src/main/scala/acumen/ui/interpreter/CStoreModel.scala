package acumen
package ui
package interpreter


import scala.collection.JavaConversions._
import scala.collection.mutable.{ArrayBuffer, HashMap, HashSet, ListBuffer}
import scala.collection.{mutable, immutable => im}
import scala.collection.immutable.{Iterable, Set, TreeMap}
import scala.reflect.ClassTag
import Errors.ShouldNeverHappen
import Pretty._
import util.Canonical._
import util.Conversions._
import interpreters.Common.specialField
import interpreters.enclosure.Interval

case class CStoreTraceData(data: Iterable[GStore]) extends TraceData(getTime(data.last), getEndTime(data.last)) with Iterable[GStore] {
  def iterator = data.iterator
}

// Note: This class is a prime candidate for specialization
// The class for transferring immutable data structure to mutable array data-structure
class Collector[T: ClassTag] extends IndexedSeq[T] {
  private[this] var _data = new Array[T](32)
  private[this] var _size = 0
  def array = _data
  def length = _size
  def apply(index: Int) = _data(index)
  def += (el: T) = {
    if (_data.length == _size) {
      val newData = new Array[T](_data.length * 2)
      Array.copy(_data, 0, newData, 0, _data.length)
      _data = newData
    }
    _data(size) = el
    _size += 1
  }
}

// Take a snapshot of the array
class Snapshot[T](coll: Collector[T]) extends im.IndexedSeq[T] {
  val length = coll.length
  val array = coll.array
  def apply(index: Int) = array(index)
}

// Create the prefix of an object in plotter and table, ex: simulator(0,0), it will be parsed in getColumnNameLive function
case class ResultKey(tag: Tag, objId: CId, fieldName: Name, vectorIdx: Option[(Int,Option[Int])]) {
  override def toString = {
    val prefix = vectorIdx match{
      case Some((n,Some(m))) => "(" + n.toString + "," + m.toString + ")"
      case Some((n,None)) => "(" + n.toString  + ")"
      case None => ""
    }
    fieldName.x + prefix
  }
}

// Take a snapshot of the result key
abstract class ResultCollector[T: ClassTag](val key: ResultKey, val isSimulator: Boolean, val startFrame: Int) extends Collector[T] {
  def snapshot : Result[T]
}
class DoubleResultCollector(key: ResultKey, simulator: Boolean, startFrame: Int) extends ResultCollector[Double](key, simulator, startFrame) {
  override def snapshot = new DoubleResult(this)
}
class EnclosureResultCollector(key: ResultKey, simulator: Boolean, startFrame: Int) extends ResultCollector[Interval](key, simulator, startFrame) {
  override def snapshot = new EnclosureResult(this)
}
class GenericResultCollector(key: ResultKey, simulator: Boolean, startFrame: Int) extends ResultCollector[GValue](key, simulator, startFrame) {
  override def snapshot = new GenericResult(this)
}

/** Key is the ResultKey of coll
  * startFrame is the row number that the object appear in the store */
abstract class Result[T](coll: ResultCollector[T]) extends Snapshot[T](coll) {
  val key = coll.key
  val isSimulator = coll.isSimulator
  val startFrame = coll.startFrame
  def getAsString(i: Int) : String
  def getAsDouble(i: Int) : Double
  def getAsPair(i: Int) = (getAsDouble(i), getAsDouble(i))
  def asDoubles : Seq[Double]
}
class DoubleResult(coll: ResultCollector[Double]) extends Result[Double](coll) {
  def getAsString(i: Int) = apply(i).toString
  def getAsDouble(i: Int) = apply(i)
  def asDoubles = this
}
class EnclosureResult(val coll: ResultCollector[Interval]) extends Result[Interval](coll) {
  def getAsString(i: Int) = apply(i).toString
  def getAsDouble(i: Int) = apply(i).midpoint.doubleValue()
  override def getAsPair(i: Int) = (apply(i).lo.doubleValue(), apply(i).hi.doubleValue())
  def asDoubles = view.map{_.midpoint.doubleValue()}
}
class GenericResult(val coll: ResultCollector[GValue]) extends Result[GValue](coll) {
  def getAsString(i: Int) = pprint(apply(i))
  def getAsDouble(i: Int) = extractDoubleNoThrow(apply(i))
  def asDoubles = view.map{extractDoubleNoThrow(_)}
}

/**
  * Stores a CDF bounding and a PDF bounding for a variable at a given time
  * @param name name of the pointed variable
  * @param t time corresponding to the PDF/CDF
  * @param pdf Bounding of the probability for a value to be in the key interval
  * @param cdf Bounding of the cdf
  */
class ProbaData(name: String, t: Double, probaOut: Interval, pdf: Map[Interval, Interval], cdf: Map[Interval, Interval]) {
  def getName = name
  def getTime = t
  def getProbaOut = probaOut
  def getPdf = pdf
  def getCdf = cdf
  def getValuesRange = Interval(pdf.keys.minBy(_.lo).lo, pdf.keys.maxBy(_.hi).hi)
}

/** The data model for plotter
 *  Each field is a map to be able to fit several stores while
 *  keeping them independant.
 *  Each store is identified by its tag stored in the tags field */
case class DataModel(columnNames: mutable.Map[Tag, im.IndexedSeq[String]],
                     rowCount: mutable.Map[Tag, Int],
                     times: mutable.Map[Tag, im.IndexedSeq[Double]],
                     stores: mutable.Map[Tag, im.IndexedSeq[Result[_]]],
                     tags: mutable.ArrayBuffer[Tag],
                     deadTags: mutable.ArrayBuffer[Tag]) extends TraceModel with PlotModel {
  //Indirection table to convert a column to a tag and a column
  val colToTagCol = mutable.ArrayBuffer.empty[Option[(Tag, Int)]]
  //FIXME: Shouldn't it be in the argument and modified by the DataAdder ?
  var probaData: Option[ProbaData] = None

  override def getRowCount(t: Tag): Int = rowCount(t)

  override def getValueAt(t: Tag, row: Int, column: Int): String = {
    try {
      val col = stores(t)(column)
      val i = row - col.startFrame
      if (i >= col.size) ""
      else col.getAsString(i)
    } catch {
      case _ => ""
    } //Is not enough to prevent from accessing values out of theoretical max index (pre-allocation)
  }

  override def getDoubleAt(t: Tag, row: Int, column: Int): Option[Double] = {
    try {
      val col = stores(t)(column)
      val i = row - col.startFrame
      if (i >= col.size) None
      else Some(col.getAsDouble(i))
    } catch {
      case _ => None
    }
  }

  override def getBoundingAt(row: Int, column: Int) = {
    val (tag, col) = tagAndColFromCol(column)
    try {
      val col0 = stores(tag)(col)
      val i = row - col0.startFrame
      if (i >= col0.size) None
      else Some(col0.getAsPair(i))
    } catch {
      case _ => None
    }
  }

  override def getBoundingAt(t: Tag, row: Int, column: Int) = {
    try {
      val col = stores(t)(column)
      val i = row - col.startFrame
      if (i >= col.size) None
      else Some(col.getAsPair(i))
    } catch {
      case _ => None
    }
  }

  override def getPlotTitle(t: Tag, col: Int): String = columnNames(t)(col)

  override def isEmpty(t: Tag): Boolean = rowCount(t) == 0

  override def getColumnCount() = tags.foldLeft(0) { case (count, t) => count + columnNames(t).length }

  override def getRowCount() = totalRowCount

  override def getValueAt(row: Int, column: Int) = {
    val (tag, col) = tagAndColFromCol(column)
    try {
      val col0 = stores(tag)(col)
      val i = row - col0.startFrame
      if (i >= col0.size) ""
      else col0.getAsString(i)
    } catch {
      case _ => ""
    }
  }

  override def getDoubleAt(row: Int, column: Int) = {
    val (tag, col) = tagAndColFromCol(column)
    try {
      val col0 = stores(tag)(col)
      val i = row - col0.startFrame
      if (i >= col0.size) None
      else Some(col0.getAsDouble(i))
    } catch {
      case _ => None
    }
  }

  /** The n-th column is obtained virtually from concatenating the 
   *  different stores in the list of tag order */
  private def tagAndColFromCol(column: Int): (Tag, Int) = {
    //Make the table big enough to reach index column
    while (colToTagCol.size <= column) colToTagCol += None
    //If the column has already been used, get it
    val tag = colToTagCol(column)
    if (tag.isDefined)
      tag.get
    else {
      var newIndex = column
      var remTags = tags
      while (newIndex >= columnNames(remTags.head).size) {
        newIndex -= columnNames(remTags.head).size
        remTags = remTags.tail
      }
      colToTagCol(column) = Some(remTags.head, newIndex)
      (remTags.head, newIndex)
    }
  }

  /** The global number of rows is given by the biggest value obtained
   *  by summing the startFrame to the size of each column */
  private def totalRowCount = tags.foldLeft(0)((trc, t) => {
    val rc = (stores(t) map (c => c.size + c.startFrame)).max
    Math.max(trc, rc)
  })

  /** The total number of column in all Stores */
  def getColumnCount(t: Tag) = columnNames(t).length

  def setIfNotContains(t: Tag) =
    // The DataModel state must be consistent, i.e. the tags in the set tags are present in every maps
    if (!tags.contains(t)) {
      tags += t
      columnNames += ((t, im.IndexedSeq.empty))
      rowCount += ((t, 0))
      times += ((t, im.IndexedSeq.empty))
      stores += ((t, im.IndexedSeq.empty))
    }

  override def getPlotTitle(column: Int) = {
    val (tag, col) = tagAndColFromCol(column)
    columnNames(tag)(col)
  }

  override def getColumnName(column: Int) = {
    val (tag, col) = tagAndColFromCol(column)
    columnNames(tag)(col)
  }

  def getColumnName(t: Tag, col: Int) = columnNames(t)(col)

  // Must be implemented for the trait but it is not relevant because of the tags
  override def isEmpty() = tags.isEmpty
  override def getTimes(t: Tag) = times(t)
  override def getTags() = tags.toSet
  override def getDeadTags() = deadTags.toSet

  /** Get the variables for plotting */
  override def getPlottables(parms: PlotParms, tagSet: Set[Tag]): im.Iterable[Plottable] = {
    val res = new ListBuffer[Plottable]
    val pickedTags = if (tagSet.isEmpty) tags else tagSet //Empty tagSet means all tags

    pickedTags foreach (t => {
      for ((result, idx) <- stores(t).zipWithIndex if !deadTags.contains(result.key.tag)) {
        // take each row from the store
        (result, result.key.fieldName.x) match {
          case (rs: DoubleResult, fn) if !specialField(fn) &&
            (parms.plotSimulator || !result.isSimulator) &&
            (parms.plotNextChild || fn != "nextChild") &&
            (parms.plotSeeds || (fn != "seed1" && fn != "seed2")) =>
            res += new PlotDoubles(result.isSimulator, result.key, result.startFrame, idx, rs)

          case (rs: EnclosureResult, fn) =>
            val collE: scala.collection.immutable.IndexedSeq[Enclosure] = rs.coll.map {
              i => Enclosure(i.loDouble, i.hiDouble, i.loDouble, i.hiDouble)
            }.toIndexedSeq
            res += new PlotEnclosure(result.isSimulator, result.key, result.startFrame, idx, collE)

          case (rs: GenericResult, fn) => rs.coll match {
            case collGV: ResultCollector[GValue] =>
              if (collGV.nonEmpty) // there is only one row in rs since we are taking each row in a loop
                collGV.head match {
                  case VLit(_: GStr | _: GDiscreteEnclosure[_]) =>
                    res += new PlotDiscrete(result.isSimulator, result.key, result.startFrame, idx, rs)
                  case VVector(n) =>
                  case _ => ()
                }
            case _ => () // Something wrong, coll should always be a ResultCollector[GValue]
          }
          case _ => () // Something wrong, there are only two types of the result
        }
      }
    })
    res.toList
  }

  override def getProba: Option[ProbaData] = probaData

  // FIXME Not a good place for a writing accessor.
  override def setProba(pd: Option[ProbaData]) = probaData = pd
}

/** CStore model used in CStoreCntrl */
class CStoreModel(ops: CStoreOpts) extends InterpreterModel {
  val MMap = collection.mutable.Map
  private var stores = MMap.empty[Tag, ArrayBuffer[ResultCollector[_]]] // The store that used for plotting and table
  def emptyStores = new ArrayBuffer[ResultCollector[_]]
  private var classes = MMap.empty[Tag, HashMap[CId,ClassName]] // The className and CId that include in the table
  def emptyClasses = new HashMap[CId,ClassName]
  private var indexes = MMap.empty[Tag, HashMap[ResultKey,Int]]
  def emptyIndexes = new HashMap[ResultKey,Int]
  private var ids = MMap.empty[Tag, HashSet[CId]]  // The id of the object
  def emptyIds = new HashSet[CId]
  private var frame = MMap.empty[Tag, Int].withDefaultValue(0)  // the row number
  private var timeKey = MMap.empty[Tag, ResultKey] // The result key for "time"

  // package all variables that need to be updated Atomically into
  // a case class
  case class Data(model: DataModel, updatingData: Boolean)
  @volatile var dataModel: Data = Data(emptyDataModel, updatingData = false)
  def emptyDataModel = DataModel(MMap.empty, MMap.empty, MMap.empty,MMap.empty, mutable.ArrayBuffer.empty, mutable.ArrayBuffer.empty)

  val pending = collection.mutable.Map.empty[Tag, ArrayBuffer[TraceData]]
  val pendingDeadTags = mutable.Set.empty[Tag]
  def addData(t: Tag, deadTag: Boolean, sts:TraceData) = {
    pending.synchronized {
      if (!pending.contains(t)) pending += t -> new ArrayBuffer
      pending(t).synchronized {
        pending(t) += sts
      }
      if (deadTag) pendingDeadTags.synchronized {
        pendingDeadTags += t
      }
    }
  }

  def flushPending() : Unit = {
    try {
      pending.synchronized {
        if (dataModel.updatingData)
          throw new java.lang.IllegalStateException("dataModel already being updated")
        if (pending.isEmpty)
          return
        dataModel = dataModel.copy(updatingData = true)
        pending.keys foreach (t => {
          var pd: Array[TraceData] = null
          pending(t).synchronized {
            pd = pending(t).toArray
            pending -= t
          }
          for (sts <- pd)
            addDataHelper(t, sts) // add the pending data
          dataModel.model.setIfNotContains(t)
          dataModel.model.columnNames(t) = stores(t).indices.map { i => getColumnNameLive(t, i) }
          dataModel.model.rowCount(t) = frame(t)
          dataModel.model.times(t) = stores(t)(indexes(t)(timeKey(t))).asInstanceOf[DoubleResultCollector].snapshot
          dataModel.model.stores(t) = stores(t).view.map { st => st.snapshot }.toIndexedSeq
          pendingDeadTags.synchronized {
            if (pendingDeadTags.contains(t)) {
              dataModel.model.deadTags add t; pendingDeadTags -= t
            }
          }
        })
      }
      dataModel = dataModel.copy(updatingData = false)
    } catch {
      case e =>
        dataModel = dataModel.copy(updatingData = false)
        throw e
    }
  }

  /** Construct the name of object for plotter and table,
    * such as: ResultKey: simulator(0, 0) -> (#0 : Main).simulator
    * Main is the name of simulator's parent */
  def getColumnNameLive(tag: Tag, col:Int) = {
    try {
      val ResultKey(t, id,x,i) = stores(tag)(col).key
      "(#" + id + " : " + classes(tag)(id).x + ")." + x.x + "'" * x.primes +
      (i match {
        case Some(k) => k._2 match{
          case Some(j) => "("+k._1+ "," + j +")"
          case None => "("+k._1+")"
        }
        case None => "" }) +
      t.pretty
    } catch { case _ => "" }  // If something is wrong, the name that being plotted is empty
  }

  //  Add value to the resultCollectors
  private def addVal(tag: Tag, id: CId, name: Name, value: GValue) = {
    def add(idx: Int, vectorIdx: Option[(Int,Option[Int])], v: GValue) =
      (stores(tag)(idx), v) match {
        case (sts:DoubleResultCollector, _) =>  sts += extractDoubleNoThrow(v)
        case (sts: EnclosureResultCollector, VLit(GConstantRealEnclosure(i))) => sts += i
        case (sts: EnclosureResultCollector, _) => throw ShouldNeverHappen()
        case (sts:GenericResultCollector, _) => sts += v
      }
    if(name.x.split("__")(0) == "pattern"  || name.x.contains(hashVariable)) ()
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
          // the variable already exists in the indexes, add the value to the stores
          if (indexes(tag).contains(ResultKey(tag ,id, name, vectorIdx))) {
            val idx = indexes(tag)(ResultKey(tag, id, name, vectorIdx))
            add(idx, vectorIdx, addedValue)
          } else {  // the variable is filtered out before, need to be inserted into the store
            val ar = newResultObj(tag, addedValue, id, name, false, vectorIdx)
            stores(tag) += ar
            indexes(tag) += ((ar.key, stores.size-1))
          }
      }
    }
  }

  /** Format the GValue to result that can be used for plotter and table */
  private def newResultObj(tag: Tag, value: GValue, id: CId, name: Name, isSimulator: Boolean,
                           vectorIdx: Option[(Int,Option[Int])]): ResultCollector[_] = {
    value match {
      case VLit(GDouble(_)|GInt(_)) =>
        val ar = new DoubleResultCollector(ResultKey(tag, id, name, vectorIdx), isSimulator, frame(tag))
        ar += extractDoubleNoThrow(value)
        ar
      case VLit(GConstantRealEnclosure(i)) =>
        val ar = new EnclosureResultCollector(ResultKey(tag, id, name, vectorIdx), isSimulator, frame(tag))
        ar += i
        ar
      case _ =>
        val ar = new GenericResultCollector(ResultKey(tag, id, name, vectorIdx), isSimulator, frame(tag))
        ar += value
        ar
    }
  }

  private def addDataHelper(t: Tag, sts:TraceData) = {
    def compIds(ido1:(CId,_), ido2:(CId,_)) = ido1._1 < ido2._1
    def compFields(p1:(Name,GValue),p2:(Name,GValue)) =
      Ordering[(String,Int)] lt ((p1._1.x, p1._1.primes),(p2._1.x, p2._1.primes))
    for (st <- sts) {
      for ((id,o) <- st.asInstanceOf[GStore].toList sortWith compIds) {
        if(!ids.contains(t)) ids += ((t, emptyIds))
        if (ids(t) contains id) // add the values of this object, if its CId is contained in the ids HashSet
          for ((name,value) <- o) addVal(t, id, name, value)
        else {  // add the object into the stores, indexes and ids
          val className = classOf(o)
          val isSimulator = className == cmagic
          if (!timeKey.contains(t) && isSimulator)
            timeKey += t -> ResultKey(t, id, Name("time", 0), None)
          if(!classes.contains(t)) classes += ((t, emptyClasses))
          classes(t) += ((id, className))
          for ((name,value) <- o.toList sortWith compFields) {
            def addObject(addedValue: GValue, vectorIdx: Option[(Int,Option[Int])]): Unit = {
              addedValue match {
                case VVector(values) =>
                  for ((ui,i) <- values.zipWithIndex) {
                    ui match{
                      case VVector(columns) =>
                        for((uii,ii) <- columns.zipWithIndex)
                          addObject(uii, Some(i,Some(ii)))
                      case _ =>
                        addObject(ui, Some(i, None))
                    }
                  }
                case _ =>
                  var ar = newResultObj(t, addedValue, id, name, isSimulator, vectorIdx)
                  if(!stores.contains(t)) stores += ((t, emptyStores))
                  stores(t) += ar
                  if(!indexes.contains(t)) indexes += ((t, emptyIndexes))
                  indexes(t) += ((ar.key, stores(t).size-1))
                  ids(t) += id
              }
            }
            if ( !specialField(name.x) && name.x.split("__")(0) != "pattern" )
              addObject(value, None)
          }
        }
      }
      if (!frame.contains(t)) frame += t -> (if(frame.nonEmpty) frame.values.max else 0)
      frame(t) += 1
    }
  }

  override def getNewData = {flushPending(); dataModel.model}

  override def getPlotModel = {flushPending(); dataModel.model}
  override def getTraceModel = {flushPending(); dataModel.model}

  override def getPlotter = new acumen.ui.plot.CStorePlotter()

  override def flush() = {flushPending()}

}
