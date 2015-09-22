package acumen
package interpreters
package enclosure2015

import breeze.linalg.{
  DenseVector, DenseMatrix 
}
import util.Canonical
import util.Canonical._
import util.Conversions._
import Common._
import Errors._
import Pretty.pprint
import enclosure.Interval
import enclosure2015.Interpreter.bannedFieldNames
import enclosure2015.Changeset._

/** Representation of the continuous state of a program.
 *  To avoid wrapping, the enclosure is represented as:
 *    midPoint + (transform * width) + error
 */
case class LohnerSet
  ( midPoint: DenseVector[Interval]
  , transform: DenseMatrix[Interval]
  , width: DenseVector[Interval]
  , error: DenseVector[Interval]) {
  private lazy val value = midPoint + (transform * width) + error
  def apply(i: Int): Interval = value(i)
}

/** Representation of the entire state of a program.
 *  Uses Lohner sets to represent continuous variables to avoid wrapping.
 *  Separate Lohner sets are used to represent the enclosure for the start-time, range and end-time. */
case class Enclosure
  ( private val st: CStore
  , lohnerSetStart: LohnerSet 
  , lohnerSetRange: LohnerSet 
  , lohnerSetEnd: LohnerSet ) {

  import Enclosure._
  
  private val idToIndex: Map[(CId,Name),Int] = odeVariables(st).zipWithIndex.toMap
  private val indexToId: Map[Int,(CId,Name)] = idToIndex.map{ case (idn, i) => (i, idn) }
  
  /* Enclosure as Map */
  
  def iterator: Iterator[(CId,CObject)] =
    // FIXME This should take the Lohner set into account!
    cStore.iterator
  def map(f: ((CId,CObject)) => (CId,CObject)): Enclosure =
    Enclosure.init((iterator map f).toMap)
  def flatMap(f: ((CId,CObject)) => scala.collection.GenTraversableOnce[(CId,CObject)]): Enclosure =
    Enclosure.init((iterator flatMap f).toMap)
  def filter(f: ((CId,CObject)) => Boolean): Enclosure =
    Enclosure.init((iterator filter f).toMap)
  def foreach(p: ((CId,CObject)) => Unit): Unit =
    // FIXME This should take the Lohner set into account!
    iterator foreach p
  def forall(p: ((CId,CObject)) => Boolean): Boolean =
    // FIXME This should take the Lohner set into account!
    iterator forall p
  def toList: List[(CId,CObject)] =
    iterator.toList
  
  /* Store Operations */
  
  def cStore: CStore = st
  
  /** Returns true if this is an enclosure of the same set of objects as that, and false otherwise. */
  def objectIds: Set[CId] =
    cStore.keySet
    
  def getObject(id: CId): CObject =
    cStore(id)
  def getObjectField(id:CId, f:Name): CValue =
    // FIXME Introduce a flag to check if st is up to date w.r.t.
    //       lohnerSet. Use it to check when to cache all 
    //       odeVariables values in st. 
    idToIndex.get((id,f)) match {
      case Some(i) =>
        // FIXME Return enclosure with range and end-time intervals
        VLit(GConstantRealEnclosure(lohnerSetRange(i)))
      case _ =>
        val obj = getObject(id)
        obj.get(f) match {
          case Some(v) => v
          case None => 
            println("Tried to look up " + (id, f) + " in:\n\n" + obj.mkString("\n"))
            throw VariableNotDeclared(f)
        }        
    }
  def setObject(id:CId, o:CObject) : Enclosure =
      update(o.map{ case (n,v) => (id,n) -> v })
  def setObjectField(id:CId, f:Name, v:CValue) : Enclosure = {
    if (f != _3D && f != _3DView && f != devicef)
      getObject(id).get(f) map { oldVal =>
        if (oldVal.yieldsPlots != v.yieldsPlots)
          throw new UnsupportedTypeChangeError(f, id, classOf(getObject(id)), oldVal, v, 
            "These values require a different number of plots")
      }
    update(Map((id,f) -> v))
  }
  def changeParent(id:CId, p:CId) : Enclosure =
    update(Map((id, parent) -> VObjId(Some(p))))
     
  /* Simulator Object Operations 
   * NOTE: These operations only affect st */  

  def getInSimulator(s:String): CValue = 
    getInSimulator(Name(s,0))
  def getInSimulator(f:Name) = 
    getObjectField(simulatorId, f)
  def getTime: Double =
    Canonical getTime cStore
  def getEndTime: Double =
    Canonical getEndTime cStore
  def getTimeStep: Double =
    Canonical getTimeStep cStore
  def getResultType: ResultType =
    Canonical getResultType cStore
  def getCls(id: CId): ClassName = 
    Canonical classOf deref(id,cStore)
  def mainId: CId = 
    Canonical mainId cStore
  def simulatorId: CId = 
    Canonical magicId cStore
  def childrenOf(id: CId): List[CId] = 
    Canonical childrenOf (id, cStore)
  def checkAccessOk(id:CId, env:Env, context: Expr): Unit =
    Common checkAccessOk (id, env, cStore, context)
  /**
   * Updates the variableCount simulator field. This corresponds to 
   * the number of plots of model variables that will be shown, but not the 
   * number of columns in the table, as the latter also contains information 
   * such as class names etc. 
   */
  def countVariables: Enclosure = 
    setObjectField(simulatorId, stateVars, VLit(GInt(countStateVars(cStore))))
    
  /* Enclosure Operations */
  
  /** Take the meet, or g.l.b., of e and that Enclosure. */
  def /\(that: Enclosure): Enclosure = this meet that
  def /\(that: Option[Enclosure]): Enclosure = if (that isDefined) this meet that.get else this
  /** Take the meet, or g.l.b., of e and that Enclosure. */
  def meet (that: Enclosure): Enclosure = merge(that: Enclosure, (l: GEnclosure[_], r: GEnclosure[_]) => (l,r) match {
    case (le: GConstantRealEnclosure, re: GConstantRealEnclosure) => Some(le /\ re)
    case (ls: GStrEnclosure, rs: GStrEnclosure) => Some(GStrEnclosure(ls.start union rs.start, ls.range union rs.range, ls.end union rs.end))
    case (ls: GIntEnclosure, rs: GIntEnclosure) => Some(GIntEnclosure(ls.start union rs.start, ls.range union rs.range, ls.end union rs.end))
    case (ls: GBoolEnclosure, rs: GBoolEnclosure) => Some(GBoolEnclosure(ls.start union rs.start, ls.range union rs.range, ls.end union rs.end))
  }).get
  /** Merge e and that Enclosure using ce to combine scalar enclosure values. */
  def merge(that: Enclosure, ce: (GEnclosure[_], GEnclosure[_]) => Option[GEnclosure[_]]): Option[Enclosure] = {
    require(this.objectIds == that.objectIds, "Can not merge enclosures with differing object sets.") // TODO Update for dynamic objects
    try {
      Some(for ((cid,co) <- this) yield { 
        val tco = that getObject cid
        require(co.keySet == tco.keySet, "Can not merge objects with differing name sets.") // TODO Update for dynamic objects
        require(classOf(co) == classOf(tco), s"Can not merge objects of differing models (${classOf(co).x}, ${classOf(tco).x}).")
        (cid, if (classOf(co) == cmagic)
          co // FIXME Sanity check this (that it is OK to merge Enclosures with different simulator objects)
        else
          for ((n, v) <- co) yield (n,
            (v, tco(n)) match {
              case (VLit(l: GEnclosure[_]), VLit(r: GEnclosure[_])) => VLit(ce(l, r).getOrElse(sys.error("Error when merging $cid.$n.")))
              case (l, r) if l == r                                 => l
            })
      )})
    } catch { case e: Throwable => None } // FIXME Use Either to propagate error information 
  }
  /** Returns a copy of e with das applied to it. */
  def apply(das: Set[CollectedAction])(implicit evalExpr: (Expr, Env, Enclosure) => CValue): Enclosure = 
    update(das.map(d => (d.selfCId, d.lhs.field) -> evalExpr(d.rhs, d.env, this)).toMap)
  /** Update st with respect to u, using st to re-initialize the Lohner sets. 
   *  TODO Avoid re-initializing the Lohner sets! */
  def update(u: Map[(CId, Name), CValue]) = Enclosure.init( 
    (for (id <- st.keySet ++ u.map{ case ((i,_),_) => i })
     yield id -> { val idUpdates = u.flatMap{ 
                     case ((`id`,n),v) => Map(n -> v); case _ => Nil }
                   st.get(id).fold(idUpdates)(_ ++ idUpdates)
                 }).toMap)
  /** Field-wise containment. */
  def contains(that: Enclosure): Boolean = { // TODO Update for dynamic objects
    def containsCObject(lo: CObject, ro: CObject): Boolean =
      if (classOf(lo) == cmagic && classOf(ro) == cmagic){
        if (lo(time) == ro(time) && lo(timeStep) == ro(timeStep))
          true
        else 
          throw internalError("Attempt to check containment of enclosures with incompatoble time domains: " +
            s" [${lo(time)},${extractDouble(lo(time)) + extractDouble(lo(timeStep))}]" +
            s", [${ro(time)},${extractDouble(ro(time)) + extractDouble(ro(timeStep))}]") 
      }
      else lo.forall {
        case (n, v) =>
          if (bannedFieldNames contains n) true
          else {
            ((v, ro get n) match {
              case (VLit(l: GConstantRealEnclosure), Some(VLit(r: GConstantRealEnclosure))) => 
                l contains r
              case (VLit(l: GStrEnclosure), Some(VLit(r: GStrEnclosure))) => 
                l contains r
              case (VLit(l: GIntEnclosure), Some(VLit(r: GIntEnclosure))) => 
                l contains r
              case (VLit(l: GBoolEnclosure), Some(VLit(r: GBoolEnclosure))) => 
                l contains r
              case (VLit(_:GStr) | _:VResultType | _:VClassName, tv @ Some(VLit(_:GStr) | _:VResultType | _:VClassName)) => 
                v == tv
              case (VObjId(Some(o1)), Some(VObjId(Some(o2)))) => 
                containsCObject(this getObject o1, that getObject o2)
              case (_, Some(tv)) => 
                throw internalError(s"Contains not applicable to ${pprint(n)}: ${pprint(v)}, ${pprint(tv)}")
              
            })}}
      this.forall { case (cid, co) => containsCObject(co, that getObject cid) }
    }
  /** Take the intersection of e and that Object. */
  def intersect(that: Enclosure): Option[Enclosure] = merge(that, (l: GroundValue, r: GroundValue) => ((l,r): @unchecked) match {
    case (le: GConstantRealEnclosure, re: GConstantRealEnclosure) => le intersect re 
    case (ls: GStrEnclosure, rs: GStrEnclosure) => (ls.start intersect rs.start, ls.enclosure intersect rs.enclosure, ls.end intersect rs.end) match {
      case (start, enclosure, end) if start.nonEmpty && enclosure.nonEmpty && end.nonEmpty => Some(GStrEnclosure(start,enclosure,end))
      case _ => sys.error(s"Empty intersection between string enclosures $ls and $rs") // FIXME Use Either to propagate error information 
    }
    case (ls: GIntEnclosure, rs: GIntEnclosure) => (ls.start intersect rs.start, ls.enclosure intersect rs.enclosure, ls.end intersect rs.end) match {
      case (start, enclosure, end) if start.nonEmpty && enclosure.nonEmpty && end.nonEmpty => Some(GIntEnclosure(start, enclosure, end))
      case _ => sys.error(s"Empty intersection between integer enclosures $ls and $rs") // FIXME Use Either to propagate error information 
    }
    case (ls: GBoolEnclosure, rs: GBoolEnclosure) => (ls.start intersect rs.start, ls.enclosure intersect rs.enclosure, ls.end intersect rs.end) match {
      case (start, enclosure, end) if start.nonEmpty && enclosure.nonEmpty && end.nonEmpty => Some(GBoolEnclosure(start, enclosure, end))
      case _ => sys.error(s"Empty intersection between boolean enclosures $ls and $rs") // FIXME Use Either to propagate error information 
    }
  })
  /** Field-wise projection. Replaces each enclosure with a new one corresponding to its start-time interval. */
  def start(): Enclosure = Enclosure(mapEnclosures{ 
    case ce: GConstantRealEnclosure => GConstantRealEnclosure(ce.start) 
    case ui: GIntEnclosure => GIntEnclosure(ui.start, ui.start, ui.start) 
    case us: GStrEnclosure => GStrEnclosure(us.start, us.start, us.start) 
    case us: GBoolEnclosure => GBoolEnclosure(us.start, us.start, us.start) 
  }, lohnerSetStart)
  /** Field-wise range. */
  def range(): Enclosure = Enclosure(mapEnclosures{ 
    case ce: GConstantRealEnclosure => GConstantRealEnclosure(ce.enclosure) 
    case ui: GIntEnclosure => GIntEnclosure(ui.range, ui.range, ui.range) 
    case us: GStrEnclosure => GStrEnclosure(us.range, us.range, us.range) 
    case us: GBoolEnclosure => GBoolEnclosure(us.range, us.range, us.range) 
  }, lohnerSetRange)
  /** Field-wise projection. Replaces each enclosure with a new one corresponding to its end-time interval. */
  def end(): Enclosure = Enclosure(mapEnclosures{ 
    case ce: GConstantRealEnclosure => GConstantRealEnclosure(ce.end) 
    case ui: GIntEnclosure => GIntEnclosure(ui.end, ui.end, ui.end) 
    case us: GStrEnclosure => GStrEnclosure(us.end, us.end, us.end) 
    case us: GBoolEnclosure => GBoolEnclosure(us.end, us.end, us.end) 
  }, lohnerSetEnd)
  /** Returns a copy of this.st where f has been applied to all enclosure fields. */
  def mapEnclosures(f: GEnclosure[_] => GEnclosure[_]): CStore =
    st map{ case (cid,co) => (cid, co.mapValues{
      case VLit(ce: GEnclosure[_]) => VLit(f(ce))
      case field => field
    })}
  /** Use f to reduce this enclosure to a value of type A. */
  def foldLeft[A](z: A)(f: (A, GConstantRealEnclosure) => A): A =
    this.flatten.foldLeft(z) { case (r, (id, n, e)) => f(r, e) }
  /** Returns iterable of all GConstantRealEnclosures contained in this object and its descendants. */
  def flatten(): Iterable[(CId, Name, GConstantRealEnclosure)] =
    cStore.flatMap{ case (cid, co) => co.flatMap{ 
      case (n, VLit(ce:GConstantRealEnclosure)) => List((cid, n, ce))
      case _ => Nil
    }}
}
object Enclosure {
  /** Initialize the Lohner Set of an Enclosure based on a CStore */
  def init(st: CStore): Enclosure = {
    val odeVs = odeVariables(st)
    def mkDenseVector(f: GEnclosure[Interval] => Interval) =
      DenseVector(odeVs.map { case (id, n) => getObjectField(id, n, st) match {
        case VLit(e: GEnclosure[Interval]) => f(e)
      }}: _*)
    val start = LohnerSet(
      midPoint = mkDenseVector(e => Interval(e.start.midpoint)),
      transform = DenseMatrix.tabulate(odeVs.length, odeVs.length) {
        case (i: Int, j: Int) => if (i == j) Interval.one else Interval.zero
      },
      width = mkDenseVector(e => e.start.width),
      error = mkDenseVector(e => Interval.zero)
    )
    new Enclosure(st, start, start, start)
  }
  /** Create an enclosure with start, range and end Lohner sets equal to ls */
  def apply(st: CStore, ls: LohnerSet): Enclosure =
    Enclosure(st, ls, ls, ls)
  /** Qualified names of all variables in st */
  def variables(st: CStore): Vector[(CId,Name)] =
    st.toVector.flatMap{ case (id,o) => o.toVector.map{ case (n,_) => (id, n) } }
  /** Qualified names of all variables that will be maintained in the Lohner sets of an Enclosure */
  def odeVariables(st: CStore): Vector[(CId,Name)] =
    // TODO Ensure that lookup in this data structure is fast!
    variables(st).filter{ case (id,n) =>
      id != magicId(st) && !(bannedFieldNames contains n) && (getObjectField(id, n, st) match {
        case VLit(_: GRealEnclosure) => true
        case _ => false
      })
    }
}

/* Scalar Enclosure Types */

case class GConstantRealEnclosure(start: Interval, enclosure: Interval, end: Interval) extends GRealEnclosure {
  require(enclosure contains end, // Enclosure may not contain start (initial condition), when the Lohner IVP solver is used 
    s"Enclosure must be valid over entire domain. Invalid enclosure: GConstantGConstantRealEnclosureEnclosure($start,$enclosure,$end)")
  override def apply(t: Interval): Interval = enclosure
  override def range: Interval = enclosure 
  override def isThin: Boolean = start.isThin && enclosure.isThin && end.isThin
  override def show: String = enclosure.toString
  def contains(that: GConstantRealEnclosure): Boolean =
    (start contains that.start) && (enclosure contains that.enclosure) && (end contains that.end)
  def /\ (that: GConstantRealEnclosure): GConstantRealEnclosure =
    GConstantRealEnclosure(start /\ that.start, enclosure /\ that.enclosure, end /\ that.end)
  def intersect(that: GConstantRealEnclosure): Option[GConstantRealEnclosure] =
    for {
      si <- start     intersect that.start
      e  <- enclosure intersect that.enclosure
      ei <- end       intersect that.end
    } yield GConstantRealEnclosure(si, e, ei)
}
object GConstantRealEnclosure {
  def apply(i: Interval): GConstantRealEnclosure = GConstantRealEnclosure(i,i,i)
  def apply(d: Double): GConstantRealEnclosure = GConstantRealEnclosure(Interval(d))
  def apply(i: Int): GConstantRealEnclosure = GConstantRealEnclosure(Interval(i))
}
abstract class GConstantDiscreteEnclosure[T](val start: Set[T], val enclosure: Set[T], val end: Set[T]) extends GDiscreteEnclosure[T] {
  def apply(t: Interval) = range
  def range = start union enclosure union end
  def isThin = start.size == 1 && enclosure.size == 1 && end.size == 1
  def show = s"{${enclosure mkString ","}}"
  def contains(that: GConstantDiscreteEnclosure[T]): Boolean =
    (that.start subsetOf this.start) && (that.enclosure subsetOf this.enclosure) && (that.end subsetOf this.end)
}
case class GStrEnclosure(override val start: Set[String], override val enclosure: Set[String], override val end: Set[String]) 
  extends GConstantDiscreteEnclosure[String](start, enclosure, end)
object GStrEnclosure {
  def apply(ss: Set[String]): GStrEnclosure = GStrEnclosure(ss, ss, ss)
  def apply(s: String): GStrEnclosure = GStrEnclosure(Set(s))
}
case class GIntEnclosure(override val start: Set[Int], override val enclosure: Set[Int], override val end: Set[Int]) 
  extends GConstantDiscreteEnclosure[Int](start, enclosure, end)
case class GBoolEnclosure(override val start: Set[Boolean], override val enclosure: Set[Boolean], override val end: Set[Boolean])  
  extends GConstantDiscreteEnclosure[Boolean](start,enclosure,end)
object GBoolEnclosure {
  def apply(ss: Set[Boolean]): GBoolEnclosure = GBoolEnclosure(ss, ss, ss)
  def apply(s: Boolean): GBoolEnclosure = GBoolEnclosure(Set(s))
}