package acumen
package util

import Errors._
import Names._
import Conversions._

object Canonical {

  /* special variables */
  val self         = name("self")
  val parent       = name("parent")
  val classf       = name("className")
  val magicf       = name("simulator")
  val devicef      = name("device")
  val time         = name("time")
  val timeStep     = name("timeStep")
  val resultType   = name("resultType")
  val endTime      = name("endTime") 
  val nextChild    = name("nextChild")
  val children     = name("children")
  val seed1        = name("seed1")
  val seed2        = name("seed2")
  val stateVars    = name("variableCount")
  val _3D          = name("_3D")
  val _3DView      = name("_3DView")
  val cmain        = ClassName("Main")
  val cmagic       = ClassName("Simulator")
  val cdevice      = ClassName("Device")

  /* object getters */
  def parentOf(o:CObject) : Option[CId] = { val VObjId(id) = o(parent); id }
  def classOf(o:CObject)  : ClassName = { val VClassName(cn) = o(classf); cn }
  def seedOf(o:CObject) : (Int,Int) = { extractSeed(o(seed1), o(seed2)) }

  /* compute children of id */
  def childrenOf(id:CId, st:CStore) : List[CId] =
    st filter { case (_,o) => parentOf(o) == Some(id) } map (_._1) toList

  /* count children of id */
  def childCount(id: CId, st: CStore): Int =
    childrenOf(id, st).size

  /* get the object id of a singleton object */
  def single(cn:ClassName)(st:CStore) : CId = 
    st find { case (_,o) => classOf(o) equals cn } match {
      case None => throw NoInstanceFound(cn)
      case Some((id,_)) => id
    }

  def mainId  = single(cmain) _
  def magicId = single(cmagic) _

  /* store query */

  def deref(a:CId, st:CStore) : CObject = st(a)

  def getObjectField(id:CId, f:Name, st:CStore) = {
    val obj = deref(id,st)
    obj.get(f) match {
      case Some(v) => v
      case None => throw VariableNotDeclared(f)
    }
  }

  // getClass is already defined in Any

  def getCls(id:CId, st:CStore) = 
    classOf(deref(id,st))

  def getParent(id:CId, st:CStore) = 
    parentOf(deref(id,st))

  def getSeed(id:CId, st:CStore) = 
    seedOf(deref(id,st))

  /* store modification */

  def setField(o:CObject, n:Name, v:CValue) : CObject = 
    if (o contains n) o.updated(n,v) 
    else throw VariableNotDeclared(n)

  def setObject(id:CId, o:CObject, s:CStore) : CStore =
    s updated (id, o)

  def setObjectField(id:CId, f:Name, v:CValue, s:CStore) : CStore = {
    val obj = deref(id,s)
    if (f != _3D && f != _3DView && f != devicef)
      obj.get(f) map { oldVal =>
        if (oldVal.yieldsPlots != v.yieldsPlots)
          throw new UnsupportedTypeChangeError(f, id, classOf(obj), oldVal, v, 
            "These values require a different number of plots")
      }
    setObject(id, setField(obj,f,v), s)
  }

  def setParent(o:CObject, a:CId) : CObject = 
    setField(o, parent, VObjId(Some(a)))

  def setSeed(o:CObject, s:(Int,Int)) : CObject = {
    val (s1,s2) = s
    val o1 = setField(o, seed1, VLit(GInt(s1)))
    setField(o1, seed2, VLit(GInt(s2)))
  }

  def changeParent(id:CId, p:CId, st:CStore) : CStore = {
    val obj = deref(id,st)
    setObject(id, setParent(obj, p), st)
  }

  def changeSeed(id:CId, s:(Int,Int), st:CStore) : CStore = {
    val obj = deref(id, st)
    setObject(id, setSeed(obj, s), st)
  }

  /* magic fields getters and setters */

  def getInSimulator(s:String, st:CStore): CValue = getInSimulator(Name(s,0), st)
  def getInSimulator(f:Name, st:CStore) = getObjectField(magicId(st), f, st)
  def setInSimulator(f:Name, v:CValue, s:CStore) = {
    val id = magicId(s)
    setObjectField(id, f, v, s)
  }

  def getTime(st:CStore)       = extractDouble(getInSimulator(time, st))
  def getTimeStep(st:CStore)   = extractDouble(getInSimulator(timeStep, st))
  def getEndTime(st:CStore)    = extractDouble(getInSimulator(endTime, st))
  def getResultType(st:CStore) = { val VResultType(t) = getInSimulator(resultType, st); t }

  def setTime(d:Double, s:CStore)       = setInSimulator(time, VLit(GDouble(d)), s)
  def setResultType(t:ResultType, s:CStore) = setInSimulator(resultType, VResultType(t), s)


  // helper methods for a GStore, used by CStoreModel
  def classOf(o:GObject)  : ClassName = { val VClassName(cn) = o.find{_._1 == classf}.get._2; cn }
  def magicId(st:GStore) : CId = 
    st find { case (_,o) => classOf(o) equals cmagic } match {
      case None => throw NoInstanceFound(cmagic)
      case Some((id,_)) => id
    }
  def deref(a:CId, st:GStore) : GObject = st.find{_._1 == a}.get._2
  def getObjectField(id:CId, f:Name, st:GStore) = deref(id,st).find{_._1 == f}.get._2
  def getInSimulator(f:Name, st:GStore) = getObjectField(magicId(st), f, st)
  def getTime(st:GStore)     = extractDouble(getInSimulator(time, st))
  def getEndTime(st:GStore)  = extractDouble(getInSimulator(endTime, st))

}

