package acumen
package util

import scala.math._
import Errors._
import acumen.interpreters.enclosure.Interval

object Conversions {

  def extractDouble(v:GroundValue) : Double = 
    v match {
      case GInt(i)    => i.toDouble
      case GDouble(x) => x
      case e: GRealEnclosure if (e isThin) => e.range.loDouble
      case GDoubleTDif(c) => c.coeff.head
      case GIntTDif(c) => c.coeff.head
      case _ => throw GroundConversionError(v, "double")
    }
  
  def extractDouble(v:Value[_]) : Double =
    v match {
      case VLit(gv) => extractDouble(gv)
      case _        => throw ConversionError(v, "double")
    }

  def extractDoubleNoThrow(v:Value[_]) : Double = 
    try {
      v match {
        case VLit(gv) => extractDouble(gv)
        case _        => throw ConversionError(v, "double")
      }
    } catch {
      case _ => Double.NaN
    }

  def extractDoubles(vs:List[Value[_]]) : List[Double] =
    vs map extractDouble

  def extractDoubles(v:Value[_]) : List[Double] =
     v match {
      case VList(vs) => extractDoubles(vs)
      case VVector(vs) => extractDoubles(vs)
      case _        => throw NotACollection(v)
    }

  def extractInt(v:GroundValue) : Int = 
    v match {
      case GInt(i)    => i
      case _ => throw GroundConversionError(v, "int")
    }
  
  def extractInt(v:Value[_]) : Int =
    v match {
      case VLit(gv) => extractInt(gv)
      case VVector(VLit(gv)::Nil) => extractInt(gv)
      case _        => throw ConversionError(v, "int")
    }

  def extractInterval(v: GroundValue): Interval =
    v match {
      case GInterval(i) => i
      case GInt(i) => Interval(i, i)
      case GDouble(d) => Interval(d, d)
      case e: GRealEnclosure => e.range
      case _ => throw GroundConversionError(v, "interval")
    }
  
  def extractInterval(v:Value[_]) : Interval =
      v match {
      case VLit(gv) => extractInterval(gv)
      case _        => throw ConversionError(v, "interval")
  }
  // Check that every row has the same length
  def isMatrix(m:List[Value[_]]):Boolean ={
    try{
    	val rowLength = m(0) match{
    	case VVector(vs) => vs.length
    	case _ => error("Row 0 is not a vector")
    	}
    	m.forall(x => x match{
    	case VVector(vs) => vs.length == rowLength
    	case _ => false
    	})}
    catch{
      case _:Throwable => false
    }
  }
  def transMatrixArray(m:List[Value[_]]):Array[Array[Double]] = {
    if(isMatrix(m)){
      m.map(x => x match{
        case VVector(vs) => extractDoubles(vs).toArray
        case _ => error(m.toString + " is not a matrix")
      }).toArray
      
    }
    else
     Array(m.map(x => extractDouble(x)).toArray)
  }
  
  def transArrayMatrix(a:Array[Array[Double]]):List[Value[_]] = {
    val ls = a.map(x => x.toList).toList
    ls.map(x => VVector(x.map(y => VLit(GDouble(y)))))		  
  }

  def extractIntervals(vs:List[Value[_]]) : List[Interval] =
    vs map extractInterval
  
  def extractIntervals(v:Value[_]) : List[Interval] =
     v match {
      case VList(vs) => extractIntervals(vs)
      case VVector(vs) => extractIntervals(vs)
      case _        => throw NotACollection(v)
    }
  
	def extractSeed(v1:Value[_], v2:Value[_]) : (Int, Int) = {
		(extractInt(v1), extractInt(v2))
	}

  def extractId[Id](v:Value[Id]) : Id = {
    v match {
      case VObjId(Some(id)) => id
      case _ => throw NotAnObject(v)
    }
  }
  
  /* Update vector e with a new value vt at position i */
  def updateEntry[A](e:Value[A],i:List[Int],vt:Value[A]):VVector[A]={
      e match {
      case VVector(l) => i match {
        case Nil => VVector(l)
        case idx :: Nil => 
          VVector(l.updated(idx,vt))       
        case idx::tail => 
          VVector(l.updated(idx,updateEntry(l(idx),tail,vt)))
      }
      case _ => throw CantIndex() }
  }
   /* Update vector e with multiple new values vts at corresponding positions is */
   def updateMultipleEntries[A](e:Value[A],is:List[List[Int]],vts:List[Value[A]]):VVector[A]={
      e match {
      case VVector(l) =>
        var result = VVector(l)
        for( (i,vt) <- is zip vts){
          result = updateEntry(result,i,vt)
        }
        result
      case _ => throw CantIndex() }
  }

}
