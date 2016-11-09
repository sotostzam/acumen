package acumen
package util

import scala.math._
import Errors._
import acumen.interpreters.enclosure.Interval
import acumen.TAD.TDif
import acumen.FAD.FDif

object Conversions {

  def extractBoolean(v:GroundValue) : Boolean =
    v match {
      case GBool(b) => b
      case _ => throw GroundConversionError(v, "boolean")
    }
  
  def groundvalueToStatic(gd: GroundValue): StaticGroundValue = gd match{
    case GInt(i) => GRational(i)
    case GDouble(d) => GRational(d)
    //case other => other.asInstanceOf[StaticGroundValue] 
   }
   def staticvalueToGround(gd: StaticGroundValue): GroundValue  = gd match{
    case GRational(i) => if (i.isWhole) GInt(i.toInt) else GDouble(i.toDouble)
    //case other => other.asInstanceOf[GroundValue] 
   }
  
  def extractBoolean(v:Value[_]): Boolean =
    v match {
      case VLit(gv) => extractBoolean(gv)
      case _        => throw ConversionError(v, "boolean")
    }
  
  def extractDouble(v:GroundValue) : Double = 
    v match {
      case GInt(i)    => i.toDouble
      case GDouble(x) => x
      case GRational(r) => r.toDouble
      case e: GRealEnclosure if (e isThin) => e.range.loDouble
      case GDoubleTDif(c) => c.coeff.head
      case GIntTDif(c) => c.coeff.head
      case i: GInterval => throw InvalidOperationOnIntervals(i)
      case _ =>throw GroundConversionError(v, "double").setPos(v.pos)
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

  def extractInt(v:GroundValue) : Int = {
    lazy val ed = extractDouble(v)    
    (v, ed) match { 
      case (GRational(i), _) if i.isWhole => i.toInt
      case (_, d) if doubleIsReal isValidInt d => doubleIsReal toInt d 
      case _ => throw GroundConversionError(v, "int")
    }
  } 
  
  def extractInt(v:Value[_]) : Int =
    v match {
      case VLit(gv) => extractInt(gv)
      case VVector(VLit(gv)::Nil) => extractInt(gv)
      case _        => throw ConversionError(v, "int")
    }

  def extractInterval(v: GroundValue): Interval =
    v match {
      case GInterval(i)              => i
      case GConstantRealEnclosure(i) => i
      case GInt(i)                   => Interval(i, i)
      case GDouble(d)                => Interval(d, d)
      
      case GIntTDif(TDif(tv,_))      => Interval(tv(0), tv(0))
      case GIntervalTDif(TDif(tv,_)) => tv(0)
      case GDoubleTDif(TDif(tv,_))   => Interval(tv(0), tv(0))
      
      case GCValueTDif(TDif(tv,_)) => tv(0) match {
        case VLit(GInterval(i))               => i
        case VLit(GConstantRealEnclosure(i))  => i
        case VLit(GInt(i))                    => Interval(i, i)
        case VLit(GDouble(d))                 => Interval(d, d)
        
        case VLit(GIntervalFDif(FDif(tfv,_))) => tfv
        
        case _ => throw GroundConversionError(v, "interval")
      }

      case e: GRealEnclosure => e.range

      case _ => throw GroundConversionError(v, "interval")
    }
  
  def extractInterval(v:Value[_]) : Interval =
      v match {
      case VLit(gv) => extractInterval(gv)
      case _        => throw ConversionError(v, "interval")
  }
  
  def extractString(v: GroundValue): String =
    v match {
      case GStr(s) => s  
      case e @ GStrEnclosure(s) if e.isThin => s.head  
      case _ => throw GroundConversionError(v, "string")
    }

  def extractString(v: Value[_]): String =
    v match {
      case VLit(gv) => extractString(gv)
      case _        => throw ConversionError(v, "string")
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
