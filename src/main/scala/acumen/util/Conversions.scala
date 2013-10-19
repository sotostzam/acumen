package acumen
package util

import scala.math._
import Errors._

object Conversions {

  def extractDouble(v:GroundValue) : Double = 
    v match {
      case GInt(i)    => i.toDouble
      case GDouble(x) => x
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
      case _        => throw ConversionError(v, "int")
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

}
