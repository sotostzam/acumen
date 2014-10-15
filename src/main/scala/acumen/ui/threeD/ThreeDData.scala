package acumen
package ui
package threeD

import Errors._
import scala.collection.JavaConversions._
import scala.collection.mutable.Map
import scala.swing._
import util.Canonical._
import util.Conversions._

/* Get the 3D-visualization data */
class ThreeDData extends Publisher {
  /* Stores all the information for 3D-visualization */
  type _3DStore = Map[CId, _3DClass];
  /* Stores 3D-visualization information for a class */
  type _3DClass = Map[Int, List[List[_]]]
  type ViewInfo  = Tuple2[Array[Double], Array[Double]]
  var _3DData: _3DStore = Map[CId, _3DClass]()
  /* The number of 3D-objects */
  var objectCount = 1;
  var frameNumber = 0
  /* Used for determine 3D-visualization play speed */
  var endTime = 0.0;
  /* Default settings to transform Acumen AST to generic data can be used later for Java3D
       Example : GStr("Sphere") => "Sphere" 	                                            */
  var _3DType = "Sphere"
  var _3DPosition = Array[Double](0.0, 0.0, 0.0)
  var _3DSize = Array[Double]();
  var _3DColor = Array[Double](1.0, 1.0, 1.0)
  var _3DAngle = Array[Double](0.0, 0.0, 0.0)
  var _3DPath = ""
  var _3DText = ""
  /* Optinal field to indicate transparent object or not */
  var _3DTexture = ""
  /* Camera's position and orientation*/
  var _3DView = List[ViewInfo]()
  def reset() {
    _3DData.clear
    _3DView = List[ViewInfo]()
    frameNumber = 0;
  }
  def init3DClassStore(id: CId, _3DData: _3DStore, objectCount: Int): Unit = {
    var temp: _3DClass = Map[Int, List[List[_]]]();
    for (i <- 0 to objectCount - 1) {
      temp += i -> List[List[_]]()
    }
    _3DData += id -> temp
  }
  def isVectorOfNumbers(l: List[_]): Boolean = {
    var result = true;
    for (x <- l)
      x match {
        case VLit(GInt(i)) =>
        case VLit(GDouble(i)) =>
        case _ => result = false;
      }
    result
  }
  /* _3DType should be a String or a Integer */
  def extractType(value: Value[_]) {
    value match {
      case VLit(GStr(s)) => this._3DType = s;
      case VLit(GInt(i)) => this._3DType = i.toString;
      case _ => throw _3DNameError(value)
    }
  }
  /* Check if the list's size is 3 and only contains numbers (int, double) */
  def checkVectorContent(l: List[_]): Boolean = {
    (l.size == 3) && isVectorOfNumbers(l)
  }
  /* _3D Position,color,angle should all be a vector with 3 numbers */
  def extractVector(value: Value[_], index: String) {
    def helper(value: Value[_]):Array[Double] = { 
      value match {
        case VLit(GPattern(ls)) => helper(VVector(ls map VLit))
        case VVector(vs) => {
          if (checkVectorContent(vs))
            extractDoubles(vs).toArray
          else throw _3DVectorError(value, index);
         }
        case _ => throw _3DVectorError(value, index);
       }
     }
    val temp = helper(value)  
    index match {
      case "position" => _3DPosition = temp
      case "color" => _3DColor = temp
      case "angle" => _3DAngle = temp
      case _ => throw ShouldNeverHappen()
    }
  }
  
  /* _3D OBJ path should be a string */
  def extractPath(value: Value[_]){
	value match {
		case VLit(GStr(obj)) => _3DPath = obj
		case _ => throw _3DNameError(value)
	}
  }
  /* _3D texture should be a string */
  def extractTexture(value: Value[_]){
	value match {
		case VLit(GStr(tex)) => _3DTexture = tex
		case _ => throw _3DNameError(value)
	}
  }
  
 def extractText(value: Value[_]) {
    value match {
      case VLit(GStr(s)) => _3DText = s
      case VLit(GInt(i)) => _3DText = i.toString
      case VLit(GDouble(i)) => _3DText = i.toInt.toString
      case _ => throw _3DNameError(value)
    }
  }
  /* _3D size should be either a vector or an number */
  def extractSize(value: Value[_]) {
    value match {
      case VLit(GPattern(ls)) => extractSize(VVector(ls map VLit))
      case VVector(vs) => {
        if (isVectorOfNumbers(vs)) _3DSize = extractDoubles(vs).toArray
        else { _3DSize = Array[Double](); throw _3DSizeError(value) }
      }
      case VLit(GInt(x)) => _3DSize = Array(x.toDouble)
      case VLit(GDouble(x)) => _3DSize = Array(x)
      case _ => throw _3DSizeError(value)
    }
    /* Check if the _3DSize is valid */
    this._3DType match {
      case "Sphere" => if (_3DSize.length != 1) throw _3DSphereSizeError()
      case "Cylinder" => if (_3DSize.length != 2) throw _3DCylinderSizeError()
      case "Cone" => if (_3DSize.length != 2) throw _3DConeSizeError()
      case "Box" => if (_3DSize.length != 3) throw _3DBoxSizeError()
      /* 3D text's size should be a number */
      case _ => if (_3DSize.length != 1) throw _3DTextSizeError()
    }
  }

  /* Add new information of each 3D-object to _3DStore */
  def addTo3DStore(id: CId, _3DData: _3DStore, value: List[Value[_]], objectCount: Int) {
    if (!_3DData.contains(id))
      init3DClassStore(id, _3DData, objectCount);

    for (i: Int <- 0 to objectCount - 1) {
      val vector = value(i)
      vector match {
        case VVector(l) => {
          if (l.size != 5 && l.size != 6)
            throw _3DError(vector)
          else {
            extractType(l(0))
            extractSize(l(2))
            extractVector(l(1), "position")
            extractVector(l(3), "color")
            extractVector(l(4), "angle")
            if (_3DType == "Text")
			        extractText(l(5))
			      else if (_3DType == "OBJ")
			        extractPath(l(5))
			      else if(l.size == 6)
			        extractTexture(l(5))
          }
        }
        case _ => throw ShouldNeverHappen()
      }
      if (_3DType == "Text")
	      _3DData(id)(i) = List(_3DType, _3DPosition,
          _3DSize, _3DColor, _3DAngle, _3DText,frameNumber) :: _3DData(id)(i)
	    else if (_3DType == "OBJ")
	      _3DData(id)(i) = List(_3DType, _3DPosition,
          _3DSize, _3DColor, _3DAngle, _3DPath, frameNumber) :: _3DData(id)(i)
      else if(_3DTexture == "transparent")
        _3DData(id)(i) = List(_3DType, _3DPosition,
          _3DSize, _3DColor, _3DAngle, _3DTexture, frameNumber) :: _3DData(id)(i)
      else
        _3DData(id)(i) = List(_3DType, _3DPosition,
          _3DSize, _3DColor, _3DAngle, frameNumber) :: _3DData(id)(i)
       
      _3DTexture = ""   
    }
  }
  /* Look for endTime in "Main" class */
  def lookUpEndTime(id: CId, o: GObject) {
    if (id.equals(new CId(List(0)))) {
      for ((name, value) <- o) {
        if (name.x == "endTime") {
          this.endTime = extractDouble(value)
        }
      }
    }
  }

/* Look for 3D camera's position and orientation in "Main" class */
  def lookUpViewInfo(id: CId, o: GObject) {
    //  CId(List()) is the main class
    if(id.equals(new CId(List()))) {
      for ((name, value) <- o) {
        if (name.x == "_3DView") {
              value match {
              case VVector(l) => {
                  if(l.size > 0)
	            _3DView = new Tuple2(extractDoubles(l(0)).toArray,
	                                 extractDoubles(l(1)).toArray) :: _3DView       
                }
              case _ => throw _3DError(value)
           }
           
        }
      }
    }
  }
  /* Add _3D information of every class to _3DStore */
  def getData(s: GStore) {
    for ((id, o) <- s) {
      lookUpEndTime(id, o)
      lookUpViewInfo(id, o)
      /* Look for variable named _3D */
      for ((name, value) <- o) {
        if (name.x == "_3D") {
          value match {
            case VVector(l) => {
             if (l.size == 0)
                { }//
             else
                l(0) match {
                  /* If it's only one object, _3D will start with a string or an int, 
								   *		example:  _3D = ["Sphere",...,...,..] 
							     *	    		  	_3D = [2,...,...,..];  
							     */
                  case VLit(_) => addTo3DStore(id, _3DData, List(value), 1)
                  /**
                   * If it contains multiple objects, _3D will start with a vector,
                   * 		example : _3D = [["Sphere",[],[]...],
                   *                    ["Sphere",[],[]...]..]
                   */
                  case VVector(some) => addTo3DStore(id, _3DData, l, l.size)
                  case _ => throw _3DError(value)
                }
            }
            case _ => throw _3DError(value)
          }
        }
      }
    }
    frameNumber += 1
  }
  /* Check if model source contains any _3D variable declarations. */  
  def modelContains3D() = _3DData.nonEmpty
    
}
