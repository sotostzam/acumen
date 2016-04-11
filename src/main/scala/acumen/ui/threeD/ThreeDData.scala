package acumen
package ui
package threeD

import acumen.Errors._
import acumen.util.Conversions._
import scala.collection.mutable
import scala.swing._

/* Get the 3D-visualization data */
class ThreeDData extends Publisher {
  /* Stores all the information for 3D-visualization */
  type _3DStore = mutable.Map[Int, _3DClass]
  /* Stores 3D-visualization information for a class */
  /* We store the CId and object number as a tuple,
  * which is equal to (CId,objectNumber) */
  type _3DClass = mutable.Map[(CId, Int), List[_]]
  type ViewInfo = (Array[Double], Array[Double])
  /* Key of _3DData is the frame number, 
     key of _3DClass is the (CId,objectNumber) */
  var _3DData = mutable.Map[Int, _3DClass]()
  /* The number of 3D-objects */
  var objectCount = 1
  var frameNumber = 0
  /* Used for determine 3D-visualization play speed */
  var endTime = 0.0f
  /* Default settings to transform Acumen AST to generic data can be
   * used later for constructing a primitive
       Example : GStr("Sphere") => "Sphere" */
  var _3DType = "Sphere"
  var _3DPosition = Array[Double](0.0, 0.0, 0.0)
  var _3DSize = Array[Double]()
  var _3DColor = Array[Double](1.0, 1.0, 1.0)
  var _3DAngle = Array[Double](0.0, 0.0, 0.0)
  var _3DPath = ""
  var _3DText = ""
  /* Optional field to indicate transparent object or not */
  var _3DTexture = ""
  var _3DCoordinates = "global"
  var _3DTransparency = -1.0
  /* Camera's position and orientation*/
  var _3DView = mutable.ArrayBuffer[ViewInfo]()
  /* CId of the object that contain _3DView info */
  var _3DViewID:CId = null
  /* Used for synchronize real time 3D-visualisation with real world time */
  protected[threeD] var timeStep = 0.0

  def reset() {
    _3DData.clear()
    _3DView.clear()
    frameNumber = 0
    _3DViewID = null
  }

  def isVectorOfNumbers(l: List[_]): Boolean = {
    var result = true
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

  /* Check if the list size of color and angle is 3 and only contains numbers (int, double) */
  def validVector(l: List[_]): Boolean = {
    (l.size == 3) && isVectorOfNumbers(l)
  }
  /* Check if the position's size is 3 or 2 and only contains numbers (int, double) */
  def validPosition(l: List[_]): Boolean = {
    (l.size == 3 || l.size == 2) && isVectorOfNumbers(l)
  }

  /* _3D Position,color,angle should all be a vector with 3 numbers */
  def extractVector(value: Value[_], index: String) {
    def helper(value: Value[_]): Array[Double] = {
      value match {
        case VLit(GPattern(ls)) => helper(VVector(ls map VLit))
        case VVector(vs) =>
          if (index != "position" && validVector(vs))
            extractDoubles(vs).toArray
          else if (index == "position" && validPosition(vs))
            extractDoubles(vs).toArray
          else throw _3DVectorError(value, index)
        case _ => throw _3DVectorError(value, index)
      }
    }
    val temp = helper(value)
    index match {
      case "position" => _3DPosition = if (temp.length == 2) Array(temp.apply(0), temp.apply(1), 0)
                                       else temp
      case "color" => _3DColor = temp
      case "angle" => _3DAngle = temp
      case _ => throw ShouldNeverHappen()
    }
  }

  /* _3D OBJ path should be a string */
  def extractPath(value: Value[_]) {
    value match {
      case VLit(GStr(obj)) => _3DPath = obj
      case _ => throw _3DNameError(value)
    }
  }

  /* _3D texture should be a string */
  def extractTransparency(value: Value[_]) {
    value match {
      case VLit(GDouble(transparency)) =>
        assignTransparency(transparency)
      case VLit(GInt(transparency)) =>
        assignTransparency(transparency.toDouble)
      case _ => throw _3DNameError(value)
    }
    def assignTransparency(transparency: Double) = {
      if (transparency == -1 || (transparency >= 0 && transparency <= 1))
        _3DTransparency = transparency
      else error("_3D object's 'transparency' parameter should either be a float number between 0 and 1 or -1")
    }
  }

  def extractCoordinates(value: Value[_]) {
    value match {
      case VLit(GStr(coordinates)) => _3DCoordinates = coordinates.toLowerCase
      case _ => throw _3DNameError(value)
    }
  }

  def extractText(value: Value[_]) {
    value match {
      case VLit(GStr(s)) => _3DText = s
      case VLit(GInt(i)) => _3DText = i.toString
      case VLit(GDouble(i)) => _3DText = i.toString
      case _ => throw _3DNameError(value)
    }
  }

  /* _3D size should be either a vector or an number */
  def extractSize(value: Value[_]) {
    value match {
      case VLit(GPattern(ls)) => extractSize(VVector(ls map VLit))
      case VVector(vs) =>
        if (isVectorOfNumbers(vs)) _3DSize = extractDoubles(vs).toArray
        else {
          _3DSize = Array[Double]()
          throw _3DSizeError(value)
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
      case "Box" =>
        if (_3DSize.length == 2) _3DSize = _3DSize :+ 0.001
        else if (_3DSize.length != 3) throw _3DBoxSizeError()
      /* 3D text's size should be a number */
      case _ => if (_3DSize.length != 1) throw _3DTextSizeError()
    }
  }

  def init3DClassStore(id: CId, _3DData: _3DStore, frameNumber: Int) = {

    val temp: _3DClass = mutable.Map[(CId, Int), List[_]]()
    _3DData += frameNumber -> temp
  }

  def addValuesTo3DClass(objectKey: (CId, Int), value: List[_],
                         _3DData: _3DStore, frameNumber: Int) = {

    if (_3DData(frameNumber).contains(objectKey))
      _3DData(frameNumber)(objectKey) = value
    else
      _3DData(frameNumber) += objectKey -> value
  }

  /* Add new information of each 3D-object to _3DStore */
  def addTo3DStore(id: CId, _3DData: _3DStore, value: List[Value[_]],
                   objectCount: Int, frameNumber: Int) {
    if (!_3DData.contains(frameNumber))
      init3DClassStore(id, _3DData, frameNumber)

    for (i <- 0 until objectCount) {
      /* objectKey is used for specifying the 3D object */
      val objectKey = (id, i)
      val vector = value(i)
      vector match {
        case VVector(l) =>
          if (l.size != 7 && l.size != 8)
            throw _3DError(vector)
          else {
            extractType(l.head)
            extractSize(l(2))
            extractVector(l(1), "position")
            extractVector(l(3), "color")
            extractVector(l(4), "angle")
            if (_3DType == "Text")
              extractText(l(5))
            else if (_3DType == "OBJ")
              extractPath(l(5))
            if (l.size == 8) {
              extractTransparency(l(7))
              extractCoordinates(l(6))
            } else {
              extractTransparency(l(6))
              extractCoordinates(l(5))
            }
          }
        case _ => throw ShouldNeverHappen()
      }
      val valueList =
        if (_3DType == "Text")
          List(_3DType, _3DPosition, _3DSize, _3DColor, _3DAngle, _3DText, _3DCoordinates, _3DTransparency)
        else if (_3DType == "OBJ")
          List(_3DType, _3DPosition, _3DSize, _3DColor, _3DAngle, _3DPath, _3DCoordinates, _3DTransparency)
        else
          List(_3DType, _3DPosition, _3DSize, _3DColor, _3DAngle, _3DCoordinates, _3DTransparency)

      addValuesTo3DClass(objectKey, valueList, _3DData, frameNumber)

      _3DTexture = ""
    }
  }

  /* Look for endTime in "Main" class */
  def lookUpEndTime(id: CId, o: GObject) {
    if (id.equals(new CId(List(0))))
      for ((name, value) <- o)
        if (name.x == "endTime")
          this.endTime = extractDouble(value).toFloat
  }

  /* Look for 3D camera's position and orientation in all the classes */
  def lookUpViewInfo(id: CId, o: GObject) {
    for ((name, value) <- o)
      if (name.x == "_3DView")
        value match {
          case VVector(l) =>
            if (l.nonEmpty) {
              _3DView += new Tuple2(extractDoubles(l.head).toArray,
                extractDoubles(l(1)).toArray)
              if (_3DViewID != id && _3DViewID != null)
                throw _3DViewDuplicateError(id, _3DViewID)
              _3DViewID = id
            }
          case VLit(_) => // initialization is empty
            if (_3DViewID != id && _3DViewID != null)
              throw _3DViewDuplicateError(id, _3DViewID)
            _3DViewID = id
          case _ => throw _3DError(value)
        }
  }

  /* Add _3D information of every class to _3DStore */
  def get3DData(s: GStore):Unit = {
    for ((id, o) <- s) {
      lookUpEndTime(id, o)
      lookUpViewInfo(id, o)
      /* Look for variable named _3D */
      for ((name, value) <- o) {
        if (name.x == "_3D") {
          value match {
            case VVector(l) =>
              if (l.isEmpty) {} //
              else
                l.head match {
                  /* If only one object, _3D will start with a string or an int,
								   *  example:  _3D = ("Sphere",...,...,..),
							     *    		  	_3D = (2,...,...,..)
							     */
                  case VLit(_) =>
                    addTo3DStore(id, _3DData, List(value), 1, frameNumber)
                  /*If it contains multiple objects, _3D will start with a vector,
                   *  example: _3D = (" Sphere ", ( ), ( )...,
                   *                  "Sphere",(),()...)..)
                   */
                  case VVector(some) =>
                    addTo3DStore(id, _3DData, l, l.size, frameNumber)
                  case _ => throw _3DError(value)
                }
            case _ => throw _3DError(value)
          }
        }
      }
    }
    if (!_3DData.contains(frameNumber))
      _3DData += frameNumber -> null
    if (_3DData(frameNumber) != null && !App.ui.views.threeDViewSelected)
      App.ui.views.selectThreeDView()
    frameNumber += 1
  }

  /* Check if model source contains any _3D variable declarations. */
  def modelContains3D():Boolean = {
    var _3DIsEmpty = false
    if (_3DData.nonEmpty) {
      for ((frameNum, objects) <- _3DData)
        if (objects != null)
          _3DIsEmpty = true
    } else _3DIsEmpty = false
    _3DIsEmpty
  }
}
