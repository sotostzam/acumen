package acumen.ui.threeD

import java.awt.event._
import java.awt.{Color, Component, Graphics}
import java.io._
import javax.swing.{SwingUtilities, JPanel}

import acumen.CId
import acumen.Errors._
import acumen.ui.Files
import com.threed.jpct._

import scala.actors._
import scala.math._
import scala.swing.Publisher
import scala.collection.mutable

/* 3D visualization panel */
class ThreeDView extends JPanel {

  Config.maxPolysVisible = 100000
  Config.useRotationPivotFrom3DS = true
  Config.useMultipleThreads = true
  Config.maxNumberOfCores = java.lang.Runtime.getRuntime.availableProcessors()
  Logger.setLogLevel(Logger.ERROR)

  val world = new World  // create a new world
  private var camera = new Camera

  val characters = new Characters

  // Add texture for the axis
  val coAxes = new coAxis (characters.allCharacters)
  val axes = coAxes.cylinders
  val axisArray = Array(new Object3D(1))
  val mainbox = drawBox(1, 1, 1)

  protected[threeD] var objects = mutable.Map[(CId, Int), Object3D]()
  protected[threeD] var scaleFactors = mutable.Map[Object3D, Array[Double]]()
  protected[threeD] var objectsToDelete = mutable.ArrayBuffer[Object3D]()

  val defaultCamPos = new SimpleVector(3, -3, 10)
  private val lookAtPoint = new SimpleVector(0,0,0) // in jPCT coordinate system
  var customView = true // enable for allowing the user move the camera by themselves
  var preCustomView = customView // to enable custom view when we pause
  var enableAnaglyph = false     // to enable anaglyph functions
  var enableRealTime = false     // to enable real time 3D rendering

  private var newMouseX = 1     // mouse position x before dragging
  private var newMouseY = 1     // mouse position y before dragging
  private var lastMouseX = 1    // mouse position x after dragging
  private var lastMouseY = 1    // mouse position y after dragging
  private var dragging = false  // flag for checking the mouse action
  private var cameraLeftDirection = -1  // to make sure the camera rotate forward or backward
  private var cameraRightDirection = 1  // to make sure the camera rotate forward or backward

  val lookAtCenter = Primitives.getSphere(20, 0.1f)

  letThereBeLight()  // create light sources for the scene

  addComponentListener(new ComponentAdapter {
    override def componentResized(e: ComponentEvent) = {
      val c = e.getSource.asInstanceOf[Component]
      initBuffer(c.getWidth, c.getHeight)
      repaint()
    }
  })

  addMouseListener(new MouseAdapter {
    override def mousePressed(e: MouseEvent) = {
      if (!dragging) {
        lastMouseX = e.getX
        lastMouseY = e.getY
        new setGlass(Color.RED, lookAtCenter, -1)
        CustomObject3D.partialBuild(lookAtCenter, false)
        lookAtCenter.translate(lookAtPoint.calcSub
          (lookAtCenter.getTransformedCenter))
        viewStateMachine("addLookAtSphere")
      }
      dragging = true
    }
    override def mouseReleased(e: MouseEvent) = {
      dragging = false
      viewStateMachine("deleteLookAtSphere")
    }
  })

  addMouseWheelListener(new MouseAdapter {
    override def mouseWheelMoved(e: MouseWheelEvent) = {
      // Returns the number of notches the mouse wheel was rotated.
      // If the mouse wheel rotated towards the user (down) the value is positive.
      // If the mouse wheel rotated away from the user (up) the value is negative.
      val zoomSpeed = e.getWheelRotation
      if (zoomSpeed >= 0) zoomout() else zoomin()
      repaint()
    }
  })

  addMouseMotionListener(new MouseAdapter {
    override def mouseDragged(e: MouseEvent) = {
      if (dragging && customView) {
        newMouseX = e.getX
        newMouseY = e.getY
        // to decrease the burden of calculations
        if (abs(newMouseX - lastMouseX) > 0.1
         || abs(newMouseY - lastMouseY) > 0.1) {
          // left button dragging, move camera
          if (SwingUtilities isLeftMouseButton e)
            cameraLeftDirection = moveCamera(cameraLeftDirection, 1, lookAtPoint)
          // right button dragging, move look at point
          else if (SwingUtilities isRightMouseButton e)
            cameraRightDirection = moveCamera(cameraRightDirection, -1,
                                              camera.getPosition)
        }
      }
    }
  })

  /* draggingDirection is the direction for left or right button,
    * click is -1 (right) or 1 (left) */
  def moveCamera (draggingDirection: Int, click: Int,
                  sphereCenter: SimpleVector): Int = {
    var cameraDirection = draggingDirection
    val cameraInitPos = camera.getPosition
    val deltaTheta = cameraDirection * (newMouseY - lastMouseY) * Pi / 500
    val deltaAlpha = (lastMouseX - newMouseX) * Pi / 750
    // translate jPCT coordinate to sphere coordinate
    val initX = click * (-cameraInitPos.x + lookAtPoint.x)
    val initY = click * (-cameraInitPos.z + lookAtPoint.z)
    val initZ = click * (-cameraInitPos.y + lookAtPoint.y)
    val radius = if (initX == 0 && initY == 0 && initZ == 0) 0.01
    else sqrt(initX * initX + initY * initY + initZ * initZ)
    // get initial theta and alpha
    val initialAlpha = if (initX != 0) atan2(initY, initX)
                       else if (initY > 0) Pi / 2
                       else -Pi / 2
    val initialTheta = acos(initZ / radius)
    var alpha = initialAlpha + deltaAlpha
    var theta = initialTheta + deltaTheta
    alpha = normalizeAngle(alpha)
    if (initialTheta > 0 && theta < 0) {
      theta = -theta
      alpha = alpha + Pi
      cameraDirection = -1 * cameraDirection
    } else if (initialTheta < Pi && theta > Pi) {
      theta = 2 * Pi - theta
      alpha = alpha + Pi
      cameraDirection = -1 * cameraDirection
    }
    val newX = radius * sin(theta) * cos(alpha) - sphereCenter.x.toDouble
    val newY = radius * sin(theta) * sin(alpha) - sphereCenter.z.toDouble
    val newZ = radius * cos(theta) - sphereCenter.y.toDouble
    if (click == 1)    // left button dragging
      camera.setPosition(-newX.toFloat, -newZ.toFloat, -newY.toFloat)
    else     // right button dragging
      lookAtPoint.set(-newX.toFloat, -newZ.toFloat, -newY.toFloat)
    lookAt(null, lookAtPoint)
    lastMouseX = newMouseX
    lastMouseY = newMouseY
    lookAtCenter.translate(lookAtPoint.calcSub(lookAtCenter.getTransformedCenter))
    repaint()
    cameraDirection
  }

  def normalizeAngle (angle: Double): Double = {
    var newAngle = angle
    while (newAngle <= -Pi) newAngle += 2 * Pi
    while (newAngle > Pi) newAngle -= 2 * Pi
    newAngle
  }

  def axisOn() = {
    viewStateMachine("addAxes")
  }


  def axisOff() = {
    viewStateMachine("deleteAxes")
  }


  // create a new buffer to draw on:
  private var buffer: FrameBuffer = null

  def initBuffer(bufferWidth: Int, bufferHeight: Int) = {
    buffer = new FrameBuffer(bufferWidth, bufferHeight,
                             FrameBuffer.SAMPLINGMODE_OGSS)
  }

  def init() = {
    // add the main box
    mainbox.setShadingMode(Object3D.SHADING_FAKED_FLAT)
    new setGlass(new Color(180, 180, 180), mainbox, 0)
    camera = world.getCamera  // grab a handle to the camera
    defaultView()
    lookAt(mainbox, null) // camera faces towards the object
    lookAtPoint.set(0,0,0)
    CustomObject3D.partialBuild(mainbox, false)
    viewStateMachine("addMainBox")
  }

  // the state machine for adding or deleting objects in view state machine
  /* There are seven states in total
  *  which are "deleteAllObject" -> delete all the objects
  *            "renderCurrentObjects" -> add and delete objects according to
  *                                      current frame
  *            "deleteAxes" -> delete the axes
  *            "deleteLookAtSphere" -> delete the red sphere at look at point
  *            "addMainBox" -> add the Main Box
  *            "addAxes" -> add the axes
  *            "addLookAtSphere" -> add the red sphere at look at point*/

  def viewStateMachine(worldState: String) = {
    this.synchronized {
      // object deleting state machine
      worldState match {
        case "renderCurrentObjects" => // only called in renderCurrentFrame()
          if (world.getObjectByName(mainbox.getName) != null)
            world.removeObject(mainbox)
          for (oldObject <- objectsToDelete) {
            if (world.getObjectByName(oldObject.getName) != null)
              world.removeObject(oldObject)
          }
          // add all the objects in current frame into the view
          for ((objectKey, objectToBuild) <- objects) {
            if (world.getObjectByName(objectToBuild.getName) == null) {
              world.addObject(objectToBuild)
            }
          }
        case "addMainBox" =>
          if (world.getObjectByName(mainbox.getName) == null)
            world.addObject(mainbox)
        case "addAxes" => // only called in axisOff function
          if (!axisArray.contains(axes(0))) {
            axisArray(0) = axes(0)
            for (i <- 0 until axes.length)
              CustomObject3D.partialBuild(axes(i), i < 6)
            world.addObjects(axes)
          }
        case "addLookAtSphere" => // called when camera rotation is finished
          if (world.getObjectByName(lookAtCenter.getName) == null)
            world.addObject(lookAtCenter)
        case "deleteAllObjects" =>
          world.removeAllObjects()
        case "deleteAxes" => // only called in axisOff function
          if (axisArray.contains(axes(0))) {
            for (i <- 0 until axes.length)
              world.removeObject(axes(i))
            axisArray(0) = null
          }
        case "deleteLookAtSphere" => // called when camera rotation is finished
          if (world.getObjectByName(lookAtCenter.getName) != null)
            world.removeObject(lookAtCenter)
        case _ => throw ShouldNeverHappen()
      }
    }
    repaint()
  }

  override def paint(g: Graphics) = {
    world.synchronized {
      buffer.clear(Color.LIGHT_GRAY) // erase the previous frame
      // render the world onto the buffer:
      world.renderScene(buffer)
      world.draw(buffer)
      buffer.update()
    }
    buffer.display(g)
  }

  // point the camera toward the given object
  def lookAt(obj: Object3D, point: SimpleVector) =
    if (obj != null)
      camera.lookAt(obj.getTransformedCenter)  // look toward the object
    else
      camera.lookAt(point)

  // create some light sources for the scene
  def letThereBeLight() = {
    // Set the overall brightness of the world:
    world.setAmbientLight(-200, -200, -200)
    // Create main light sources:
    world.addLight(new SimpleVector(15.076f, -7.904f, 0f), 12, 12, 12)
    world.addLight(new SimpleVector(-15.076f, -7.904f, 0f), 12, 12, 12)
    world.addLight(new SimpleVector(0, -8f, 0), 8, 8, 8)
  }

  def defaultView() = {
    camera.setPosition(defaultCamPos)
    camera.setFOVLimits(0.01f, 3.0f)
    camera.setFOV(0.65f)
  }

  def reset() = {
    viewStateMachine("deleteAllObjects")
    objects.clear()
    scaleFactors.clear()
    objectsToDelete.clear()
    axisArray(0) = null
    init()
  }

  def zoomin()  = camera.increaseFOV(0.1f)
  def zoomout() = camera.decreaseFOV(0.1f)

  def transformView(position: Array[Double], rotation: Array[Double]) = {
    val cameraToSet = world.getCamera
    cameraToSet.setPosition(-position(0).toFloat, -position(2).toFloat,
                            -position(1).toFloat)
    rotateObject(null, rotation, "Camera", cameraToSet)
  }

  def drawBox(length: Double, width: Double, height: Double): Object3D = {
    val box = new Object3D(12)

    val upperLeftFront  = new SimpleVector(-width/2, -height/2, -length/2)
    val upperRightFront = new SimpleVector( width/2, -height/2, -length/2)
    val lowerLeftFront  = new SimpleVector(-width/2,  height/2, -length/2)
    val lowerRightFront = new SimpleVector( width/2,  height/2, -length/2)

    val upperLeftBack   = new SimpleVector(-width/2, -height/2,  length/2)
    val upperRightBack  = new SimpleVector( width/2, -height/2,  length/2)
    val lowerLeftBack   = new SimpleVector(-width/2,  height/2,  length/2)
    val lowerRightBack  = new SimpleVector( width/2,  height/2,  length/2)

    // Front
    box.addTriangle(upperLeftFront,0,0, lowerLeftFront,0,1, upperRightFront,1,0)
    box.addTriangle(upperRightFront,1,0, lowerLeftFront,0,1, lowerRightFront,1,1)

    // Back
    box.addTriangle(upperLeftBack,0,0, upperRightBack,1,0, lowerLeftBack,0,1)
    box.addTriangle(upperRightBack,1,0, lowerRightBack,1,1, lowerLeftBack,0,1)

    // Upper
    box.addTriangle(upperLeftBack,0,0, upperLeftFront,0,1, upperRightBack,1,0)
    box.addTriangle(upperRightBack,1,0, upperLeftFront,0,1, upperRightFront,1,1)

    // Lower
    box.addTriangle(lowerLeftBack,0,0, lowerRightBack,1,0, lowerLeftFront,0,1)
    box.addTriangle(lowerRightBack,1,0, lowerRightFront,1,1, lowerLeftFront,0,1)

    // Left
    box.addTriangle(upperLeftFront,0,0, upperLeftBack,1,0, lowerLeftFront,0,1)
    box.addTriangle(upperLeftBack,1,0, lowerLeftBack,1,1, lowerLeftFront,0,1)

    // Right
    box.addTriangle(upperRightFront,0,0, lowerRightFront,0,1, upperRightBack,1,0)
    box.addTriangle(upperRightBack,1,0, lowerRightFront, 0,1, lowerRightBack,1,1)

    box
  }

  // rotate object or camera
  def rotateObject(rotateObject: Object3D, angle: Array[Double], objectType: String, rotateCamera: Camera) = {
    // Once we added the object, we should also move the object to the position at that time
    val tranObjectRotMatrixX = new Matrix()
    val tranObjectRotMatrixY = new Matrix()
    val tranObjectRotMatrixZ = new Matrix()
    val tranObjectRotTempMat = new Matrix()
    val tranObjectRotMatrix  = new Matrix()
    // to make the coordinate as same as Java3D
    if (objectType == "Cylinder" || objectType == "Cone")
      tranObjectRotMatrix.rotateX((-Pi / 2).toFloat)
    else if (objectType == "OBJ") {
      tranObjectRotMatrix.rotateX((-Pi / 2).toFloat)
      tranObjectRotMatrix.rotateY(Pi.toFloat)
    }
    tranObjectRotMatrixZ.rotateZ(-angle(1).toFloat)
    tranObjectRotMatrixY.rotateY(-angle(2).toFloat)
    tranObjectRotMatrixX.rotateX(angle(0).toFloat)
    tranObjectRotTempMat.matMul(tranObjectRotMatrixX)
    tranObjectRotTempMat.matMul(tranObjectRotMatrixZ)
    tranObjectRotTempMat.matMul(tranObjectRotMatrixY)
    tranObjectRotMatrix.matMul(tranObjectRotTempMat)
    if (objectType != "Camera")
      rotateObject.setRotationMatrix(tranObjectRotMatrix)
    else {
      // angle is the position of look at point in this case
      lookAtPoint.set(-angle(0).toFloat, -angle(2).toFloat, -angle(1).toFloat)
      lookAt(null, lookAtPoint)
    }
  }
}

/* Timer for 3D-visualization, sends message to 3D renderer to coordinate animation */
class ScalaTimer(receiver: _3DDisplay, endTime: Double,
                 playSpeed: Double) extends Publisher with Actor {
  var pause = true
  var destroy = false
  var sleepTime = 0.0
  var extraTime = 0.0
  var initSpeed = 0.0

  if (receiver.totalFrames > 0)
    sleepTime = endTime * 1000 / receiver.totalFrames

  initSpeed = sleepTime
  sleepTime /= playSpeed
  extraTime = ((sleepTime - sleepTime.toLong) * 1000000).toInt // To nano sec

  def act() {
    loopWhile(!destroy) {
      if (destroy)
        exit()
      if (pause)
      /* Tell the receiver to show the next frame */
        receiver ! "go"
      /* Millisecond and Nanosecond */
      Thread.sleep(sleepTime.toLong, extraTime.toInt)
    }
  }
}

/* 3D Render */
class _3DDisplay(app: ThreeDView, slider: Slider3D,
                 _3DDataBuffer: mutable.Map[Int,mutable.Map[(CId,Int),List[_]]],
                 lastFrame: Int, endTime: Float,
                 _3DView: mutable.ArrayBuffer[(Array[Double], Array[Double])])
                 extends Publisher with Actor {
  /* Default directory where all the OBJ files are */
  private val _3DBasePath = Files._3DDir.getAbsolutePath
  private var currentFrame = 0
  var totalFrames = lastFrame
  var destroy = false
  /* used for recording last frame number */
  private var lastRenderFrame = 0
  private var mouseSleepTime = 10.toLong
  private var lastSetFrameTime = System.currentTimeMillis()
  private var setFrameDone = true

  def stop() = {
    app.viewStateMachine("deleteAllObjects")
    app.objects.clear()
    app.axisArray(0) = null
    app.scaleFactors.clear()
    app.objectsToDelete.clear()
    lastRenderFrame = 0
    mouseSleepTime = 10.toLong
  }

  def checkResizeable(newSize: Array[Double]): Boolean =
    newSize.forall(d => !(d.isNaN || d.isInfinite))

  def setScaleFactors(size: Array[Double], o: Object3D, objectType: String,
                      scaleFactors: mutable.Map[Object3D, Array[Double]]) = {
    def scaleFactor(si: Int, bi1: Int, bi2: Int) =
      if (size(si) == 0) abs(o.getMesh.getBoundingBox()(bi1)
                           - o.getMesh.getBoundingBox()(bi2)) / 0.001
      else               abs(o.getMesh.getBoundingBox()(bi1)
                           - o.getMesh.getBoundingBox()(bi2)) / size(si)
    // Add the object scale factor into a map
    val factorX =
      scaleFactor(0, 1, 0)
    val factorY =
      if (Seq("Box", "Cylinder", "Cone") contains objectType)
        scaleFactor(1, 3, 2)
      else
        scaleFactor(0, 3, 2)
    val factorZ =
      if (objectType == "Box")
        scaleFactor(2, 5, 4)
      else
        scaleFactor(0, 5, 4)
    scaleFactors += o -> Array(factorX, factorY, factorZ)
  }

  // Because of scale function, we never allow the initial size to be 0
  def checkSize (size: Double): Double = if (size == 0) 0.001 else size

  def calculateResizeFactor (o: Object3D, size: Array[Double], scaleFactors:
                          mutable.Map[Object3D, Array[Double]]): Array[Float] =
    if (scaleFactors.contains(o)) {
      if (o != null) {
        val (xFactor, yFactor, zFactor) =
          (if (size(0) == 0) scaleFactors(o)(0) * 0.001
            / abs(o.getMesh.getBoundingBox()(1) - o.getMesh.getBoundingBox()(0))
           else              scaleFactors(o)(0) * size(0)
            / abs(o.getMesh.getBoundingBox()(1) - o.getMesh.getBoundingBox()(0)),
           if (size(1) == 0) scaleFactors(o)(1) * 0.001
            / abs(o.getMesh.getBoundingBox()(3) - o.getMesh.getBoundingBox()(2))
           else              scaleFactors(o)(1) * size(1)
            / abs(o.getMesh.getBoundingBox()(3) - o.getMesh.getBoundingBox()(2)),
           if (size(2) == 0) scaleFactors(o)(2) * 0.001
            / abs(o.getMesh.getBoundingBox()(5) - o.getMesh.getBoundingBox()(4))
           else              scaleFactors(o)(2) * size(2)
            / abs(o.getMesh.getBoundingBox()(5) - o.getMesh.getBoundingBox()(4)))
        Array(xFactor.toFloat, yFactor.toFloat, zFactor.toFloat)
      } else Array(0.001f,0.001f,0.001f)
    } else Array(0.001f,0.001f,0.001f)

  /** Uses a vertex controller to rescale  **/
  def setReSize(scaleX: Float, scaleY: Float, scaleZ: Float,
                o: Object3D) = {
    try {
      val planeMesh = o.getMesh
      val resizer = new Resizer(scaleX,scaleY,scaleZ)
      planeMesh.setVertexController(resizer, IVertexController.PRESERVE_SOURCE_MESH)
      planeMesh.applyVertexController()
      planeMesh.removeVertexController()
    } catch {
      case e: java.lang.NullPointerException =>
        println("Och!!!")
    }
  }

  def getLastType(objectKey: (CId, Int), valueList: List[_]): Any = {
    if (_3DDataBuffer.contains(lastRenderFrame)
      && _3DDataBuffer(lastRenderFrame) != null) {
      if (_3DDataBuffer(lastRenderFrame).contains(objectKey)) {
        valueList(0)
      } else false // can not find the object
    } else false
  }

  def getLastContent(objectKey: (CId, Int), valueList: List[_]): Any = {
    if (_3DDataBuffer.contains(lastRenderFrame)
      && _3DDataBuffer(lastRenderFrame) != null) {
      if (_3DDataBuffer(lastRenderFrame).contains(objectKey)) {
        valueList(5)
      } else false   // can not find the object
    } else false
  }

  def needToResize(objectKey: (CId, Int), currentValList: List[_],
                   lastValList: List[_]): Boolean = {
    var resizeResult = false
    if (_3DDataBuffer.contains(lastRenderFrame)
      && _3DDataBuffer(lastRenderFrame) != null) {
      val (lastSize : Array[Double], curSize: Array[Double]) =
        (lastValList(2), currentValList(2))
      for (i <- 0 until lastSize.length)
        if (lastSize(i) != curSize(i))
          resizeResult = true
    } else resizeResult = true
    resizeResult
  }

  def renderCurrentFrame() = this.synchronized {
    app.objects.synchronized {
      app.objectsToDelete.clear()
      // 3d objects within the current frame
      if (_3DDataBuffer.contains(currentFrame)
        && _3DDataBuffer(currentFrame) != null) {
        for ((objectKey, valueList) <- _3DDataBuffer(currentFrame))
          if (!app.objects.contains(objectKey))
            matchingObject(objectKey, valueList, currentFrame)
          else if (app.objects.contains(objectKey)  // this should not happen
            && app.world.getObjectByName(app.objects(objectKey).getName) == null) {
            app.objects -= objectKey
            matchingObject(objectKey, valueList, currentFrame)
          } else
            transformObject(objectKey, valueList, lastRenderFrame, currentFrame)

        // delete the object not in this frame
        for ((objectKey, o) <- app.objects)
          if (!_3DDataBuffer(currentFrame).contains(objectKey))
            deleteObj(objectKey)

        lastRenderFrame = currentFrame
        if(currentFrame < _3DView.size)
          app.transformView(_3DView(currentFrame)._1, _3DView(currentFrame)._2)
        app.viewStateMachine("renderCurrentObjects")
      }
    }
  }

  /**
   * Moving and rotating the object
   */
  def transformObject(objectKey: (CId, Int), valueList: List[_],
                      lastFrame: Int, currentFrame: Int) {
    var objID = app.objects(objectKey).getID  // get the object ID
    /* Get the 3D information of the object at that frame	*/
    val (name: String, position: Array[Double], size: Array[Double],
    color: Array[Double], angle: Array[Double]) =
      (valueList(0), valueList(1), valueList(2), valueList(3), valueList(4))
    val (text: String, path: String) =
      (if (name == "Text") valueList(5) else " ",
       if (name == "OBJ")  valueList(5) else " ")

    // get the object need to transform
    var transObject: Object3D = app.world.getObject(objID)

    val lastValueList: List[_] =
      if (_3DDataBuffer.contains(lastFrame) && _3DDataBuffer(lastFrame) != null
        && _3DDataBuffer(lastFrame).contains(objectKey))
        _3DDataBuffer(lastFrame)(objectKey)
      else null

    val lastFrameName =
      if (getLastType(objectKey, lastValueList) == false) "New Object"
      else getLastType(objectKey, lastValueList)

    val needResize = if (lastFrameName != name) false
    else needToResize(objectKey, valueList, lastValueList)
    // reset the type and size for the object, matching the type of object first
    name match {
      case "Box" =>
        // the type has been changed, delete the old object and create a new one
        if (lastFrameName != name) {
          // change the object in
          // Since some object need to scale, we never allow the initial size become 0
          val (sizeToSetX, sizeToSetY, sizeToSetZ) = (checkSize(size(1)),
                                                      checkSize(size(0)),
                                                      checkSize(size(2)))
          app.objectsToDelete += app.world.getObject(objID)
          app.scaleFactors -= app.objects(objectKey)
          app.objects(objectKey) = app.drawBox(abs(sizeToSetX), abs(sizeToSetY),
                                               abs(sizeToSetZ))
          transObject = app.objects(objectKey)
          setScaleFactors(Array(sizeToSetY,sizeToSetZ,sizeToSetX), transObject,
                          name, app.scaleFactors)
          objID = app.objects(objectKey).getID // refresh the object ID
          transObject.setShadingMode(Object3D.SHADING_FAKED_FLAT)
        } else if (checkResizeable(size) && needResize) {
          // just need to change the size
          if (app.objects.contains(objectKey) && transObject != null) {
            val (sizeToSetX, sizeToSetY, sizeToSetZ) = (checkSize(size(1)),
                                                        checkSize(size(2)),
                                                        checkSize(size(0)))
            val factors = calculateResizeFactor(transObject, Array(sizeToSetZ,
                                      sizeToSetY, sizeToSetX), app.scaleFactors)
            setReSize(factors(0), factors(1), factors(2), transObject)
          }
        }
      case "Cylinder" =>
        // the type has been changed, delete the old object and create a new one
        if (lastFrameName != name) {
          // change the object in
          val (sizeToSetR, sizeToSetS) = (checkSize(size(0)), checkSize(size(1)))
          app.objectsToDelete += app.world.getObject(objID)
          app.scaleFactors -= app.objects(objectKey)
          app.objects(objectKey) = Primitives.getCylinder(20, abs(sizeToSetR.toFloat),
                                    abs(sizeToSetS / (2 * sizeToSetR)).toFloat)
          transObject = app.objects(objectKey)
          setScaleFactors(size, transObject, name, app.scaleFactors)
          objID = app.objects(objectKey).getID // refresh the object ID
          transObject.setShadingMode(Object3D.SHADING_FAKED_FLAT)
        } else if (checkResizeable(size) && needResize) {
          if (app.objects.contains(objectKey) && transObject != null) {
            // just need to change the size
            val (sizeToSetR, sizeToSetS) = (checkSize(size(0)), checkSize(size(1)))
            val factors = calculateResizeFactor(transObject, Array(sizeToSetR,
                                      sizeToSetS, sizeToSetR), app.scaleFactors)
            setReSize(factors(0), factors(1), factors(2), transObject)
          }
        }
      case "Cone" =>
        // the type has been changed, delete the old object and create a new one
        if (lastFrameName != name) {
          // change the object in
          val (sizeToSetR, sizeToSetS) = (checkSize(size(0)), checkSize(size(1)))
          app.objectsToDelete += app.world.getObject(objID)
          app.scaleFactors -= app.objects(objectKey)
          app.objects(objectKey) = Primitives.getCone(20, abs(sizeToSetR.toFloat),
            abs(sizeToSetS / (sizeToSetR * 2)).toFloat)
          transObject = app.objects(objectKey)
          setScaleFactors(size, transObject, name, app.scaleFactors)
          objID = app.objects(objectKey).getID // refresh the object ID
        } else if (checkResizeable(size) && needResize) {
          if (app.objects.contains(objectKey) && transObject != null) {
            // just need to change the size
            val (sizeToSetR, sizeToSetS) = (checkSize(size(0)), checkSize(size(1)))
            val factors = calculateResizeFactor(transObject, Array(sizeToSetR,
                                      sizeToSetS, sizeToSetR), app.scaleFactors)
            setReSize(factors(0), factors(1), factors(2), transObject)
          }
        }
      case "Sphere" =>
        // the type has been changed, delete the old object and create a new one
        if (lastFrameName != name) {
          // change the object in
          val sizeToSetR = checkSize(size(0))
          app.objectsToDelete += app.world.getObject(objID)
          app.scaleFactors -= app.objects(objectKey)
          app.objects(objectKey) = Primitives.getSphere(10, abs(sizeToSetR.toFloat))
          transObject = app.objects(objectKey)
          setScaleFactors(size, transObject, name, app.scaleFactors)
          objID = app.objects(objectKey).getID // refresh the object ID
        } else if (checkResizeable(size) && needResize) {
          if (app.objects.contains(objectKey) && transObject != null) {
            // just need to change the size
            val sizeToSetR = checkSize(size(0))
            val factors = calculateResizeFactor(transObject, Array(sizeToSetR,
                                      sizeToSetR, sizeToSetR), app.scaleFactors)
            setReSize(factors(0), factors(1), factors(2), transObject)
          }
        }
      case "Text" =>
        val lastFrameContent =
          if (getLastContent(objectKey, lastValueList) == false) "new" + name
          else getLastContent(objectKey, lastValueList)
        // the type has been changed, delete the old object and create a new one
        if ((lastFrameName != name || lastFrameContent != text) && text != "") {
          // change the object in
          val sizeToSetR = checkSize(size(0))
          app.objectsToDelete += app.world.getObject(objID)
          app.scaleFactors -= app.objects(objectKey)
          app.objects(objectKey) = buildText(text, sizeToSetR)
          transObject = app.objects(objectKey)
          setScaleFactors(size, transObject, name, app.scaleFactors)
          objID = app.objects(objectKey).getID // refresh the object ID
        } else if (checkResizeable(size) && needResize) {
          if (app.objects.contains(objectKey) && transObject != null) {
            // just need to change the size
            val sizeToSetR = checkSize(size(0))
            val factors = calculateResizeFactor(transObject, Array(sizeToSetR,
                                      sizeToSetR, sizeToSetR), app.scaleFactors)
            setReSize(factors(0), factors(1), factors(2), transObject)
          }
        }
      case "OBJ" =>
        val lastFrameContent =
          if (getLastContent(objectKey, lastValueList) == false) "new" + name
          else getLastContent(objectKey, lastValueList)
        // the type has been changed, we need to delete the old object and create a one
        if ((lastFrameName != name || lastFrameContent != path) && path != "") {
          // change the object in
          val sizeToSetR = checkSize(size(0) / 132)
          app.objectsToDelete += app.world.getObject(objID)
          app.scaleFactors -= app.objects(objectKey)
          app.objects(objectKey) = loadObj(path, sizeToSetR)
          transObject = app.objects(objectKey)
          setScaleFactors(size, transObject, name, app.scaleFactors)
          objID = app.objects(objectKey).getID // refresh the object ID
        } else if (checkResizeable(size) && needResize) {
          if (app.objects.contains(objectKey) && transObject != null) {
            // just need to change the size
            val sizeToSetR = checkSize(size(0) / 132)
            val factors = calculateResizeFactor(transObject, Array(sizeToSetR,
                                      sizeToSetR, sizeToSetR), app.scaleFactors)
            setReSize(factors(0), factors(1), factors(2), transObject)
          }
        }
      case _ => throw ShouldNeverHappen()
    }

    if (transObject != null) {
      // reset the color for the object
      setColor(transObject, color)
      // rotate the object
      if (checkResizeable(angle))
        app.rotateObject(transObject, angle, name, null)
      // calculate the transVector for the object and translate it
      val tempTransVector = new SimpleVector(-position(0), -position(2),
                                             -position(1))
      val transVector = tempTransVector.calcSub(transObject.getTransformedCenter)
      transObject.translate(transVector)
      CustomObject3D.partialBuild(transObject, name == "Text")
    }
  }

  def matchingObject(c: (CId, Int), valueList: List[_], currentFrame: Int) = {
    /* Find the corresponding index of the object */
    /* Get the 3D information of the object at that frame	*/
    val (name: String, position: Array[Double], size: Array[Double],
    color: Array[Double], angle: Array[Double]) =
      (valueList(0), valueList(1), valueList(2), valueList(3), valueList(4))
    val (text: String, path: String) =
      (if (name == "Text") valueList(5) else " ",
        if (name == "OBJ")  valueList(5) else " ")

    val newObject = name match {
      case "Box" =>
        val (sizeToSetX, sizeToSetY, sizeToSetZ) = (checkSize(size(1)),
          checkSize(size(0)),
          checkSize(size(2)))
        app.drawBox(abs(sizeToSetX), abs(sizeToSetY), abs(sizeToSetZ))
      case "Cylinder" =>
        val (sizeToSetR, sizeToSetS) = (checkSize(size(0)), checkSize(size(1)))
        Primitives.getCylinder(20, abs(sizeToSetR).toFloat,
          abs(sizeToSetS / (sizeToSetR * 2)).toFloat)
      case "Cone" =>
        val (sizeToSetR, sizeToSetS) = (checkSize(size(0)), checkSize(size(1)))
        Primitives.getCone(20, abs(sizeToSetR.toFloat),
          abs(sizeToSetS / (sizeToSetR * 2)).toFloat)
      case "Sphere" =>
        val sizeToSetR = checkSize(size(0))
        Primitives.getSphere(20, abs(sizeToSetR.toFloat))
      case "Text" =>
        val sizeToSetR = checkSize(size(0))
        if (text != "")  // model err, do nothing
          buildText(text, sizeToSetR)
        else
          null
      case "OBJ" =>
        val sizeToSetR = checkSize(size(0) / 132)
        if (path != "")  // model err, do nothing
          loadObj(path, sizeToSetR)
        else
          null
      case _ => throw ShouldNeverHappen()
    }

    if (newObject != null) {
      // set color to the object
      setColor(newObject, color)
      if (name == "Box" || name == "Cylinder")
        newObject.setShadingMode(Object3D.SHADING_FAKED_FLAT)
      // rotate the object
      app.rotateObject(newObject, angle, name, null)

      // calculate the transVector for the object and translate object
      val tempTransVector = new SimpleVector(-position(0), -position(2), -position(1))
      val transVector = tempTransVector.calcSub(newObject.getTransformedCenter)
      newObject.translate(transVector)

      // calculate resize factor for object
      if (name == "Box")
        setScaleFactors(Array(size(0), size(2), size(1)), newObject, name,
          app.scaleFactors)
      else
        setScaleFactors(size, newObject, name, app.scaleFactors)
      app.objects += c -> newObject
      if (name != "Text")
        CustomObject3D.partialBuild(newObject, false)
      else {
        newObject.setRotationPivot(new SimpleVector(0,0,0))
        newObject.setCenter(new SimpleVector(0,0,0))
        CustomObject3D.partialBuild(newObject, true)
      }
    }
  }

  def act() {
    loopWhile(!destroy) {
      if (destroy)
        exit()
      react {
        case "go" =>
          if (currentFrame == totalFrames) {
            // Animation is over
            slider.setProgress3D(100)
            slider.setTime(endTime.toFloat)
            app.customView = true
          }
          if (totalFrames > 0) {
            val percentage = currentFrame * 100 / totalFrames
            slider.setProgress3D(percentage)
            slider.setTime(percentage / 100f * endTime)
          }
          if (currentFrame <= totalFrames) {
            renderCurrentFrame()
            currentFrame = setFrameNumber("go", currentFrame)
          }
        case "set frame" =>
          setFrameDone = false
          currentFrame = setFrameNumber("set frame", currentFrame)
          if (totalFrames > 0 && currentFrame <= totalFrames) {
            val startRenderTime = System.currentTimeMillis()
            val percentage = currentFrame * 100 / totalFrames
            slider.setTime(percentage / 100f * endTime)
            renderCurrentFrame()
            val renderTime = System.currentTimeMillis() - startRenderTime
            if (mouseSleepTime < renderTime && renderTime < 100.toLong)
              mouseSleepTime = renderTime
            setFrameDone = true
          }
      }
    }
  }

  def setFrameNumber (setMode: String, lastFrameNumber: Int): Int = {
    val newFrameNumber =
      if (setMode == "go") lastFrameNumber + 1
      else slider.bar.value * totalFrames / 100
    newFrameNumber
  }

  // Reactions to the mouse events
  reactions += {
    case e: scala.swing.event.MouseDragged =>
      val curSystemTime = System.currentTimeMillis()
      if (curSystemTime > lastSetFrameTime + mouseSleepTime
        && setFrameDone)
        receiver ! "set frame"
      lastSetFrameTime = System.currentTimeMillis()
  }

  /**
   * Delete an 3D-object from scene
   */
  def deleteObj(c: (CId, Int)) {
    if (app.objects.contains(c)) {
      if (app.world.getObjectByName(app.objects(c).getName) != null)
        app.objectsToDelete += app.objects(c)
      app.objects -= c
    }
  }

  // build the 3D text by adding .3ds object
  def buildText(text: String, size: Double): Object3D = {
    val allChar = text.toCharArray
    val objectsArray = new scala.collection.mutable.ArrayBuffer[Object3D]
    var firstLetter = true
    var distanceToShift = 0.0
    //  copy the characters in to make change
    for (i <- 0 until allChar.length) {
      if (app.characters.allCharacters.contains(allChar(i)) && !firstLetter) {
        objectsArray += new Object3D(app.characters.allCharacters(allChar(i)), false)
        // calculate the shift distance from last letter, firstly, we need to get the last letter index
        var j = 1 // the count for blank space
        while (!app.characters.allCharacters.contains(allChar(i-j)) && j <= i) {
          j += 1
        }
        // get the width of last letter
        val tempDis = abs(objectsArray.apply(i-j).getMesh.getBoundingBox()(1)
                        - objectsArray.apply(i-j).getMesh.getBoundingBox()(0))
        if (app.characters.letterDistance.contains(allChar(i-j)))
          distanceToShift += tempDis + app.characters.letterDistance(allChar(i-j))
        else
          distanceToShift += tempDis + 0.2
        objectsArray.apply(i).translate(-distanceToShift.toFloat, 0, 0)
        objectsArray.apply(i).translateMesh()
        objectsArray.apply(i).setTranslationMatrix(new Matrix())
      } else if (firstLetter && app.characters.allCharacters.contains(allChar(i))){
        objectsArray += new Object3D(app.characters.allCharacters(allChar(i)), false)
        firstLetter = false
      } else {
        objectsArray += new Object3D(1)
        firstLetter = false
        if (i != 0) {
          var j = 1     // the count for blank space
          while (!app.characters.allCharacters.contains(allChar(i-j)) && j <= i) {
            j += 1
          }
          distanceToShift += 0.2 * j
        }
      }
    }
    val stringObject = Object3D.mergeAll(objectsArray.toArray)
    setReSize(size.toFloat, size.toFloat, size.toFloat, stringObject)
    stringObject
  }

  // Load .obj file
  def loadObj(path: String, size: Double): Object3D = {
    //read in the geometry information from the data file
    val _3DFolder = new File(_3DBasePath + File.separator)
    val listOfFiles = _3DFolder.listFiles()
    val objectFileBase = (_3DBasePath + File.separator + path).split("\\.")(0)
    var objectTexture: Texture = null
    val texturePath = objectFileBase + ".png"
    var MTLPath:String = null
    for (i <- 0 until listOfFiles.length) {
      if (listOfFiles(i).getPath == texturePath
        && !TextureManager.getInstance().containsTexture(objectFileBase + ".mtl"))
        objectTexture = new Texture(texturePath)
      if (listOfFiles(i).getPath == objectFileBase + ".mtl")
        MTLPath = listOfFiles(i).getPath
    }
    val objFileloader = Loader.loadOBJ(_3DBasePath + File.separator
                                      + path, MTLPath, size.toFloat)(0)
    if (objectTexture != null) {
      TextureManager.getInstance().addTexture(MTLPath, objectTexture)
      objFileloader.setTexture(MTLPath)
    } else {
      objFileloader.setTexture(objectFileBase + ".mtl")
    }
    objFileloader
  }

  def setColor(objectToSet: Object3D, colorRGB: Array[Double]) =
    objectToSet.setAdditionalColor(new Color
    ( max(0, min(255, colorRGB(0) * 255)).toInt
    , max(0, min(255, colorRGB(1) * 255)).toInt
    , max(0, min(255, colorRGB(2) * 255)).toInt ))

}

// Transparent box
class setGlass(color: Color, objectA: Object3D, transparancy: Int) {
  objectA.setTransparencyMode(Object3D.TRANSPARENCY_MODE_DEFAULT) //TRANSPARENCY_MODE_DEFAULT
  objectA.setTransparency(transparancy) // the transparency level. 0 is the highest possible transparency
  objectA.setAdditionalColor(color) // the color of the object3D
}

/* Vertex controller classes */

case class Resizer(xFactor: Float, yFactor: Float, zFactor: Float)
                  extends GenericVertexController {
  def apply() {
    val s = getSourceMesh
    val d = getDestinationMesh
    for (i <- 0 until s.length) {
      d(i).x = s(i).x * xFactor
      d(i).z = s(i).z * zFactor
      d(i).y = s(i).y * yFactor
    }
  }
}

// Axis
class coAxis(characters: Map[Char, Object3D]) {
  val cylinders: Array[Object3D] = new Array[Object3D](9)
  for (x <- 0 until 3)
    cylinders(x) = Primitives.getCylinder(12, 0.01f, 50f)
  for (x <- 3 until 6)
    cylinders(x) = Primitives.getCone(12, 0.05f, 2f)
  cylinders(8) = new Object3D(characters('y'), false)
  cylinders(7) = new Object3D(characters('x'), false)
  cylinders(6) = new Object3D(characters('z'), false)
  for (i <- 6 to 8) {
    cylinders(i).setRotationPivot(new SimpleVector(0,0,0))
    cylinders(i).setCenter(new SimpleVector(0,0,0))
    cylinders(i).scale(0.4f)
  }
  for (i <- 0 until cylinders.length)
    new setGlass( if      (i % 3 == 0) Color.BLUE
                  else if (i % 3 == 1) Color.RED
                  else                 Color.GREEN
                , cylinders(i), -1)
  cylinders(0).translate(0f, -0.5f, 0f)       // z axis cylinder
  cylinders(3).translate(0f, -1f, 0f)         // z axis cone
  cylinders(6).translate(-0.05f, -1f, 0f)     // z text
  cylinders(1).rotateZ(0.5f * -Pi.toFloat)    // x axis cylinder
  cylinders(1).translate(-0.5f, 0f, 0f)
  cylinders(4).translate(-1f, 0f, 0f)         // x axis cone
  cylinders(4).rotateZ(0.5f * Pi.toFloat)
  cylinders(7).translate(-1f, -0.05f, 0f)     // x text
  cylinders(2).rotateX(-0.5f * Pi.toFloat)    // y axis cylinder
  cylinders(2).translate(0f, 0f, -0.5f)
  cylinders(5).translate(0f, 0f, -1f)         // y axis cone
  cylinders(5).rotateX(-0.5f * Pi.toFloat)
  cylinders(8).translate(0f, -0.05f, -1f)     // y text
}

class Characters {
  // initialize the Map to define symbol character path (for creating 3D symbol character)
  val symbolPath = {
    val symbols = ",./<>?;:\'\"+-*|!@#$%^&()[]{}=".toCharArray
    val paths = Array("comma","dot","div","lessthan","biggerthan","questionMark","semicolon","colon",
                      "quote","doubleQuote","plus","sub","mul","or","exclamation","at","sharp","dollar",
                      "percent","powerMark","and","leftBra","rightBra","leftSBra","rightSBra","leftBraces",
                      "rightBraces","equal")
    (symbols zip paths).toMap
  }
  // initialize the Map to define the distance between each letter
  val letterDistance = {
    val characters = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz1234567890".toCharArray
    val distance = Array(
      0.03, 0.12, 0.09, 0.11, 0.12, 0.12, 0.09, 0.13, 0.17, 0.07, // A,B,C,D,E,F,G,H,I,J
      0.1,  0.1,  0.1,  0.1,  0.1,  0.1,  0.1,  0.08, 0.08, 0.08, // K,L,M,N,O,P,Q,R,S,T
      0.14, 0.08, 0.08, 0.08, 0.07, 0.08, 0.04, 0.1,  0.1,  0.1,  // U,V,W,X,Y,Z,a,b,c,d
      0.1,  0.07, 0.07, 0.1,  0.13, 0.1,  0.1,  0.1,  0.1,  0.1,  // e,f,g,h,i,j,k,l,m,n
      0.1,  0.1,  0.1,  0.1,  0.1,  0.07, 0.11, 0.08, 0.07, 0.07, // o,p,q,r,s,t,u,v,w,x
      0.07, 0.08, 0.35, 0.1,  0.1,  0.1,  0.1,  0.1,  0.1,  0.1,  // y,z,1,2,3,4,5,6,7,8
      0.1,  0.1)                                                  // 9,0 
    (characters zip distance).toMap
  }

  // load text .3ds file in
  def loadLetter(path: Char): Object3D =
    if (path.isUpper)         // read a uppercase character
      load3DSFile("Uppercase_Characters/", path.toString)
    else if (path.isLower)    // read a lowercase character
      load3DSFile("Lowercase_Characters/", path.toString)
    else if (path.isDigit)    // read a digit
      load3DSFile("Digit_Characters/", path.toString)
    else if (symbolPath.contains(path))    // read a digit
      load3DSFile("Symbol_Characters/", symbolPath(path))
    else null

  def load3DSFile(folder: String, file: String): Object3D =
    Loader.load3DS(getClass.getClassLoader.getResourceAsStream("acumen/ui/threeD/" + folder + file + ".3ds"), 1f)(0)

  // initialize the Character Objects Map
  val allCharacters = {
    val charToLoad = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz1234567890,./<>?;:\'\"+-*|!@#$%^&()[]{}=".toCharArray
    val characterObject = new Array[Object3D](90)
    for (i <- 0 until charToLoad.length)
      characterObject(i) = loadLetter(charToLoad(i))
    (charToLoad zip characterObject).toMap
  }
}

