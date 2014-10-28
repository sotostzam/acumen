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

/* 3D visualization panel */
class ThreeDView extends JPanel {

  // Set to true after everything finishes loading:
  private var initialized = false

  Config.maxPolysVisible = 100000
  Config.useRotationPivotFrom3DS = true
  Config.useMultipleThreads = true
  Config.maxNumberOfCores = java.lang.Runtime.getRuntime.availableProcessors()
  Logger.setLogLevel(Logger.ERROR)

  val world = new World  // create a new world
  private var camera = new Camera

  val characters = new Characters

  // Add texture for the axis
  val coAxes = new coAxis (characters.characters)
  val axes = coAxes.cylinders
  val axisArray = Array(new Object3D(1))

  protected[threeD] var objects = scala.collection.mutable.Map[List[_], Object3D]()
  protected[threeD] var scaleFactors = scala.collection.mutable.Map[Object3D, Array[Double]]()

  val defaultCamPos = new SimpleVector(3, -3, 10)
  private val lookAtPoint = new SimpleVector(0,0,0) // in jPCT coordinate system
  var customView = true // enable for allowing the user move the camera by themselves
  var preCustomView = customView // to enable custom view when we pause

  private var newMouseX = 1     // mouse position x before dragging
  private var newMouseY = 1     // mouse position y before dragging
  private var lastMouseX = 1    // mouse position x after dragging
  private var lastMouseY = 1    // mouse position y after dragging
  private var dragging = false  // flag for checking the mouse action
  private var cameraLeftDirection = -1  // to make sure the camera rotate forward or backward
  private var cameraRightDirection = -1  // to make sure the camera rotate forward or backward

  val lookAtCenter = Primitives.getSphere(20, 0.1f)

  letThereBeLight()  // create light sources for the scene

  addComponentListener(new ComponentAdapter {
    override def componentResized(e: ComponentEvent) = {
      val c = e.getSource.asInstanceOf[Component]
      initBuffer(c.getWidth, c.getHeight)
    }
  })

  addMouseListener(new MouseAdapter {
    override def mousePressed(e: MouseEvent) = {
      if (!dragging) {
        lastMouseX = e.getX
        lastMouseY = e.getY
        new setGlass(Color.RED, lookAtCenter, -1)
        world.addObject(lookAtCenter)
        CustomObject3D.partialBuild(lookAtCenter, false)
        lookAtCenter.translate(lookAtPoint.calcSub(lookAtCenter.getTransformedCenter))
        repaint()
      }
      dragging = true
    }
    override def mouseReleased(e: MouseEvent) = {
      dragging = false
      world.removeObject(lookAtCenter)
      repaint()
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
        if (abs(newMouseX - lastMouseX) > 5 || abs(newMouseY - lastMouseY) > 5) {
          // Initialize the camera coordinate
          if (SwingUtilities isLeftMouseButton e)        // left button dragging, move camera
            cameraLeftDirection = moveCamera(cameraLeftDirection, 1, lookAtPoint)
          else if (SwingUtilities isRightMouseButton e)   // right button dragging, move look at point
            cameraRightDirection = moveCamera(cameraRightDirection, -1, camera.getPosition)
        }
      }
    }
  })

  /** draggingDirection is the direction for left or right button, click is -1 (right) or 1 (left) */
  def moveCamera (draggingDirection: Int, click: Int, sphereCenter: SimpleVector): Int = {
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

  def axisOn() =
    if (!axisArray.contains(axes(0))) {
      axisArray(0) = axes(0)
      world.addObjects(axes)
      for (i <- 0 until axes.length)
        CustomObject3D.partialBuild(axes(i), i < 6)
      this.repaint()
    }

  def axisOff() =
    if (axisArray.contains(axes(0))) {
      for (i <- 0 until axes.length)
        world.removeObject(axes(i))
      axisArray(0) = null
      this.repaint()
    }

  // create a new buffer to draw on:
  var buffer: FrameBuffer = null

  def initBuffer(bufferWidth: Int, bufferHeight: Int) = {
    buffer = new FrameBuffer(bufferWidth, bufferHeight, FrameBuffer.SAMPLINGMODE_OGSS)
  }

  def init() = {
    // add the main box
    val mainbox = drawBox(1, 1, 1)
    mainbox.setShadingMode(Object3D.SHADING_FAKED_FLAT)
    new setGlass(new Color(180, 180, 180), mainbox, 0)
    world.addObject(mainbox)
    camera = world.getCamera  // grab a handle to the camera
    defaultView()
    lookAt(mainbox, null) // camera faces towards the object
    initialized = true
    lookAtPoint.set(0,0,0)
    world.buildAllObjects()
    this.repaint()
  }

  override def paint(g: Graphics) = {
    buffer.clear(Color.LIGHT_GRAY) // erase the previous frame
    // render the world onto the buffer:
    world.renderScene(buffer)
    world.draw(buffer)
    buffer.update()
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
    objects.clear()
    scaleFactors.clear()
    world.removeAllObjects()
    axisArray(0) = null
    defaultView()
    init()
  }

  def zoomin()  = camera.increaseFOV(0.1f)
  def zoomout() = camera.decreaseFOV(0.1f)

  def transformView(position: Array[Double], rotation: Array[Double]) = {
    val cameraToSet = world.getCamera
    cameraToSet.setPosition(-position(0).toFloat, -position(2).toFloat, -position(1).toFloat)
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

  /** Uses a vertex controller to rescale  **/
  def setReSize(scaleX: Float, scaleY: Float,  scaleZ: Float, planeMesh: Mesh) =
    if (planeMesh != null) {
      planeMesh.setVertexController(new Resizer(scaleX,scaleY,scaleZ), IVertexController.PRESERVE_SOURCE_MESH)
      planeMesh.applyVertexController()
      planeMesh.removeVertexController()
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
      rotateCamera.setBack(tranObjectRotMatrix)
      rotateCamera.matMul(tranObjectRotTempMat)
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
                 _3DDataBuffer: scala.collection.mutable.Map[CId, scala.collection.mutable.Map[Int, scala.collection.mutable.Buffer[List[_]]]],
                 lastFrame1: Double, endTime: Float,
                 _3DView: scala.collection.mutable.ArrayBuffer[(Array[Double], Array[Double])]) extends Publisher with Actor {
  /* Default directory where all the OBJ files are */
  private val _3DBasePath = Files._3DDir.getAbsolutePath
  private var currentFrame = 0 // FrameNumber
  var lastFrame = lastFrame1
  var totalFrames = lastFrame.toInt
  var pause = false
  var destroy = false
  val startFrameNumber = 2

  def stop() =
    if (app.objects.nonEmpty) {
      app.world.removeAllObjects()
      app.objects.clear()
      app.axisArray(0) = null
      app.scaleFactors.clear()
    }

  def bufferFrame(list: List[_]): Int =
    list.last match {
      case time: Int => time
      case _ => throw ShouldNeverHappen()
    }

  def bufferPosition(list: List[_]): Array[Double] =
    list(1) match {
      case p: Array[Double] => p
      case _ => throw ShouldNeverHappen()
    }

  def bufferAngle(list: List[_]): Array[Double] =
    list(4) match {
      case p: Array[Double] => p
      case _ => throw ShouldNeverHappen()
    }

  def bufferType(list: List[_]): String =
    list(0) match {
      case p: String => p
      case _ => throw ShouldNeverHappen()
    }

  def bufferColor(list: List[_]): List[Double] =
    list(3) match {
      case p: Array[Double] => p.toList
      case _ => throw ShouldNeverHappen()
    }

  def bufferSize(list: List[_]): List[Double] =
    list(2) match {
      case p: Array[Double] => p.toList
      case _ => throw ShouldNeverHappen()
    }

  def bufferString(list: List[_]): String =
    list(5) match {
      case s: String => s
      case _ => throw ShouldNeverHappen()
    }

  // Return the first frame number of the object
  def firstFrame(buffer: scala.collection.mutable.Buffer[List[_]]): Int =
    buffer.head.last match {
      case first: Int => first
      case _ => throw ShouldNeverHappen()
    }

  def lastFrame(buffer: scala.collection.mutable.Buffer[List[_]]): Int =
    buffer.last.last match {
      case last: Int => last
      case _ => throw ShouldNeverHappen()
    }

  def checkResizeable(newSize: List[Double]): Boolean = 
    newSize.forall(d => !(d.isNaN || d.isInfinite))

  def setScaleFactors(size: List[Double], o: Object3D, objectType: String,
                      scaleFactors: scala.collection.mutable.Map[Object3D, Array[Double]]) = {
    def scaleFactor(si: Int, bi1: Int, bi2: Int) =
      if (size(si) == 0) abs(o.getMesh.getBoundingBox()(bi1) - o.getMesh.getBoundingBox()(bi2)) / 0.001
      else               abs(o.getMesh.getBoundingBox()(bi1) - o.getMesh.getBoundingBox()(bi2)) / size(si)
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

  def checkSize (size: Double): Double = if (size == 0) 0.001 else size

  def calculateResizeFactor (o: Object3D, size: List[Double], scaleFactors: scala.collection.mutable.Map[Object3D, Array[Double]]): Array[Float] =
    if (scaleFactors.contains(o)) {
      if (o != null) {
        val (xFactor, yFactor, zFactor) =
          (if (size(0) == 0) scaleFactors(o)(0) * 0.001   / abs(o.getMesh.getBoundingBox()(1) - o.getMesh.getBoundingBox()(0))
           else              scaleFactors(o)(0) * size(0) / abs(o.getMesh.getBoundingBox()(1) - o.getMesh.getBoundingBox()(0)),
           if (size(1) == 0) scaleFactors(o)(1) * 0.001   / abs(o.getMesh.getBoundingBox()(3) - o.getMesh.getBoundingBox()(2))
           else              scaleFactors(o)(1) * size(1) / abs(o.getMesh.getBoundingBox()(3) - o.getMesh.getBoundingBox()(2)),
           if (size(2) == 0) scaleFactors(o)(2) * 0.001   / abs(o.getMesh.getBoundingBox()(5) - o.getMesh.getBoundingBox()(4))
           else              scaleFactors(o)(2) * size(2) / abs(o.getMesh.getBoundingBox()(5) - o.getMesh.getBoundingBox()(4)))
        Array(xFactor.toFloat, yFactor.toFloat, zFactor.toFloat)
      } else Array(0.001f,0.001f,0.001f)
    } else Array(0.001f,0.001f,0.001f)

  /**
   * Moving and rotating the object
   */
  def transformObject(id: List[_], objects: scala.collection.mutable.Map[List[_], Object3D],
                      scaleFactors: scala.collection.mutable.Map[Object3D, Array[Double]],
                      buffer: scala.collection.mutable.Buffer[List[_]],
                      currentFrame: Int) {
    var objID = 1
    /* Find the corresponding index of the object */
    val index = currentFrame - bufferFrame(buffer.head)
    /* Get the 3D information of the object at that frame	*/
    if (totalFrames > buffer.size)
      totalFrames = buffer.size
    val (tempPosition, tempAngle, tempColor, tempSize, tempType) =
      if (index >= 0 && index < totalFrames)
        (bufferPosition(buffer(index)) , bufferAngle(buffer(index)), bufferColor(buffer(index)),
        bufferSize(buffer(index)), bufferType(buffer(index)))
      else (Array(0.0,0.0,0.0), Array(0.0,0.0,0.0), List(0.0,0.0,0.0), List(0.0), " ")
    val (tempContent, tempPath) =
    if (index >= 0 && index < totalFrames)
      (if (tempType == "Text") bufferString(buffer(index)) else " ",
       if (tempType == "OBJ") bufferString(buffer(index)) else " ")
    else (" ", " ")
    // get the object ID
    if (objects.contains(id) && tempType != " ") {
      // get the object need to transform
      objID = objects(id).getID
      var transObject = view.getObject(objID)
      if (transObject != null) {
        // we don't need to care about first frame, since all the objects are fresh
        if (index >= 1) {
          // reset the type and size for the object, matching the type of object first
          val lastTempType = bufferType(buffer(index - 1))
          tempType match {
            case "Box" =>
              // the type has been changed, we need to delete the old object and create a one
              if (lastTempType != tempType) {
                // change the object in
                if (view.getObject(objID) != null)
                  view.removeObject(objID)
                // Since some object need to scale, we never allow the initial size become 0
                val (sizeToSetX, sizeToSetY, sizeToSetZ) = (checkSize(tempSize(1)),
                  checkSize(tempSize(0)),
                  checkSize(tempSize(2)))
                scaleFactors -= objects(id)
                objects(id) = app.drawBox(abs(sizeToSetX), abs(sizeToSetY), abs(sizeToSetZ))
                transObject = objects(id)
                setScaleFactors(List(sizeToSetY,sizeToSetZ,sizeToSetX), transObject, tempType, scaleFactors)
                objID = objects(id).getID // refresh the object ID
                view.addObject(transObject)
                transObject.setShadingMode(Object3D.SHADING_FAKED_FLAT)
              } else if (checkResizeable(tempSize)) {
                // just need change the size
                if (objects.contains(id) && transObject != null) {
                  val (sizeToSetX, sizeToSetY, sizeToSetZ) = (checkSize(tempSize(1)),
                    checkSize(tempSize(2)),
                    checkSize(tempSize(0)))
                  val factors = calculateResizeFactor(transObject, List(sizeToSetZ, sizeToSetY, sizeToSetX), scaleFactors)
                  val boxMesh = transObject.getMesh
                  app.setReSize(factors(0), factors(1), factors(2), boxMesh)
                }
              }
            case "Cylinder" => // we don't need to care about first frame, since all the objects are fresh
              // the type has been changed, we need to delete the old object and create a one
              if (lastTempType != tempType) {
                // change the object in
                if (view.getObject(objID) != null)
                  view.removeObject(objID)
                val (sizeToSetR, sizeToSetS) = (checkSize(tempSize(0)), checkSize(tempSize(1)))
                scaleFactors -= objects(id)
                objects(id) = Primitives.getCylinder(20, abs(sizeToSetR.toFloat), abs(sizeToSetS / (2 * sizeToSetR)).toFloat)
                transObject = objects(id)
                setScaleFactors(tempSize, transObject, tempType, scaleFactors)
                objID = objects(id).getID // refresh the object ID
                view.addObject(transObject)
                transObject.setShadingMode(Object3D.SHADING_FAKED_FLAT)
              } else if (checkResizeable(tempSize)) {
                if (objects.contains(id) && transObject != null) {   // just need change the size
                  val (sizeToSetR, sizeToSetS) = (checkSize(tempSize(0)), checkSize(tempSize(1)))
                  val factors = calculateResizeFactor(transObject, List(sizeToSetR, sizeToSetS, sizeToSetR), scaleFactors)
                  val boxMesh = transObject.getMesh
                  app.setReSize(factors(0), factors(1), factors(2), boxMesh)
                }
              }
            case "Cone" => // we don't need to care about first frame, since all the objects are fresh
              // the type has been changed, we need to delete the old object and create a one
              if (lastTempType != tempType) {
                // change the object in
                if (view.getObject(objID) != null)
                  view.removeObject(objID)
                val (sizeToSetR, sizeToSetS) = (checkSize(tempSize(0)), checkSize(tempSize(1)))
                scaleFactors -= objects(id)
                objects(id) = Primitives.getCone(20, abs(sizeToSetR.toFloat), abs(sizeToSetS / (sizeToSetR * 2)).toFloat)
                transObject = objects(id)
                setScaleFactors(tempSize, transObject, tempType, scaleFactors)
                objID = objects(id).getID // refresh the object ID
                view.addObject(transObject)
              } else if (checkResizeable(tempSize)) {
                if (objects.contains(id) && transObject != null) {   // just need change the size
                val (sizeToSetR, sizeToSetS) = (checkSize(tempSize(0)), checkSize(tempSize(1)))
                  val factors = calculateResizeFactor(transObject, List(sizeToSetR, sizeToSetS, sizeToSetR), scaleFactors)
                  val boxMesh = transObject.getMesh
                  app.setReSize(factors(0), factors(1), factors(2), boxMesh)
                }
              }
            case "Sphere" => // we don't need to care about first frame, since all the objects are fresh
              // the type has been changed, we need to delete the old object and create a one
              if (lastTempType != tempType) {
                // change the object in
                if (view.getObject(objID) != null)
                  view.removeObject(objID)
                val sizeToSetR = checkSize(tempSize(0))
                scaleFactors -= objects(id)
                objects(id) = Primitives.getSphere(10, abs(sizeToSetR.toFloat))
                transObject = objects(id)
                setScaleFactors(tempSize, transObject, tempType, scaleFactors)
                objID = objects(id).getID // refresh the object ID
                view.addObject(transObject)
              } else if (checkResizeable(tempSize)) {
                if (objects.contains(id) && transObject != null) {   // just need change the size
                val sizeToSetR = checkSize(tempSize(0))
                  val factors = calculateResizeFactor(transObject, List(sizeToSetR, sizeToSetR, sizeToSetR), scaleFactors)
                  val boxMesh = transObject.getMesh
                  app.setReSize(factors(0), factors(1), factors(2), boxMesh)
                }
              }
            case "Text" =>
              val lastTempContent = bufferString(buffer(index-1))
              // the type has been changed, we need to delete the old object and create a one
              if ((lastTempType != tempType || lastTempContent != tempContent) && !tempContent.isEmpty) {
                // change the object in
                if (view.getObject(objID) != null)
                  view.removeObject(objID)
                val sizeToSetR = checkSize(tempSize(0))
                scaleFactors -= objects(id)
                objects(id) = buildText(tempContent, sizeToSetR)
                transObject = objects(id)
                setScaleFactors(tempSize, transObject, tempType, scaleFactors)
                objID = objects(id).getID // refresh the object ID
                view.addObject(transObject)
              } else if (checkResizeable(tempSize)) {
                if (objects.contains(id) && transObject != null) {   // just need change the size
                val sizeToSetR = checkSize(tempSize(0))
                  val factors = calculateResizeFactor(transObject, List(sizeToSetR, sizeToSetR, sizeToSetR), scaleFactors)
                  val boxMesh = transObject.getMesh
                  app.setReSize(factors(0), factors(1), factors(2), boxMesh)
                }
              }
            case "OBJ" =>
              val lastTempPath = bufferString(buffer(index -1))
              // the type has been changed, we need to delete the old object and create a one
              if ((lastTempType != tempType || tempPath != lastTempPath) && !tempPath.isEmpty) {
                // change the object in
                if (view.getObject(objID) != null)
                  view.removeObject(objID)
                val sizeToSetR = checkSize(tempSize(0) / 10)
                scaleFactors -= objects(id)
                objects(id) = loadObj(tempPath, sizeToSetR)
                transObject = objects(id)
                setScaleFactors(tempSize, transObject, tempType, scaleFactors)
                objID = objects(id).getID // refresh the object ID
              } else if (checkResizeable(tempSize)) {
                if (objects.contains(id) && transObject != null) {   // just need change the size
                val sizeToSetR = checkSize(tempSize(0) / 10)
                  val factors = calculateResizeFactor(transObject, List(sizeToSetR, sizeToSetR, sizeToSetR), scaleFactors)
                  val boxMesh = transObject.getMesh
                  app.setReSize(factors(0), factors(1), factors(2), boxMesh)
                }
              }
            case _ => throw ShouldNeverHappen()
          }
        }
      }

      if (transObject != null) {
        // reset the color for the object
        setColor(transObject, tempColor)
        // rotate the object
        if (checkResizeable(tempAngle.toList))
          app.rotateObject(transObject, tempAngle, tempType, null)
        // calculate the transVector for the object and translate it
        val tempTransVector = new SimpleVector(-tempPosition(0), -tempPosition(2), -tempPosition(1))
        val transVector = tempTransVector.calcSub(transObject.getTransformedCenter)
        transObject.translate(transVector)
        CustomObject3D.partialBuild(transObject, tempType == "Text")
      }
    }
  }

  def renderCurrentFrame() = {
    for ((id, map) <- _3DDataBuffer) // acumen objects
      for ((objectNumber, buffer) <- map) // 3d objects within
        if (firstFrame(buffer) <= currentFrame && totalFrames >= currentFrame)
          if (!app.objects.contains(List(id, objectNumber)))
            matchingObject(List(id, objectNumber), buffer, currentFrame)
          else
            transformObject(List(id, objectNumber), app.objects, app.scaleFactors, buffer, currentFrame)
        else
          if (app.objects.contains(List(id, objectNumber)))
            deleteObj(List(id, objectNumber))
    if(currentFrame < _3DView.size)
      app.transformView(_3DView(currentFrame)._1, _3DView(currentFrame)._2)
    app.repaint()
  }

  // Main execution loop
  var view = app.world

  def act() {
    loopWhile(!destroy) {
      if (destroy)
        exit()
      react {
        case "go" =>
          renderCurrentFrame()
          if (slider.firstPlayed) {
            slider.firstPlayed = false
            slider.bar.enabled = true
          }
          if (currentFrame == totalFrames) {
            // Animation is over
            slider.setProgress3D(100)
            slider.setTime(endTime.toFloat)
            app.customView = true
            destroy = true
            pause = true
          }
          if (totalFrames > 0) {
            val percentage = currentFrame * 100 / totalFrames
            slider.setProgress3D(percentage)
            slider.setTime(percentage / 100f * endTime)
          }
          if (currentFrame < totalFrames)
            currentFrame += 1
      }
    }
  }

  // Reactions to the mouse events
  reactions += {
    case e: scala.swing.event.MouseDragged =>
      if (!slider.firstPlayed) {
        currentFrame = slider.bar.value * totalFrames / 100
        slider.setProgress3D(slider.bar.value)
        slider.setTime((slider.bar.value / 100f) * endTime)
        if (currentFrame < 2)
          currentFrame = startFrameNumber
        if (currentFrame > totalFrames)
          currentFrame = totalFrames
        if (pause)
          renderCurrentFrame()
      }
  }

  /**
   * Delete an 3D-object from scene
   */
  def deleteObj(c: List[_]) {
    if (app.objects.contains(c)) {
      app.objects.remove(c)
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
      if ((allChar(i).isLetterOrDigit || app.characters.symbolPath.contains(allChar(i))) && !firstLetter) {
        objectsArray += new Object3D(app.characters.characters(allChar(i)), false)
        // calculate the shift distance from last letter, firstly, we need to get the last letter index
        var j = 1 // the count for blank space
        while (!allChar(i - j).isLetterOrDigit && !app.characters.symbolPath.contains(allChar(i - j)) && j <= i) {
          j += 1
        }
        // get the width of last letter
        val tempDistance = objectsArray.apply(i - j).getMesh.getBoundingBox()(1) - objectsArray.apply(i - j).getMesh.getBoundingBox()(0)
        if (allChar(i-j).isLetterOrDigit)
          distanceToShift += tempDistance + app.characters.letterDistance(allChar(i-j))
        else
          distanceToShift += tempDistance + 0.2
        objectsArray.apply(i).translate(-distanceToShift.toFloat, 0, 0)
        objectsArray.apply(i).translateMesh()
        objectsArray.apply(i).setTranslationMatrix(new Matrix())
      } else if (firstLetter && (allChar(i).isLetterOrDigit || app.characters.symbolPath.contains(allChar(i)))){
        objectsArray += new Object3D(app.characters.characters(allChar(i)), false)
        firstLetter = false
      } else {
        objectsArray += new Object3D(1)
        firstLetter = false
        if (i != 0) {
          var j = 1     // the count for blank space
          while (!allChar(i - j).isLetterOrDigit && !app.characters.symbolPath.contains(allChar(i - j)) && j <= i) {
            j += 1
          }
          distanceToShift += 0.2 * j
        }
      }
    }
    val stringObject = Object3D.mergeAll(objectsArray.toArray)
    val boxMesh = stringObject.getMesh
    app.setReSize(size.toFloat, size.toFloat, size.toFloat, boxMesh)
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
      if (listOfFiles(i).getPath == texturePath && !TextureManager.getInstance().containsTexture(objectFileBase + ".mtl"))
        objectTexture = new Texture(texturePath)
      if (listOfFiles(i).getPath == objectFileBase + ".mtl")
        MTLPath = listOfFiles(i).getPath
    }
    val objFileloader = Loader.loadOBJ(_3DBasePath + File.separator + path, MTLPath, size.toFloat)(0)
    if (objectTexture != null) {
      TextureManager.getInstance().addTexture(MTLPath, objectTexture)
      objFileloader.setTexture(MTLPath)
    } else {
      objFileloader.setTexture(objectFileBase + ".mtl")
    }
    objFileloader
  }

  def matchingObject(c: List[_], buffer: scala.collection.mutable.Buffer[List[_]],
                     currentFrame: Int) = {
    /* Find the corresponding index of the object */
    val index = currentFrame - bufferFrame(buffer.head)
    if (totalFrames > buffer.size)
      totalFrames = buffer.size
    /* Get the 3D information of the object at that frame	*/
    val (position, angle, color, size, name) =
      if (index >= 0 && index < totalFrames)
        (bufferPosition(buffer(index)), bufferAngle(buffer(index)), bufferColor(buffer(index)),
         bufferSize(buffer(index)), bufferType(buffer(index)))
      else (Array(0.0,0.0,0.0), Array(0.0,0.0,0.0), List(0.0,0.0,0.0), List(0.0), " ")
    val (text, path) =
      if (index >= 0 && index < totalFrames)
        (if (name == "Text") bufferString(buffer(index)) else " ",
         if (name == "OBJ")  bufferString(buffer(index)) else " ")
      else (" ", " ")

    if (name != " ") {
      val newObject = name match {
        case "Box" =>
          // Since some object need to scale, we never allow the initial size become 0
          val (sizeToSetX, sizeToSetY, sizeToSetZ) = (checkSize(size(1)),
            checkSize(size(0)),
            checkSize(size(2)))
          app.drawBox(abs(sizeToSetX), abs(sizeToSetY), abs(sizeToSetZ))
        case "Cylinder" =>
          val (sizeToSetR, sizeToSetS) = (checkSize(size(0)), checkSize(size(1)))
          Primitives.getCylinder(20, abs(sizeToSetR).toFloat, abs(sizeToSetS / (sizeToSetR * 2)).toFloat)
        case "Cone" =>
          val (sizeToSetR, sizeToSetS) = (checkSize(size(0)), checkSize(size(1)))
          Primitives.getCone(20, abs(sizeToSetR.toFloat), abs(sizeToSetS / (sizeToSetR * 2)).toFloat)
        case "Sphere" =>
          val sizeToSetR = checkSize(size(0))
          Primitives.getSphere(20, abs(sizeToSetR.toFloat))
        case "Text" =>
          val sizeToSetR = checkSize(size(0))
          if (!text.isEmpty)  // model err, do nothing
            buildText(text, sizeToSetR)
          else
            null
        case "OBJ" =>
          val sizeToSetR = checkSize(size(0) / 10)
          if (!path.isEmpty)  // model err, do nothing
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
          setScaleFactors(List(size(0), size(2), size(1)), newObject, name, app.scaleFactors)
        else
          setScaleFactors(size, newObject, name, app.scaleFactors)
        app.objects -= c
        app.objects += c.toList -> newObject
        view.addObject(newObject)
        if (name != "Text")
          CustomObject3D.partialBuild(newObject, false)
        else {
          newObject.setRotationPivot(new SimpleVector(0,0,0))
          newObject.setCenter(new SimpleVector(0,0,0))
          CustomObject3D.partialBuild(newObject, true)
        }
      }
    }
  }

  def setColor(objectToSet: Object3D, colorList: List[Double]) =
    objectToSet.setAdditionalColor(new Color( max(0, min(255, colorList(0) * 255)).toInt
                                            , max(0, min(255, colorList(1) * 255)).toInt
                                            , max(0, min(255, colorList(2) * 255)).toInt ))

}

// Transparent box
class setGlass(color: Color, objectA: Object3D, transparancy: Int) {
  objectA.setTransparencyMode(Object3D.TRANSPARENCY_MODE_DEFAULT) //TRANSPARENCY_MODE_DEFAULT
  objectA.setTransparency(transparancy) // the transparency level. 0 is the highest possible transparency
  objectA.setAdditionalColor(color) // the color of the object3D
}

/* Vertex controller classes */

case class Resizer(xFactor: Float, yFactor: Float, zFactor: Float) extends GenericVertexController {
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
class coAxis(characters: scala.collection.immutable.Map[Char, Object3D]) {
  val cylinders: Array[Object3D] = new Array[Object3D](9)
  for (x <- 0 until 3)
    cylinders(x) = Primitives.getCylinder(12, 0.01f, 120f)
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
  cylinders(0).translate(0f, -1.2f, 0f)
  cylinders(3).translate(0f, -2.4f, 0f)
  cylinders(6).translate(-0.05f, -2.2f, 0f)
  cylinders(1).rotateZ(0.5f * -Pi.toFloat)
  cylinders(1).translate(-1.2f, 0f, 0f)
  cylinders(4).translate(-2.4f, -0.07f, 0f)
  cylinders(4).rotateZ(0.5f * Pi.toFloat)
  cylinders(7).translate(-2.2f, -0.05f, 0f)
  cylinders(2).rotateX(-0.5f * Pi.toFloat)
  cylinders(2).translate(0f, 0f, -1.2f)
  cylinders(5).translate(0f, -0.07f, -2.4f)
  cylinders(5).rotateX(-0.5f * Pi.toFloat)
  cylinders(8).translate(0f, -0.05f, -2.2f)
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
  val characters = {
    val charToLoad = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz1234567890,./<>?;:\'\"+-*|!@#$%^&()[]{}=".toCharArray
    val characterObject = new Array[Object3D](90)
    for (i <- 0 until charToLoad.length)
      characterObject(i) = loadLetter(charToLoad(i))
    (charToLoad zip characterObject).toMap
  }
}


