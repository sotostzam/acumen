package acumen.ui.threeD

import java.awt.event._
import java.awt.{Color, Component, Font, Graphics}
import java.io._
import javax.swing.JPanel

import acumen.CId
import acumen.Errors._
import acumen.render._
import acumen.ui.{App, Files, Progress3d}
import com.threed.jpct._

import scala.actors._
import scala.collection.mutable.Map
import scala.math._
import scala.swing.Publisher

/**
 * Created by xufei on 9/4/14.
 */

/* 3D visualization panel */
class jPCT_ThreeDView extends JPanel {

  var alive = true
  // Set to true after everything finishes loading:
  var initialized = false

  Config.maxPolysVisible = 100000

  val world = new World()  // create a new world

  letThereBeLight();  // create light sources for the scene

  var camera = new Camera

  var cameraPos = new SimpleVector()

  //Config.useMultipleThreads = true

  // Add texture for the axis
  val coAxises = new coAxis
  val axises = coAxises.cylinders

  val axisArray = Array(new Object3D(1))

  val defaultCamPos = new SimpleVector(3, -3, 10)

  var newMouseX = 1     // mouse position x before dragging
  var newMouseY = 1     // mouse position y before dragging
  var lastMouseX = 1    // mouse position x after dragging
  var lastMouseY = 1    // mouse position y after dragging
  var dragging = false  // flag for checking the mouse action
//  val fontToPaint = new GLFont(new Font("Dialog", Font.PLAIN, 12))

  addComponentListener(new ComponentAdapter {
    override def componentResized(e: ComponentEvent) = {
      val c = e.getSource.asInstanceOf[Component]
      initBuffer(c.getWidth, c.getHeight)
    }
  })

  addMouseListener(new MouseAdapter {
    override def mousePressed(e: MouseEvent) = {
      if (dragging == false){
        lastMouseX = e.getX()
        lastMouseY = e.getY()
      }
      dragging = true
    }
    override def mouseReleased(e: MouseEvent) = {
      dragging = false
    }
  })

  addMouseWheelListener(new MouseAdapter {
    override def mouseWheelMoved(e: MouseWheelEvent) = {
      //Returns the number of notches the mouse wheel was rotated.
      // If the mouse wheel rotated towards the user (down) the value is positive.
      // If the mouse wheel rotated away from the user (up) the value is negative.
      val zoomSpeed = e.getWheelRotation
      if (zoomSpeed >= 0) {    // zoom out
        zoomout
      } else {    // zoom in
        zoomin
      }
      camera.lookAt(new SimpleVector(0,0,0))
      repaint()
    }
  })

  addMouseMotionListener(new MouseAdapter {
    override def mouseDragged(e: MouseEvent) = {
      if(dragging){
        newMouseX = e.getX()
        newMouseY = e.getY()
        // to decrease the burden of calculations
        if (Math.abs(newMouseX - lastMouseX) > 5 || Math.abs(newMouseY - lastMouseY) > 5) {
          var alpha = 1.0
          var theta = 1.0
          var newX = 1.0
          var newY = 1.0
          var newZ = 1.0
          var tempInt = 1
          // Initialize the camera coordinate
          val cameraInitPos = camera.getPosition
          val initX = cameraInitPos.x
          val initY = cameraInitPos.y
          val initZ = cameraInitPos.z
          val radius = cameraInitPos.length()
          alpha = Math.acos(initY / radius)
          if (initX != 0) {
            theta = Math.atan2(initZ, initX)
          } else {
            if (initZ > 0)
              theta = Math.PI / 2
            else
              theta = -Math.PI / 2
          }
          val deltaAlpha = (newMouseY - lastMouseY) * Math.PI / 500
          val deltaTheta = (lastMouseX - newMouseX) * Math.PI / 750
          alpha += deltaAlpha
          // make sure alpha is between 0 and PI
          if (alpha > Math.PI) {
            tempInt = (alpha / Math.PI).toInt
            if (tempInt % 2 == 0) {
              alpha -= tempInt * Math.PI
            } else {
              alpha = (tempInt + 1) * Math.PI - alpha
            }
          } else if (alpha < 0) {
            tempInt = (alpha / Math.PI).toInt
            if (tempInt % 2 == 0) {
              alpha = Math.abs(alpha + tempInt * Math.PI)
            } else {
              alpha += (Math.abs(tempInt) + 1) * Math.PI
            }
          }
          theta += deltaTheta
          newX = radius * Math.sin(alpha) * Math.cos(theta)
          newZ = radius * Math.sin(alpha) * Math.sin(theta)
          newY = radius * Math.cos(alpha)
          val deltax = newX - initX
          val deltay = newY - initY
          val deltaz = newZ - initZ
          moveCamera(deltax, deltay, deltaz)
          lastMouseX = newMouseX
          lastMouseY = newMouseY
          camera.lookAt(new SimpleVector(0,0,0))
          repaint()
        }
      }
    }
  })

  def moveCamera(dx: Double, dy: Double, dz: Double)= {
    val moveVector = new SimpleVector(dx, dy, dz)
    cameraPos.set(camera.getPosition.calcAdd(moveVector))
    camera.setPosition(cameraPos)
  }

  def axisOn() = {
    if (!axisArray.contains(axises(0))){
      axisArray(0) = axises(0)
      world.addObjects(axises)
      world.buildAllObjects()
    }
    this.repaint()
  }

  def axisOff() = {
    if (axisArray.contains(axises(0))){
      world.removeObject(axises(0))
      world.removeObject(axises(1))
      world.removeObject(axises(2))
      world.buildAllObjects()
      axisArray(0) = null
      this.repaint()
    }
  }

  // create a new buffer to draw on:
  var buffer: FrameBuffer = null

  def initBuffer (bufferWidth: Int, bufferHeight: Int) = {
    buffer = new FrameBuffer(bufferWidth, bufferHeight, FrameBuffer.SAMPLINGMODE_OGSS)
  }

  def init() = {
    // add the main box
    val mainbox = drawBox(1,1,1)
    new setGlass(new Color(75,75,75), mainbox, 0)
    world.addObject(mainbox)
    lookAt(mainbox)  // camera faces towards the object
    initialized = true
    axisOn()
    world.buildAllObjects()
    this.repaint()
  }

  override def paint(g: Graphics) = {
    buffer.clear(Color.LIGHT_GRAY)   // erase the previous frame
    // render the world onto the buffer:
    world.renderScene(buffer)
    world.draw(buffer)
    buffer.update()
    buffer.display(g)
  }

  // point the camera toward the given object
  def lookAt(obj: Object3D) = {
    camera = world.getCamera()  // grab a handle to the camera
    defaultView()
    camera.lookAt(obj.getTransformedCenter())  // look toward the object
  }

  // create some light sources for the scene
  def letThereBeLight() = {
    // Set the overall brightness of the world:
    world.setAmbientLight(-200, -200, -200)
    // Create main light sources:
//    val d = 100
//    val i = (d / 7.5).toInt
//    List( (-d, -d,  0)
//      , ( d, -d,  0)
//      , ( 0, -d, -d)
//      , ( 0, -d,  d)
//    ).map(l => world.addLight(new SimpleVector(l._1, l._2, l._3), 10, 10, 10))
//    world.addLight(new SimpleVector(0, -100, 100), 14, 14, 14)
//    world.addLight(new SimpleVector(0, -100, -100), 14, 14, 14)
    world.addLight(new SimpleVector(15.076f, -7.904f, 0f), 18, 18, 18)
    world.addLight(new SimpleVector(-15.076f, -7.904f, 0f), 18, 18, 18)
    world.addLight(new SimpleVector(0,-5f,0), 2, 2, 2)
  }

  var objects = Map[List[_],Object3D]()
  // List[_] = [CID: class name, integer; instance number]

  def defaultView() = {
    cameraPos.set(defaultCamPos)
    camera.setPosition(cameraPos)
    camera.setFOVLimits(0.01f, 3.0f)
    camera.setFOV(0.55f)
  }

  def reset() = {
    world.removeAllObjects()
    axisArray(0) = null
    world.newCamera()
    defaultView()
    init()
  }

  def zoomin() = {
    camera.increaseFOV(0.1f)
  }

  def zoomout() = {
    camera.decreaseFOV(0.1f)
  }

  def drawBox(length: Double, width: Double, height: Double): Object3D = {
    val box = new Object3D(12)

    val upperLeftFront=new SimpleVector(-width/2, -height/2, -length/2)
    val upperRightFront=new SimpleVector(width/2, -height/2, -length/2)
    val lowerLeftFront=new SimpleVector(-width/2, height/2, -length/2)
    val lowerRightFront=new SimpleVector(width/2, height/2, -length/2)

    val upperLeftBack = new SimpleVector(-width/2, -height/2, length/2)
    val upperRightBack = new SimpleVector(width/2, -height/2, length/2)
    val lowerLeftBack = new SimpleVector(-width/2, height/2, length/2)
    val lowerRightBack = new SimpleVector(width/2, height/2, length/2)

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

  def draw3Dline(pointA: SimpleVector, pointB: SimpleVector, width: Float, textureName: String): Object3D = {
    val line = new Object3D(8)
    val offset = width / 10.0f
    line.addTriangle (new SimpleVector(pointA.x, pointA.y - offset, pointA.z), 0, 0,
                      new SimpleVector(pointA.x, pointA.y + offset, pointA.z), 0, 1,
                      new SimpleVector(pointB.x, pointB.y + offset, pointB.z), 1, 1,
                      TextureManager.getInstance.getTextureID(textureName))
    line.addTriangle (new SimpleVector(pointB.x, pointB.y + offset, pointB.z), 0, 0,
                      new SimpleVector(pointB.x, pointB.y - offset, pointB.z), 0, 1,
                      new SimpleVector(pointA.x, pointA.y - offset, pointA.z), 1, 1,
                      TextureManager.getInstance.getTextureID(textureName))
    line.addTriangle (new SimpleVector(pointB.x, pointB.y - offset, pointB.z), 0, 0,
                      new SimpleVector(pointB.x, pointB.y + offset, pointB.z), 0, 1,
                      new SimpleVector(pointA.x, pointA.y + offset, pointA.z), 1, 1,
                      TextureManager.getInstance.getTextureID(textureName))
    line.addTriangle (new SimpleVector(pointA.x, pointA.y + offset, pointA.z), 0, 0,
                      new SimpleVector(pointA.x, pointA.y - offset, pointA.z), 0, 1,
                      new SimpleVector(pointB.x, pointB.y - offset, pointB.z), 1, 1,
                      TextureManager.getInstance.getTextureID(textureName))
    line.addTriangle (new SimpleVector(pointA.x, pointA.y, pointA.z + offset), 0, 0,
                      new SimpleVector(pointA.x, pointA.y, pointA.z - offset), 0, 1,
                      new SimpleVector(pointB.x, pointB.y, pointB.z - offset), 1, 1,
                      TextureManager.getInstance.getTextureID(textureName))
    line.addTriangle (new SimpleVector(pointB.x, pointB.y, pointB.z - offset), 0, 0,
                      new SimpleVector(pointB.x, pointB.y, pointB.z + offset), 0, 1,
                      new SimpleVector(pointA.x, pointA.y, pointA.z + offset), 1, 1,
                      TextureManager.getInstance.getTextureID(textureName))
    line.addTriangle (new SimpleVector(pointB.x, pointB.y, pointB.z + offset), 0, 0,
                      new SimpleVector(pointB.x, pointB.y, pointB.z - offset), 0, 1,
                      new SimpleVector(pointA.x, pointA.y, pointA.z - offset), 1, 1,
                      TextureManager.getInstance.getTextureID(textureName))
    line.addTriangle (new SimpleVector(pointA.x, pointA.y, pointA.z - offset), 0, 0,
                      new SimpleVector(pointA.x, pointA.y, pointA.z + offset), 0, 1,
                      new SimpleVector(pointB.x, pointB.y, pointB.z + offset), 1, 1,
                      TextureManager.getInstance.getTextureID(textureName))
    line.setLighting(Object3D.LIGHTING_NO_LIGHTS)
    line.setAdditionalColor(Color.WHITE)
    return line
  }

  /** uses a vertex controller to rescale  **/

  def setBoxSize (scaleLength: Float, scaleWidth: Float, scaleHeight: Float, planeMesh: Mesh) = {
    val demoControl = new ResizerBox(scaleWidth,scaleHeight,scaleLength)
    planeMesh.setVertexController(demoControl, IVertexController.PRESERVE_SOURCE_MESH)
    planeMesh.applyVertexController()
    planeMesh.removeVertexController()
  }

  def setCylConeSize (radiusFactor: Float, heightFactor: Float, planeMesh: Mesh) = {
    val demoControl = new ResizerCylCone(radiusFactor, heightFactor)
    planeMesh.setVertexController(demoControl, IVertexController.PRESERVE_SOURCE_MESH)
    planeMesh.applyVertexController()
    planeMesh.removeVertexController()
  }
}

/* Timer for 3D-visualization, sends message to 3D renderer to coordinate animation */
class jPCT_ScalaTimer(receiver: _3DDisplayJPCT, endTime: Double,
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
class _3DDisplayJPCT(app: jPCT_ThreeDView, slider: Slider3d,
                     _3DDateBuffer: Map[CId, Map[Int, scala.collection.mutable.Buffer[List[_]]]],
                     lastFrame1: Double, endTime: Double,
                     _3DView: List[(Array[Double],Array[Double])]) extends Publisher with Actor {
  /* Default directory where all the OBJ files are */
  private val _3DBasePath = Files._3DDir.getAbsolutePath()
  var currentFrame = 0 // FrameNumber
  var totalFrames = 2
  var lastFrame = 2.0
  var pause = false
  var destroy = false
  lastFrame = lastFrame1
  totalFrames = lastFrame.toInt
  val startFrameNumber = 2

  def stop {
    if (!app.objects.isEmpty)
      app.world.removeAllObjects()
  }

  def bufferFrame(list: List[_]): Int = {
    list.last match {
      case time: Int => time
      case _ => throw ShouldNeverHappen()
    }
  }

  def bufferPosition(list: List[_]): Array[Double] = {
    list(1) match {
      case p: Array[Double] => p
      case _ => throw ShouldNeverHappen()
    }
  }

  def bufferAngle(list: List[_]): Array[Double] = {
    list(4) match {
      case p: Array[Double] => p
      case _ => throw ShouldNeverHappen()
    }
  }

  def bufferType(list: List[_]): String = {
    list(0) match {
      case p: String => p
      case _ => throw ShouldNeverHappen()
    }
  }

  def bufferColor(list: List[_]): List[Double] = {
    list(3) match {
      case p: Array[Double] => p.toList
      case _ => throw ShouldNeverHappen()
    }
  }

  def bufferSize(list: List[_]): List[Double] = {
    list(2) match {
      case p: Array[Double] => p.toList
      case _ => throw ShouldNeverHappen()
    }
  }

  def bufferString(list: List[_]): String = {
    list(5) match {
      case s: String => s
      case _ => throw ShouldNeverHappen()
    }
  }

  // Return the first frame number of the object
  def firstFrame(buffer: scala.collection.mutable.Buffer[List[_]]): Int = {
    buffer.head.last match {
      case first: Int => first
      case _ => throw ShouldNeverHappen()
    }
  }

  def lastFrame(buffer: scala.collection.mutable.Buffer[List[_]]): Int = {
    buffer.last.last match {
      case last: Int => last
      case _ => throw ShouldNeverHappen()
    }
  }

  def rotateCylCone(objectToRot: Object3D) = {
    objectToRot.rotateX((-Math.PI/2).toFloat)
  }

  /**
   * Moving and rotating the object
   */
  def transformObject(id: List[_], objects: Map[List[_], Object3D],
                      buffer: scala.collection.mutable.Buffer[List[_]],
                      currentFrame: Int) {
    var tempPosition = Array[Double](0.0, 0.0, 0.0)
    var tempAngle = Array[Double](0.0, 0.0, 0.0)
    var tempColor = List[Double](0.0, 0.0, 0.0)
    var tempSize = List[Double](0.0)
    var tempType = " "
    var objID = 1
    var transVector = new SimpleVector(0.0, 0.0, 0.0)
    /* Find the corresponding index of the object */
    val index = (currentFrame - bufferFrame(buffer.head))
    if (index >= 0 && index < buffer.size) {
      /* The position of the object at that frame	*/
      tempPosition = bufferPosition(buffer(index))
      /* The angle of the object at that frame */
      tempAngle = bufferAngle(buffer(index))
      /* The color of the object at that frame */
      tempColor = bufferColor(buffer(index))
      /* The size of the object at that frame */
      tempSize = bufferSize(buffer(index))
      /* The type of the object at that frame */
      tempType = bufferType(buffer(index))
    }
    // get the object ID
    objID = objects(id).getID
    // get the object need to transform
    var transObject = view.getObject(objID)
    // reset the type and size for the object, matching the type of object first
    tempType match {
      case "Box" => {
        // we don't need to care about first frame, since all the objects are fresh
        if (index >= 1){
          val lastTempType = bufferType(buffer(index - 1))
          val lastTempSize = bufferSize(buffer(index - 1))
          // the type has been changed, we need to delete the old object and create a one
          if (lastTempType != tempType) {
            // change the object in
            objects(id) = app.drawBox(abs(tempSize(0)), abs(tempSize(1)), abs(tempSize(2)))
            transObject = objects(id)
            objID = objects(id).getID // renew the object ID
            transObject.setShadingMode(Object3D.SHADING_FAKED_FLAT)
          } else { // just need change the size
            if (lastTempSize != tempSize) {
              val boxMesh = transObject.getMesh
              val lengthFactor = tempSize(0) / lastTempSize(0)
              val widthFactor = tempSize(1) / lastTempSize(1)
              val heightFactor = tempSize(2) / lastTempSize(2)
              app.setBoxSize(lengthFactor.toFloat, widthFactor.toFloat, heightFactor.toFloat, boxMesh)
            }
          }
        }
      }
      case "Cylinder" => {
        // we don't need to care about first frame, since all the objects are fresh
        if (index >= 1){
          val lastTempType = bufferType(buffer(index - 1))
          val lastTempSize = bufferSize(buffer(index - 1))
          // the type has been changed, we need to delete the old object and create a one
          if (lastTempType != tempType) {
            // change the object in
            objects(id) = Primitives.getCylinder(50, abs((tempSize(0)).toFloat), abs((tempSize(1) / (2 * tempSize(0))).toFloat))
            transObject = objects(id)
            objID = objects(id).getID // renew the object ID
            rotateCylCone(transObject)
            transObject.setShadingMode(Object3D.SHADING_FAKED_FLAT)
          } else { // just need change the size
            if (lastTempSize != tempSize) {
              val boxMesh = transObject.getMesh
              val radiusFactor = tempSize(0) / lastTempSize(0)
              val heightFactor = tempSize(1) / lastTempSize(1)
              app.setCylConeSize(radiusFactor.toFloat, heightFactor.toFloat, boxMesh)
            }
          }
        }
      }
      case "Cone" => {
        // we don't need to care about first frame, since all the objects are fresh
        if (index >= 1){
          val lastTempType = bufferType(buffer(index - 1))
          val lastTempSize = bufferSize(buffer(index - 1))
          // the type has been changed, we need to delete the old object and create a one
          if (lastTempType != tempType) {
            // change the object in
            objects(id) = Primitives.getCone(50, abs((tempSize(0)).toFloat), abs((tempSize(1) / (tempSize(0) * 2)).toFloat))
            transObject = objects(id)
            objID = objects(id).getID // renew the object ID
            rotateCylCone(transObject)
          } else { // just need change the size
            if (lastTempSize != tempSize) {
              val boxMesh = transObject.getMesh
              val radiusFactor = tempSize(0) / lastTempSize(0)
              val heightFactor = tempSize(1) / lastTempSize(1)
              app.setCylConeSize(radiusFactor.toFloat, heightFactor.toFloat, boxMesh)
            }
          }
        }
      }
      case "Sphere" => {
        // we don't need to care about first frame, since all the objects are fresh
        if (index >= 1){
          val lastTempType = bufferType(buffer(index - 1))
          val lastTempSize = bufferSize(buffer(index - 1))
          // the type has been changed, we need to delete the old object and create a one
          if (lastTempType != tempType) {
            // change the object in
            objects(id) = Primitives.getSphere(10, abs((tempSize(0)).toFloat))
            transObject = objects(id)
            objID = objects(id).getID // renew the object ID
          } else { // just need change the size
            if (lastTempSize != tempSize) {
              val radiusFactor = tempSize(0) / lastTempSize(0)
              transObject.scale(radiusFactor.toFloat)
            }
          }
        }
      }
      case "Text" => // do nothing now
      case "OBJ" => // do nothing now
      case _ => throw ShouldNeverHappen()
    }
    // rotate the object
    if (index >= 1) {
      val lastTempAngle = bufferAngle(buffer(index - 1))
      if (tempAngle(0) != lastTempAngle(0)) {
        transObject.rotateX((tempAngle(0) - lastTempAngle(0)).toFloat)
      }
      if (tempAngle(1) != lastTempAngle(1)) {
        transObject.rotateZ((tempAngle(1) - lastTempAngle(1)).toFloat)
      }
      if (tempAngle(2) != lastTempAngle(2)) {
        transObject.rotateY((lastTempAngle(2) - tempAngle(2)).toFloat)
      }
    } else {
      transObject.rotateZ(tempAngle(1).toFloat)
      transObject.rotateY(-tempAngle(2).toFloat)
      transObject.rotateX(tempAngle(0).toFloat)
    }

    // calculate the transVector for the object
    val tempTransVector = new SimpleVector(-tempPosition(0), -tempPosition(2), -tempPosition(1))
    transVector = tempTransVector.calcSub(transObject.getTransformedCenter)
    transObject.translate(transVector)
    // reset the color for the object
    val colorToSet = Array(1.0, 1.0, 1.0)
    colorToSet(0) = tempColor(0) * 255
    colorToSet(1) = tempColor(1) * 255
    colorToSet(2) = tempColor(2) * 255
    if (colorToSet(0) > 255)
      colorToSet(0) = 255
    if (colorToSet(1) > 255)
      colorToSet(1) = 255
    if (colorToSet(2) > 255)
      colorToSet(2) = 255
    transObject.setAdditionalColor(new Color(colorToSet(0).toInt, colorToSet(1).toInt, colorToSet(2).toInt))
  }

  def renderCurrentFrame() = {
    for ((id, map) <- _3DDateBuffer) { // acumen objects
      for ((objectNumber, buffer) <- map) { // 3d objects within
        if (firstFrame(buffer) <= currentFrame && lastFrame(buffer) >= currentFrame) {
          if (!app.objects.contains(List(id, objectNumber))) {
            matchingObject(List(id, objectNumber), buffer, currentFrame)
          } else {
            transformObject(List(id, objectNumber), app.objects, buffer, currentFrame)
          }
        } else {
          if (app.objects.contains(List(id, objectNumber))) {
            deleteObj(List(id, objectNumber))
            view.removeObject(app.objects.getOrElse(List(id, objectNumber), null))  // remove the object from the view
          }
        }
      }
    }
  }

  // Main execution loop
  var view = app.world
  def act() {
    loopWhile(!destroy) {
      if (destroy)
        exit
      react {
        case "go" => {
          renderCurrentFrame
          view.buildAllObjects()
          app.repaint()
          if (currentFrame == totalFrames){ // Animation is over
            emitProgress(100)
            destroy = true
            pause = true
          }
          if (totalFrames > 0)
            emitProgress((currentFrame * 100 / totalFrames))
          if (currentFrame < totalFrames)
            currentFrame += 1
        }
      }
    }
  }

  def addText (text: String, size: Double, color: List[Double],
               position: Array[Double], framebuffer: FrameBuffer) = {
    val glFont = GLFont.getGLFont(new java.awt.Font("Dialog", Font.PLAIN, size.toInt))
    var colorHSB = Array[Float](1,1,1)
    // transfer acumen color (RGB=>(0,1)) to jPCT color (HSB=>(0,1))
    colorHSB = Color.RGBtoHSB((color(0) * 255).toInt,(color(1) * 255).toInt, (color(2) * 255).toInt, colorHSB)
    glFont.blitString(framebuffer, text, position(0).toInt, position(1).toInt, 1, Color.getHSBColor(colorHSB(0),colorHSB(1),colorHSB(2)));
  }

  /**
   * Delete an 3D-object from scene
   */
  def deleteObj(c: List[_]) {
    if (app.objects.contains(c)) {
      app.objects.remove(c)
    }
  }

  // Load .obj fil
  def loadObj(path:String, size: Double): Array[Object3D] = {
    //read in the geometry information from the data file
    val objFileloader = Loader.loadOBJ(_3DBasePath + File.separator + path,null,size.toFloat)
    return objFileloader
  }

  def matchingObject (c: List[_], buffer: scala.collection.mutable.Buffer[List[_]],
                      currentFrame: Int) = {
    var color = List[Double](1.0, 1.0, 1.0)
    var size = List[Double](1.0)
    var position = Array[Double](1.0,1.0,1.0)
    var angle = Array[Double](1.0,1.0,1.0)
    var name = " "
    var path = " "
    var text = " "
    val index = (currentFrame - bufferFrame(buffer.head))
    var opaque = false
    var transVector = new SimpleVector(0,0,0)
    if (index >= 0 && index < buffer.size) {
      val list = buffer(index)
      color = bufferColor(list) // Get the color and size of the object
      size = bufferSize(list)
      angle = bufferAngle(list)
      name = bufferType(list)
      position = bufferPosition(list)
      if (name == "Text")
        text = bufferString(list)
      else if (name == "OBJ")
        path = bufferString(list)
      else if (list(5) == "transparent")
        opaque = true
    }

    var newObject = new Object3D(1)

    name match {
      case "Box" => newObject = app.drawBox(abs(size(1)), abs(size(0)),abs(size(2)))
      case "Cylinder" => newObject = Primitives.getCylinder(50, abs((size(0)).toFloat), abs((size(1) / (size(0) * 2)).toFloat))
      case "Cone" => newObject = Primitives.getCone(50, abs((size(0)).toFloat), abs((size(1) / (size(0) * 2)).toFloat))
      case "Sphere" => newObject = Primitives.getSphere(20, abs((size(0)).toFloat))
      case "Text" => //addText(text, size(0), color, position, app.buffer)
      case "OBJ" =>
        //for (a <- loadObj(path, size(0)))
          //newObject = a
      case _ => throw ShouldNeverHappen()
    }

    val colorToSet = Array(1.0, 1.0, 1.0)
    colorToSet(0) = color(0) * 255
    colorToSet(1) = color(1) * 255
    colorToSet(2) = color(2) * 255
    if (colorToSet(0) > 255)
      colorToSet(0) = 255
    if (colorToSet(1) > 255)
      colorToSet(1) = 255
    if (colorToSet(2) > 255)
      colorToSet(2) = 255
    newObject.setAdditionalColor(new Color(colorToSet(0).toInt, colorToSet(1).toInt, colorToSet(2).toInt))

    if (name == "Cylinder" || name == "Cone")
      rotateCylCone(newObject)

    if (name == "Box" || name == "Cylinder")
      newObject.setShadingMode(Object3D.SHADING_FAKED_FLAT)

    // Once we added the object, we should also move the object to the position at that time
    newObject.rotateZ(angle(1).toFloat)
    newObject.rotateY(-angle(2).toFloat)
    newObject.rotateX(angle(0).toFloat)

    // calculate the transVector for the object
    val tempTransVector = new SimpleVector(-position(0), -position(2), -position(1))
    transVector = tempTransVector.calcSub(newObject.getTransformedCenter)

    newObject.translate(transVector)

    //newObject.setSpecularLighting(true)
    if (name != "Text") {
      app.objects -= c
      app.objects += c.toList -> newObject
      view.addObject(newObject)
    }
  }

  // Update the slider value
  private def emitProgress(p: Int) = App.actor ! Progress3d(p)

}

// Transparent box
class setGlass(color: Color, objectA: Object3D, transparancy: Int) {
  //objectA.setSpecularLighting(true)
  objectA.setTransparencyMode(Object3D.TRANSPARENCY_MODE_DEFAULT) //TRANSPARENCY_MODE_DEFAULT
  objectA.setTransparency(transparancy) // the transparency level. 0 is the highest possible transparency
  objectA.setAdditionalColor(color) // the color of the object3D
  //objectA.setCulling(false) //Disables back side culling for the current object
}

// vertax controller classes

class ResizerBox (xFactor: Float, yFactor: Float, zFactor: Float) extends GenericVertexController {

  var XFactor = xFactor
  var YFactor = yFactor
  var ZFactor = zFactor

  def apply() {
    val s = getSourceMesh()
    val d = getDestinationMesh()

    for (i <- 0 until s.length) {
      d(i).x = s(i).x * XFactor
      d(i).y = s(i).y * YFactor
      d(i).z = s(i).z * ZFactor
    }
  }
}

class ResizerCylCone (radiusFactor: Float, heightFactor: Float) extends GenericVertexController {

  var RadiusFactor = radiusFactor
  var HeightFactor = heightFactor

  def apply() {
    val s = getSourceMesh()
    val d = getDestinationMesh()

    for (i <- 0 until s.length) {
      d(i).x = s(i).x * RadiusFactor
      d(i).z = s(i).z * RadiusFactor
      d(i).y = s(i).y * HeightFactor
    }
  }
}

//Axis
class coAxis{
  val cylinders:Array[Object3D] = new Array[Object3D](3)
  for(x <- 0 until cylinders.length){
    cylinders(x) = Primitives.getCylinder(50,0.01f,400f)
    cylinders(x).build()
  }
  new setGlass(Color.BLUE,cylinders(0), -1) //Z
  new setGlass(Color.RED,cylinders(1), -1) //X
  new setGlass(Color.GREEN,cylinders(2), -1) //Y

  cylinders(0).translate(0f,-3f,0f) // if use 30f then use y = -3(-2.5)
  cylinders(1).rotateZ(0.5f * -Math.PI.toFloat)
  cylinders(1).translate(-3f,0f,0f)
  cylinders(2).rotateX(-0.5f * Math.PI.toFloat)
  cylinders(2).translate(0f,0f, -3f)
}