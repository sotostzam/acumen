package acumen.ui.threeD

import java.awt.event._
import java.awt.image.BufferedImage
import java.awt.{Color, Component, Graphics}
import java.io._
import javax.imageio.ImageIO
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
  val mainbox = drawBox(1, 1, 1)
  val coAxes = new coAxis (characters.allCharacters, mainbox)
  val axes = coAxes.cylinders
  val axisArray = Array(new Object3D(1))

  protected[threeD] var objects = mutable.Map[(CId, Int), Object3D]()
  protected[threeD] var scaleFactors = mutable.Map[Object3D, Array[Double]]()
  protected[threeD] var objectsToDelete = mutable.ArrayBuffer[Object3D]()
  protected[threeD] var objectsCopy = mutable.Map[(CId, Int), Object3D]()  // for anaglyph
  val eyeOffSet = 0.15f
  val anaglyphTexture = getClass.getClassLoader.getResourceAsStream("acumen/ui/threeD/anaglyph.png")
  val imageOrig = ImageIO.read(anaglyphTexture)//ImageIO.read(anaglyphInputstream)

  val defaultCamPos = new SimpleVector(3, -3, 10)
  private val lookAtPoint = new SimpleVector(0,0,0) // in jPCT coordinate system
  var customView = true // enable for allowing the user move the camera by themselves
  var preCustomView = customView // to enable custom view when we pause

  var percentagemissDL = 0.0
  var averageSlack = 0.0
  private var fps = 0
  private var lps = 0
  private var _3DTimeCounter = System.currentTimeMillis()

  private var newMouseX = 1     // mouse position x before dragging
  private var newMouseY = 1     // mouse position y before dragging
  private var lastMouseX = 1    // mouse position x after dragging
  private var lastMouseY = 1    // mouse position y after dragging
  private var dragging = false  // flag for checking the mouse action
  private var cameraLeftDirection = (-1,-1)  // to make sure the camera rotate forward or backward both in X and Y directions
  private var cameraRightDirection = (1,1) // to make sure the camera rotate forward or backward both in X and Y directions
  private var cameraFlipped = false

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
            cameraLeftDirection = moveCamera(cameraLeftDirection, 1)
          // right button dragging, move look at point
          else if (SwingUtilities isRightMouseButton e)
            cameraRightDirection = moveCamera(cameraRightDirection, -1)
        }
      }
    }
  })

  /* draggingDirection is the direction for left or right button,
   * click is -1 (right) or 1 (left) */
  def moveCamera (initialCameraDirection: (Int,Int), click: Int): (Int,Int) = {
    // conversion between jPCT and XYZ
    def convSVtoXYZ(v : SimpleVector) = (v.x.toDouble, v.y.toDouble, v.z.toDouble)
    def convXYZtoSV(v : (Double, Double, Double)) = v match { case (x, y, z) => 
      new SimpleVector(x.toFloat, y.toFloat, z.toFloat) }
    
    // conversion between XYZ and Spherical
    // (x, y, z) -> (r, theta, phi)
    def convXYZtoSPH(v : (Double, Double, Double)) = v match { case (x, y, z) =>
      val r     = sqrt(x * x + y * y + z * z)              // radius
      val theta = if (r != 0) acos(z / r) else 0           // theta = the angle of the vector with the (x,y) plane
      val phi   = if (x != 0 || y != 0) atan2(y, x) else 0 // phi = plane polar angle of the projection on the (x,y) plane
      (r, theta, phi) }

    // (r, theta, phi) -> (x, y, z)
    def convSPHtoXYZ(v : (Double, Double, Double)) = v match { case (r, theta, phi) =>
      val x = r * sin(theta) * cos(phi)
      val y = r * sin(theta) * sin(phi)
      val z = r * cos(theta)
      (x, y, z) }
    
    // transformation of XYZ coordinates
    def transform(positionVector : SimpleVector, newOrigin : SimpleVector, invert : Boolean = false) : SimpleVector = { 
      // the local coordinates given by the orientation of the camera
      // the new origin is newOrigin
      val newX = new SimpleVector(1,0,0)
      val newY = new SimpleVector(0,0,1)
      val newZ = new SimpleVector(0,1,0)
      
      // the transformation matrix
      val S = new Matrix()
      S.setRow(0, newX.x, newX.y, newX.z, 0)
      S.setRow(1, newY.x, newY.y, newY.z, 0)
      S.setRow(2, newZ.x, newZ.y, newZ.z, 0) 

      invert match {
        case false => // the transformation
          val v = newOrigin calcSub positionVector
          v rotate S
          v
        case true  => // the inverse transformation
          val v = positionVector
          v rotate (S invert3x3())
          newOrigin calcSub v 
      } }
    
    // initial XYZ of the 1st object in local coordinates centered at the 2nd
    val (x0, y0, z0) = convSVtoXYZ( if (click == 1)
                                      transform(camera.getPosition, lookAtPoint)
                                    else
                                      transform(lookAtPoint, camera.getPosition) )
    
    // initial spherical coordinates
    val (r0, theta0, phi0) = convXYZtoSPH((x0, y0, z0))

    // mouse movement
    val deltaTheta = initialCameraDirection._1 * (newMouseY - lastMouseY) * Pi / 500
    val deltaPhi   = initialCameraDirection._2 * (newMouseX - lastMouseX) * Pi / 750
    
    // updated spherical coordinates
    var (r1, theta1, phi1) = (r0, theta0 + deltaTheta, phi0 + deltaPhi)

    // camera flip
    val cameraDirection = 
      ( if (signum(sin(theta0)) != signum(sin(theta1))) {
          theta1 = -theta1
          phi1 = phi1 + Pi
          if (click == 1) cameraFlipped = !cameraFlipped 
          -1 * initialCameraDirection._1  
        } else initialCameraDirection._1 , 
        initialCameraDirection._2 )
    
    // updated XYZ coordinates
    val (x1, y1, z1) = convSPHtoXYZ((r1, theta1, phi1))
      
    // updated jPCT
    if (click == 1) camera      setPosition transform(convXYZtoSV((x1, y1, z1)), lookAtPoint, true)
    else            lookAtPoint set         transform(convXYZtoSV((x1, y1, z1)), camera.getPosition, true)
    
    // look at the lookAtPoint
    lookAt(null,lookAtPoint)
    
    // if the camera is flipped we need to adjust
    if (cameraFlipped)
      camera.rotateCameraZ(Pi.toFloat)

    // translate the lookAtSphere so that it is centered at lookAtPoint
    lookAtCenter.translate(lookAtPoint.calcSub(lookAtCenter.getTransformedCenter))

    // storing the mouse position
    lastMouseX = newMouseX
    lastMouseY = newMouseY
    
    repaint()
    cameraDirection
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
    camera = world.getCamera  // grab a handle to the camera
    cameraLeftDirection = (-1,-1)
    cameraRightDirection = (1,1)
    cameraFlipped = false
    defaultView()
    lookAt(coAxes.mainbox, null) // camera faces towards the object
    lookAtPoint.set(0,0,0)
  }

  // the state machine for adding or deleting objects in view state machine
  /* There are seven states in total
  *  which are "deleteAllObject" -> delete all the objects
  *            "renderCurrentObjects" -> add and delete objects according to
  *                                      current frame
  *            "deleteAxes" -> delete the axes
  *            "deleteLookAtSphere" -> delete the red sphere at look at point
  *            "addAxes" -> add the axes and main box
  *            "addLookAtSphere" -> add the red sphere at look at point*/

  protected[threeD] var waitingPaint = false

  def viewStateMachine(worldState: String) = {
    world.synchronized {
      waitingPaint = true
      // object deleting state machine
      worldState match {
        case "renderCurrentObjects" => // only called in renderCurrentFrame()
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
          // add the anaglyph objects in current frame
          if (acumen.ui.App.ui.getStartAnaglyph)
            for ((objectKey, objectToBuild) <- objectsCopy)
              if (world.getObjectByName(objectToBuild.getName) == null)
                world.addObject(objectToBuild)
        case "addAxes" => // only called in axisOff function
          if (!axisArray.contains(axes(0))) {
            axisArray(0) = axes(0)
            for (i <- axes.indices)
              CustomObject3D.partialBuild(axes(i), i < 7)
            world.addObjects(axes)
          }
        case "addLookAtSphere" => // called when camera rotation is finished
          if (world.getObjectByName(lookAtCenter.getName) == null)
            world.addObject(lookAtCenter)
        case "deleteAllObjects" =>
          world.removeAllObjects()
        case "deleteAxes" => // only called in axisOff function
          if (axisArray.contains(axes(0))) {
            for (i <- axes.indices)
              world.removeObject(axes(i))
            axisArray(0) = null
          }
        case "deleteLookAtSphere" => // called when camera rotation is finished
          if (world.getObjectByName(lookAtCenter.getName) != null)
            world.removeObject(lookAtCenter)
        case _ => throw ShouldNeverHappen()
      }
      repaint()
    }
  }

  override def paint(g: Graphics) = this.synchronized{
    world.synchronized {
      buffer.clear(Color.LIGHT_GRAY) // erase the previous frame
      // render the world onto the buffer:
      world.renderScene(buffer)
      world.draw(buffer)
      buffer.update()
      buffer.display(g)
      // calculate the fps
      fps += 1
      if (System.currentTimeMillis() - _3DTimeCounter > 1000) {
        lps = fps
        fps = 0
        _3DTimeCounter = System.currentTimeMillis()
      }
      g.drawString("FPS: " + lps.toString, 10, 30)
      // draw real time render information
      if (acumen.ui.App.ui.getStartRealTime) {
        g.drawString("Missed deadlines: %.4f".format(percentagemissDL * 100) + "%", 10, 45)
        g.drawString("Waiting time: %.4f".format(averageSlack * 100) + "%", 10, 60)
      }
      waitingPaint = false
    }
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
    world.setAmbientLight(-125, -125, -125)
    // Create main light sources:
    world.addLight(new SimpleVector(150f, -80f, 0f), 18, 18, 18)
    world.addLight(new SimpleVector(-150f, -80f, 0f), 18, 18, 18)
    world.addLight(new SimpleVector(0f, -80f, -150f), 8, 8, 6)
    world.addLight(new SimpleVector(0f, -80f, 150f), 8, 8, 8)
    world.addLight(new SimpleVector(0, -80f, 0), 6, 6, 6)
  }

  def defaultView() = {
    camera.setPosition(defaultCamPos)
    camera.setFOVLimits(0.01f, 3.0f)
    camera.setFOV(0.65f)
  }

  def reset() = {
    viewStateMachine("deleteAllObjects")
    objects.clear()
    objectsCopy.clear()
    scaleFactors.clear()
    objectsToDelete.clear()
    axisArray(0) = null
    init()
  }

  def zoomin()  = camera.increaseFOV(0.02f)
  def zoomout() = camera.decreaseFOV(0.02f)

  def transformView(position: Array[Double], rotation: Array[Double]) = {
    val cameraToSet = world.getCamera
    cameraToSet.setPosition(-position(0).toFloat, -position(2).toFloat,
                            -position(1).toFloat)
    rotateObject(null, rotation, "Camera", cameraToSet)
  }

  /** Isolate Color Channel */
  def isolateColorChannel(image: BufferedImage, channel: String): BufferedImage = {
    val result = new BufferedImage(image.getWidth, image.getHeight, image.getType)
    for (i <- 0 until image.getWidth){
      for (j <- 0 until image.getHeight) {
        var newPixel: Int = 0
        val red = new Color(image.getRGB(i, j)).getRed
        val green = new Color(image.getRGB(i, j)).getGreen
        val blue = new Color(image.getRGB(i, j)).getBlue
        if (channel == "red")
          newPixel = newPixel | red << 16
        else if (channel == "green")
          newPixel = newPixel | green << 8
        else if (channel == "cyan")
          newPixel = newPixel | green << 8 | blue
        result.setRGB(i, j, newPixel)
      }
    }
    result
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
    tranObjectRotMatrixZ.rotateZ(angle(1).toFloat)
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
  if (playSpeed < 1)
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
class _3DDisplay(app: ThreeDView, slider: Slider3D, playSpeed: Double,
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
  initAnaglyph()

  def stop() = {
    app.viewStateMachine("deleteAllObjects")
    app.objects.clear()
    app.objectsCopy.clear()
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
        valueList.head
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
      for (i <- lastSize.indices)
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

  def renderFrameInRealTime() = this.synchronized {
    app.objects.synchronized {
      app.objectsToDelete.clear()
      val latestFrame = _3DDataBuffer.size - 1
      if (_3DDataBuffer(latestFrame) != null) {
        for ((objectKey, valueList) <- _3DDataBuffer(latestFrame))
          if (!app.objects.contains(objectKey))
            matchingObject(objectKey, valueList, latestFrame)
          else if (app.objects.contains(objectKey)  // this should not happen
            && app.world.getObjectByName(app.objects(objectKey).getName) == null) {
            app.objects -= objectKey
            matchingObject(objectKey, valueList, latestFrame)
          } else
            transformObject(objectKey, valueList, lastRenderFrame, latestFrame)

        // delete the object not in this frame
        for ((objectKey, o) <- app.objects)
          if (_3DDataBuffer.contains(latestFrame) && !_3DDataBuffer(latestFrame).contains(objectKey))
            deleteObj(objectKey)
        lastRenderFrame = latestFrame
        if (_3DView.nonEmpty)
          app.transformView(_3DView.last._1, _3DView.last._2)
        app.viewStateMachine("renderCurrentObjects")
      }
    }
  }

  /**
   * Moving and rotating the object
   */
  def transformObject(objectKey: (CId, Int), valueList: List[_],
                      lastFrame: Int, currentFrame: Int) {
    var objID = app.objects(objectKey).getName  // get the object ID
    /* Get the 3D information of the object at that frame	*/
    val (name: String, position: Array[Double], size: Array[Double],
    color: Array[Double], angle: Array[Double]) =
      (valueList(0), valueList(1), valueList(2), valueList(3), valueList(4))
    val (text: String, path: String) =
      (if (name == "Text") valueList(5) else " ",
       if (name == "OBJ")  valueList(5) else " ")

    // get the object need to transform
    var transObject: Object3D = app.objects(objectKey)
    var anaglyphObject: Object3D =
      if (acumen.ui.App.ui.getStartAnaglyph) app.objectsCopy(objectKey)
      else null
    var newAnaglyphObject = false

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
        if (lastFrameName != name && checkResizeable(size)
          && checkResizeable(angle)) {
          // change the object in
          // Since some object need to scale, we never allow the initial size become 0
          val (sizeToSetX, sizeToSetY, sizeToSetZ) = (checkSize(size(1)),
                                                      checkSize(size(0)),
                                                      checkSize(size(2)))
          app.objectsToDelete += app.objects(objectKey)
          app.scaleFactors -= app.objects(objectKey)
          app.objects(objectKey) = app.drawBox(abs(sizeToSetX), abs(sizeToSetY),
                                               abs(sizeToSetZ))
          transObject = app.objects(objectKey)
          setScaleFactors(Array(sizeToSetY,sizeToSetZ,sizeToSetX), transObject,
                          name, app.scaleFactors)
          objID = app.objects(objectKey).getName // refresh the object ID
          transObject.setShadingMode(Object3D.SHADING_FAKED_FLAT)
          if (anaglyphObject != null) {
            anaglyphObject = transAnaglyphObject(transObject, objectKey)
            newAnaglyphObject = true
          }
        } else if (checkResizeable(size) && needResize) {
          // just need to change the size
          if (app.objects.contains(objectKey) && transObject != null) {
            val (sizeToSetX, sizeToSetY, sizeToSetZ) = (checkSize(size(1)),
                                                        checkSize(size(2)),
                                                        checkSize(size(0)))
            val factors = calculateResizeFactor(transObject, Array(sizeToSetZ,
                                      sizeToSetY, sizeToSetX), app.scaleFactors)
            setReSize(factors(0), factors(1), factors(2), transObject)
            if (anaglyphObject != null)
              setReSize(factors(0), factors(1), factors(2), anaglyphObject)
          }
        }
      case "Cylinder" =>
        // the type has been changed, delete the old object and create a new one
        if (lastFrameName != name && checkResizeable(size)
          && checkResizeable(angle)) {
          // change the object in
          val (sizeToSetR, sizeToSetS) = (checkSize(size(0)), checkSize(size(1)))
          app.objectsToDelete += app.objects(objectKey)
          app.scaleFactors -= app.objects(objectKey)
          app.objects(objectKey) = Primitives.getCylinder(20, abs(sizeToSetR.toFloat),
                                    abs(sizeToSetS / (2 * sizeToSetR)).toFloat)
          transObject = app.objects(objectKey)
          setScaleFactors(size, transObject, name, app.scaleFactors)
          objID = app.objects(objectKey).getName // refresh the object ID
          transObject.setShadingMode(Object3D.SHADING_FAKED_FLAT)
          if (anaglyphObject != null) {
            anaglyphObject = transAnaglyphObject(transObject, objectKey)
            newAnaglyphObject = true
          }
        } else if (checkResizeable(size) && needResize) {
          if (app.objects.contains(objectKey) && transObject != null) {
            // just need to change the size
            val (sizeToSetR, sizeToSetS) = (checkSize(size(0)), checkSize(size(1)))
            val factors = calculateResizeFactor(transObject, Array(sizeToSetR,
                                      sizeToSetS, sizeToSetR), app.scaleFactors)
            setReSize(factors(0), factors(1), factors(2), transObject)
            if (anaglyphObject != null)
              setReSize(factors(0), factors(1), factors(2), anaglyphObject)
          }
        }
      case "Cone" =>
        // the type has been changed, delete the old object and create a new one
        if (lastFrameName != name && checkResizeable(size)
          && checkResizeable(angle)) {
          // change the object in
          val (sizeToSetR, sizeToSetS) = (checkSize(size(0)), checkSize(size(1)))
          app.objectsToDelete += app.objects(objectKey)
          app.scaleFactors -= app.objects(objectKey)
          app.objects(objectKey) = Primitives.getCone(20, abs(sizeToSetR.toFloat),
            abs(sizeToSetS / (sizeToSetR * 2)).toFloat)
          transObject = app.objects(objectKey)
          setScaleFactors(size, transObject, name, app.scaleFactors)
          objID = app.objects(objectKey).getName// refresh the object ID
          if (anaglyphObject != null) {
            anaglyphObject = transAnaglyphObject(transObject, objectKey)
            newAnaglyphObject = true
          }
        } else if (checkResizeable(size) && needResize) {
          if (app.objects.contains(objectKey) && transObject != null) {
            // just need to change the size
            val (sizeToSetR, sizeToSetS) = (checkSize(size(0)), checkSize(size(1)))
            val factors = calculateResizeFactor(transObject, Array(sizeToSetR,
                                      sizeToSetS, sizeToSetR), app.scaleFactors)
            setReSize(factors(0), factors(1), factors(2), transObject)
            if (anaglyphObject != null)
              setReSize(factors(0), factors(1), factors(2), anaglyphObject)
          }
        }
      case "Sphere" =>
        // the type has been changed, delete the old object and create a new one
        if (lastFrameName != name && checkResizeable(size)
          && checkResizeable(angle)) {
          // change the object in
          val sizeToSetR = checkSize(size(0))
          app.objectsToDelete += app.objects(objectKey)
          app.scaleFactors -= app.objects(objectKey)
          app.objects(objectKey) = Primitives.getSphere(10, abs(sizeToSetR.toFloat))
          transObject = app.objects(objectKey)
          setScaleFactors(size, transObject, name, app.scaleFactors)
          objID = app.objects(objectKey).getName // refresh the object ID
          if (anaglyphObject != null) {
            anaglyphObject = transAnaglyphObject(transObject, objectKey)
            newAnaglyphObject = true
          }
        } else if (checkResizeable(size) && needResize) {
          if (app.objects.contains(objectKey) && transObject != null) {
            // just need to change the size
            val sizeToSetR = checkSize(size(0))
            val factors = calculateResizeFactor(transObject, Array(sizeToSetR,
                                      sizeToSetR, sizeToSetR), app.scaleFactors)
            setReSize(factors(0), factors(1), factors(2), transObject)
            if (anaglyphObject != null)
              setReSize(factors(0), factors(1), factors(2), anaglyphObject)
          }
        }
      case "Text" =>
        val lastFrameContent =
          if (getLastContent(objectKey, lastValueList) == false) "new" + name
          else getLastContent(objectKey, lastValueList)
        // the type has been changed, delete the old object and create a new one
        if ((lastFrameName != name || lastFrameContent != text) && text != ""
          && checkResizeable(size) && checkResizeable(angle)) {
          // change the object in
          val sizeToSetR = checkSize(size(0))
          app.objectsToDelete += app.objects(objectKey)
          app.scaleFactors -= app.objects(objectKey)
          app.objects(objectKey) = buildText(text, sizeToSetR)
          transObject = app.objects(objectKey)
          setScaleFactors(size, transObject, name, app.scaleFactors)
          objID = app.objects(objectKey).getName // refresh the object ID
          if (anaglyphObject != null) {
            anaglyphObject = transAnaglyphObject(transObject, objectKey)
            newAnaglyphObject = true
          }
        } else if (checkResizeable(size) && needResize) {
          if (app.objects.contains(objectKey) && transObject != null) {
            // just need to change the size
            val sizeToSetR = checkSize(size(0))
            val factors = calculateResizeFactor(transObject, Array(sizeToSetR,
                                      sizeToSetR, sizeToSetR), app.scaleFactors)
            setReSize(factors(0), factors(1), factors(2), transObject)
            if (anaglyphObject != null)
              setReSize(factors(0), factors(1), factors(2), anaglyphObject)
          }
        }
      case "OBJ" =>
        val lastFrameContent =
          if (getLastContent(objectKey, lastValueList) == false) "new" + name
          else getLastContent(objectKey, lastValueList)
        // the type has been changed, we need to delete the old object and create a one
        if ((lastFrameName != name || lastFrameContent != path) && path != ""
          && checkResizeable(size) && checkResizeable(angle)) {
          // change the object in
          val sizeToSetR = checkSize(size(0) / 132)
          app.objectsToDelete += app.objects(objectKey)
          app.scaleFactors -= app.objects(objectKey)
          app.objects(objectKey) = loadObj(path, sizeToSetR)
          transObject = app.objects(objectKey)
          setScaleFactors(size, transObject, name, app.scaleFactors)
          objID = app.objects(objectKey).getName // refresh the object ID
          if (anaglyphObject != null) {
            anaglyphObject = transAnaglyphObject(transObject, objectKey)
            newAnaglyphObject = true
          }
        } else if (checkResizeable(size) && needResize) {
          if (app.objects.contains(objectKey) && transObject != null) {
            // just need to change the size
            val sizeToSetR = checkSize(size(0) / 132)
            val factors = calculateResizeFactor(transObject, Array(sizeToSetR,
                                      sizeToSetR, sizeToSetR), app.scaleFactors)
            setReSize(factors(0), factors(1), factors(2), transObject)
            if (anaglyphObject != null)
              setReSize(factors(0), factors(1), factors(2), anaglyphObject)
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
      if (anaglyphObject != null) {
        val colorAverage = (color(0) + color(1) + color(2)) / 3
        transObject.setTransparencyMode(Object3D.TRANSPARENCY_MODE_ADD)
        transObject.setTransparency(0)
        setColor(transObject, Array(colorAverage, colorAverage, colorAverage))
        anaglyphObject.setTransparencyMode(Object3D.TRANSPARENCY_MODE_ADD)
        anaglyphObject.setTransparency(0)
        if (TextureManager.getInstance().containsTexture("ConeRed"))
          transObject.setTexture("ConeRed")
        // reset the color for the object
        setColor(anaglyphObject, Array(colorAverage, colorAverage, colorAverage))
        // rotate the object
        if (checkResizeable(angle))
          app.rotateObject(anaglyphObject, angle, name, null)
        // calculate the transVector for the object and translate it
        anaglyphObject.translate(transVector)
        if (newAnaglyphObject)
          anaglyphObject.translate(app.eyeOffSet, 0, 0)
        if (TextureManager.getInstance().containsTexture("ConeCyan"))
          anaglyphObject.setTexture("ConeCyan")
        CustomObject3D.partialBuild(anaglyphObject, name == "Text")
      }
      CustomObject3D.partialBuild(transObject, name == "Text")
    }
  }

  def matchingObject(c: (CId, Int), valueList: List[_], currentFrame: Int) = {
    /* Find the corresponding index of the object */
    /* Get the 3D information of the object at that frame	*/
    val (name: String, position: Array[Double], size: Array[Double],
    color: Array[Double], angle: Array[Double]) =
      (valueList.head, valueList(1), valueList(2), valueList(3), valueList(4))
    val (text: String, path: String) =
      (if (name == "Text") valueList(5) else " ",
        if (name == "OBJ")  valueList(5) else " ")

    val newObject =
      if (checkResizeable(size) && checkResizeable(angle)) name match {
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
      } else null

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
      if (acumen.ui.App.ui.getStartAnaglyph) {
        newObject.setTransparencyMode(Object3D.TRANSPARENCY_MODE_ADD)
        newObject.setTransparency(0)
        val colorAverage = (color(0) + color(1) + color(2)) / 3
        setColor(newObject, Array(colorAverage, colorAverage, colorAverage))
        val anaglyphObject = new Object3D(newObject, false)
        if (TextureManager.getInstance().containsTexture("ConeRed"))
          newObject.setTexture("ConeRed")
        if (TextureManager.getInstance().containsTexture("ConeCyan"))
          anaglyphObject.setTexture("ConeCyan")
        anaglyphObject.translate(app.eyeOffSet, 0, 0)
        app.objectsCopy += c -> anaglyphObject
        if (name != "Text")
          CustomObject3D.partialBuild(anaglyphObject, false)
        else {
          anaglyphObject.setRotationPivot(new SimpleVector(0,0,0))
          anaglyphObject.setCenter(new SimpleVector(0,0,0))
          CustomObject3D.partialBuild(anaglyphObject, true)
        }
      }
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

  // initialize for anaglyph
  def initAnaglyph() = {
    val imageRedChannel = app.isolateColorChannel(app.imageOrig, "red")
    val imageCyanChannel = app.isolateColorChannel(app.imageOrig, "cyan")
    val imageGreenChannel = app.isolateColorChannel(app.imageOrig, "green")

    val textureManager = TextureManager.getInstance
    //red texture
    if (!textureManager.containsTexture("ConeRed"))
      textureManager.addTexture("ConeRed", new Texture(imageRedChannel))
    //cyan texture
    if (!textureManager.containsTexture("ConeCyan"))
      textureManager.addTexture("ConeCyan", new Texture(imageCyanChannel))
    if (!textureManager.containsTexture("ConeGreen"))
    //green texture
      textureManager.addTexture("ConeGreen", new Texture(imageGreenChannel))
  }

  def transAnaglyphObject(oriObject: Object3D, obKey: (CId, Int)): Object3D = {
    app.objectsToDelete += app.objectsCopy(obKey)
    app.objectsCopy(obKey) = new Object3D(oriObject, false)
    val anaglyphObject = app.objectsCopy(obKey)
    anaglyphObject
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
            if (!app.waitingPaint)
              renderCurrentFrame()
            currentFrame = setFrameNumber("go", currentFrame)
          }
        case "set frame" =>
          setFrameDone = false
          currentFrame = setFrameNumber("set frame", currentFrame)
          if (totalFrames > 0 && currentFrame <= totalFrames
            && !app.waitingPaint) {
            val percentage = currentFrame * 100 / totalFrames
            slider.setTime(percentage / 100f * endTime)
            renderCurrentFrame()
            setFrameDone = true
          }
        case "real time render" =>
          if (!app.waitingPaint)
            renderFrameInRealTime()
      }
    }
  }

  def setFrameNumber (setMode: String, lastFrameNumber: Int): Int = {
    var newFrameNumber = 0
    if (setMode == "go" && playSpeed >= 1)
      newFrameNumber = lastFrameNumber + 1 * playSpeed.toInt
    else if (setMode == "go" && playSpeed < 1)
      newFrameNumber = lastFrameNumber + 1
    else newFrameNumber = slider.bar.value * totalFrames / 100
    newFrameNumber
  }

  // Reactions to the mouse events
  reactions += {
    case e: scala.swing.event.MouseDragged =>
      val curSystemTime = System.currentTimeMillis()
      if (curSystemTime > lastSetFrameTime + mouseSleepTime
        && setFrameDone) {
        receiver ! "set frame"
        lastSetFrameTime = System.currentTimeMillis()
      }
  }

  /**
   * Delete an 3D-object from scene
   */
  def deleteObj(c: (CId, Int)) {
    if (app.objects.contains(c)) {
      if (app.world.getObjectByName(app.objects(c).getName) != null) {
        app.objectsToDelete += app.objects(c)
        if (acumen.ui.App.ui.getStartAnaglyph)
          app.objectsToDelete += app.objectsCopy(c)
      }
      app.objectsCopy -= c
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

  // Load external object files
  def loadObj(path: String, size: Double): Object3D = {
    //read in the geometry information from the data file
    val _3DFolderPath = (_3DBasePath + File.separator + path).split("\\.")(0)
    val _3DFolder = new File(_3DFolderPath) // the folder of the objects' files
    // load all the texture files
    val objectFiles =
      if (_3DFolder.canExecute) _3DFolder.listFiles()
      else throw _3DLoadFileError(path)
    val objFile = objectFiles.filter(file => file.getName.endsWith(".3ds") || file.getName.endsWith(".obj"))
    val textureFiles = objectFiles.filter(file => file.getName.endsWith(".png") || file.getName.endsWith(".jpg"))
    val mtlFile = objectFiles.filter(file => file.getName.endsWith(".mtl"))
    // make sure there's only one object file
    if (objFile.isEmpty) throw _3DLoadFileError("Can not find object file for " + path)
    else if (objFile.length > 1) throw _3DLoadFileError("Duplicate object files for " + path)
    // load OBJ file
    val resultObject =
      if (objFile.head.getName.endsWith(".obj")) {
        // make sure there's only one material file
        if (mtlFile.isEmpty) throw _3DLoadFileError("Can not find material file for " + path)
        else if (mtlFile.length > 1) throw _3DLoadFileError("Duplicate material files for " + path)
        else {
          for (texture <- textureFiles) {
            if (!TextureManager.getInstance().containsTexture(texture.getName))
              TextureManager.getInstance().addTexture(texture.getName, new Texture(texture.toString))
          }
        }
        Object3D.mergeAll(Loader.loadOBJ(objFile.head.toString, mtlFile.head.toString, size.toFloat))
      }
      // load 3ds file
      else {
        for (texture <- textureFiles) {
          if (!TextureManager.getInstance().containsTexture(texture.getName))
            TextureManager.getInstance().addTexture(texture.getName, new Texture(texture.toString))
        }
        Object3D.mergeAll(Loader.load3DS(objFile.head.toString, size.toFloat))
      }
    resultObject
  }

  def setColor(objectToSet: Object3D, colorRGB: Array[Double]) =
    objectToSet.setAdditionalColor(new Color
    ( max(0, min(255, colorRGB(0) * 255)).toInt
    , max(0, min(255, colorRGB(1) * 255)).toInt
    , max(0, min(255, colorRGB(2) * 255)).toInt ))

}

// Transparent box
class setGlass(color: Color, objectA: Object3D, transparancy: Int) {
  objectA.setTransparencyMode(Object3D.TRANSPARENCY_MODE_ADD) //TRANSPARENCY_MODE_DEFAULT
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
class coAxis(characters: Map[Char, Object3D], mainBox: Object3D) {
  val cylinders: Array[Object3D] = new Array[Object3D](10)
  val mainbox = mainBox
  // add the main box
  mainbox.setShadingMode(Object3D.SHADING_FAKED_FLAT)
  new setGlass(new Color(50, 50, 50), mainbox, 0)
  cylinders(0) = mainbox
  for (x <- 1 until 4)
    cylinders(x) = Primitives.getCylinder(12, 0.01f, 50f)
  for (x <- 4 until 7)
    cylinders(x) = Primitives.getCone(12, 0.05f, 2f)
  cylinders(9) = new Object3D(characters('y'), false)
  cylinders(8) = new Object3D(characters('x'), false)
  cylinders(7) = new Object3D(characters('z'), false)
  for (i <- 7 to 9) {
    cylinders(i).setRotationPivot(new SimpleVector(0,0,0))
    cylinders(i).setCenter(new SimpleVector(0,0,0))
    cylinders(i).scale(0.6f)
    cylinders(i).rotateY(Pi.toFloat)
    cylinders(i).rotateMesh()
    cylinders(i).setBillboarding(Object3D.BILLBOARDING_ENABLED)
  }

  for (i <- 1 until cylinders.length)
    new setGlass( if      (i % 3 == 1) Color.BLUE
                  else if (i % 3 == 2) Color.RED
                  else                 Color.GREEN
                , cylinders(i), -1)
  cylinders(1).translate(0f, -0.5f, 0f)       // z axis cylinder
  cylinders(4).translate(0f, -1f, 0f)         // z axis cone
  cylinders(7).translate(-0.05f, -1f, 0f)     // z text
  cylinders(2).rotateZ(0.5f * -Pi.toFloat)    // x axis cylinder
  cylinders(2).translate(-0.5f, 0f, 0f)
  cylinders(5).translate(-1f, 0f, 0f)         // x axis cone
  cylinders(5).rotateZ(0.5f * Pi.toFloat)
  cylinders(8).translate(-1f, -0.05f, 0f)     // x text
  cylinders(3).rotateX(-0.5f * Pi.toFloat)    // y axis cylinder
  cylinders(3).translate(0f, 0f, -0.5f)
  cylinders(6).translate(0f, 0f, -1f)         // y axis cone
  cylinders(6).rotateX(-0.5f * Pi.toFloat)
  cylinders(9).translate(0f, -0.05f, -1f)     // y text
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

