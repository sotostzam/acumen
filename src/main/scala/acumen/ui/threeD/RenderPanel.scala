package acumen.ui.threeD

import java.awt._
import java.awt.event._
import javax.swing._

import com.threed.jpct._
import acumen.render._

/**
 * Created by xufei on 9/18/14.
 */
class RenderPanel extends JPanel {

  // Main game-loop thread liveness:
  var alive = true
  // Set to true after everything finishes loading:
  var initialized = false

  val borderLayout = new BorderLayout()
  this.setLayout(borderLayout)

  val world = new World() // create a new world

  var camera = new Camera

  var cameraPos = new SimpleVector()

  var newMouseX = 1
  // mouse position x before dragging
  var newMouseY = 1
  // mouse position y before dragging
  var lastMouseX = 1
  // mouse position x after dragging
  var lastMouseY = 1
  // mouse position y after dragging
  var dragging = false

  // create a new buffer to draw on:
  var buffer = new FrameBuffer(640, 480, FrameBuffer.SAMPLINGMODE_NORMAL)

  val glFont = GLFont.getGLFont(new Font("Dialog", Font.PLAIN, 12))

  addComponentListener(new ComponentAdapter {
    override def componentResized(e: ComponentEvent) = {
      val c = e.getSource.asInstanceOf[Component]
      initBuffer(c.getWidth, c.getHeight)
    }
  })

  addMouseListener(new MouseAdapter {
    override def mousePressed(e: MouseEvent) = {
      if (dragging == false) {
        newMouseX = e.getX()
        newMouseY = e.getY()
      }
      dragging = true
    }

    override def mouseReleased(e: MouseEvent) = {
      dragging = false
    }
  })

  addMouseMotionListener(new MouseAdapter {
    override def mouseDragged(e: MouseEvent) = {
      if (dragging) {
        lastMouseX = e.getX()
        lastMouseY = e.getY()
        //        var cameraInitAngleX = 1.0
        //        var cameraInitAngleY = 1.0
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
        val deltaAlpha = (lastMouseY - newMouseY) * Math.PI / 5000
        val deltaTheta = (newMouseX - lastMouseX) * Math.PI / 7000
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
      }
      camera.lookAt(new SimpleVector(0, 0, 0))
      repaint()
    }
  })

  def moveCamera(dx: Double, dy: Double, dz: Double) = {
    val moveVector = new SimpleVector(dx, dy, dz)
    cameraPos.set(camera.getPosition.calcAdd(moveVector))
    camera.setPosition(cameraPos)
  }

  def initBuffer(bufferWidth: Int, bufferHeight: Int) = {
    buffer = new FrameBuffer(bufferWidth, bufferHeight, FrameBuffer.SAMPLINGMODE_NORMAL)
  }

  def renderPanel() = {

    //create a box
    val animatedTextureBox = Primitives.getCylinder(100, 1f, 1f)
    val boxMesh = animatedTextureBox.getMesh
    setCylConeSize(1f, 1f, boxMesh)

    world.addObject(animatedTextureBox)
    world.buildAllObjects()

    lookAt(animatedTextureBox) // camera faces towards the object
    letThereBeLight(); // create light sources for the scene

    initialized = true

    glFont.blitString(buffer, "Hello Acumen!", 50, 50, 10, Color.WHITE)
  }

  def transBoxAngle(transObject: Object3D) = {
    transObject.rotateY(-(Math.PI / 4).toFloat)
  }

  def drawBox(length: Double, width: Double, height: Double): Object3D = {
    val box = new Object3D(12)

    val upperLeftFront = new SimpleVector(-width / 2, -height / 2, -length / 2)
    val upperRightFront = new SimpleVector(width / 2, -height / 2, -length / 2)
    val lowerLeftFront = new SimpleVector(-width / 2, height / 2, -length / 2)
    val lowerRightFront = new SimpleVector(width / 2, height / 2, -length / 2)

    val upperLeftBack = new SimpleVector(-width / 2, -height / 2, length / 2)
    val upperRightBack = new SimpleVector(width / 2, -height / 2, length / 2)
    val lowerLeftBack = new SimpleVector(-width / 2, height / 2, length / 2)
    val lowerRightBack = new SimpleVector(width / 2, height / 2, length / 2)

    // Front
    box.addTriangle(upperLeftFront, 0, 0, lowerLeftFront, 0, 1, upperRightFront, 1, 0)
    box.addTriangle(upperRightFront, 1, 0, lowerLeftFront, 0, 1, lowerRightFront, 1, 1)

    // Back
    box.addTriangle(upperLeftBack, 0, 0, upperRightBack, 1, 0, lowerLeftBack, 0, 1)
    box.addTriangle(upperRightBack, 1, 0, lowerRightBack, 1, 1, lowerLeftBack, 0, 1)

    // Upper
    box.addTriangle(upperLeftBack, 0, 0, upperLeftFront, 0, 1, upperRightBack, 1, 0)
    box.addTriangle(upperRightBack, 1, 0, upperLeftFront, 0, 1, upperRightFront, 1, 1)

    // Lower
    box.addTriangle(lowerLeftBack, 0, 0, lowerRightBack, 1, 0, lowerLeftFront, 0, 1)
    box.addTriangle(lowerRightBack, 1, 0, lowerRightFront, 1, 1, lowerLeftFront, 0, 1)

    // Left
    box.addTriangle(upperLeftFront, 0, 0, upperLeftBack, 1, 0, lowerLeftFront, 0, 1)
    box.addTriangle(upperLeftBack, 1, 0, lowerLeftBack, 1, 1, lowerLeftFront, 0, 1)

    // Right
    box.addTriangle(upperRightFront, 0, 0, lowerRightFront, 0, 1, upperRightBack, 1, 0)
    box.addTriangle(upperRightBack, 1, 0, lowerRightFront, 0, 1, lowerRightBack, 1, 1)

    box
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
  def lookAt(obj: Object3D) = {
    camera = world.getCamera() // grab a handle to the camera
    camera.setPosition(0, 0, 10) // set its *relative* position
    camera.lookAt(obj.getTransformedCenter()) // look toward the object
  }

  // create some light sources for the scene
  def letThereBeLight() = {
    world.getLights().setOverbrightLighting(Lights.OVERBRIGHT_LIGHTING_DISABLED)
    world.getLights().setRGBScale(Lights.RGB_SCALE_2X)

    // Set the overall brightness of the world:
    world.setAmbientLight(50, 50, 50)

    // Create a main light-source:
    world.addLight(new SimpleVector(50, -50, 300), 20, 20, 20)
  }

  def setBoxSize(scaleLength: Float, scaleWidth: Float, scaleHeight: Float, planeMesh: Mesh) = {
    val demoControl = new ResizerBox(scaleWidth, scaleHeight, scaleLength)
    planeMesh.setVertexController(demoControl, IVertexController.PRESERVE_SOURCE_MESH)
    planeMesh.applyVertexController()
    planeMesh.removeVertexController()
  }

  def setCylConeSize(radiusFactor: Float, heightFactor: Float, planeMesh: Mesh) = {
    val demoControl = new ResizerCylCone(radiusFactor, heightFactor)
    planeMesh.setVertexController(demoControl, IVertexController.PRESERVE_SOURCE_MESH)
    planeMesh.applyVertexController()
    planeMesh.removeVertexController()
  }

}

