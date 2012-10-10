package acumen
package ui

import Errors._
import com.sun.j3d.utils.universe._
import com.sun.j3d.utils.geometry._
import com.sun.j3d.utils.behaviors.mouse._

import javax.media.j3d._
import javax.media.j3d.Group
import javax.media.j3d.ColoringAttributes
import javax.media.j3d.BranchGroup
import javax.vecmath.{ AxisAngle4d, Color3f, Point3d, Vector3d, Point3f, Vector3f }
import java.lang.Thread
import javax.media.j3d.Font3D
import javax.media.j3d.FontExtrusion
import java.awt.Font

import scala.collection.mutable.Map
import scala.collection.mutable.Buffer
import scala.actors._
import scala.swing.BorderPanel
import scala.math._
import scala.swing.Publisher
import swing.event._
import swing._

/* 3D visualization panel */
class ThreeDView() extends BorderPanel {
  /* Each object belongs to an TG and an BG */
  var trans = Map[List[_], TransformGroup]()
  var branches = Map[List[_], BranchGroup]() // List[_] = [CID: class name, integer; instance number];
  // FIXME: List[_] to class object

  var scene = new BranchGroup()
  val config = SimpleUniverse.getPreferredConfiguration()
  var jCanvas = new Canvas3D(config)
  var canvas = new SCanvas3D(jCanvas)
  var u = new SimpleUniverse(jCanvas)
  var backGround = new BranchGroup()

  /* Create a canvas for 3D-rendering */
  def init(): SCanvas3D = {
    scene.setCapability(BranchGroup.ALLOW_DETACH);
    scene.setCapability(javax.media.j3d.Group.ALLOW_CHILDREN_EXTEND);
    scene.setCapability(javax.media.j3d.Group.ALLOW_CHILDREN_READ);
    scene.setCapability(javax.media.j3d.Group.ALLOW_CHILDREN_WRITE);
    backGround.setCapability(BranchGroup.ALLOW_DETACH);
    backGround addChild new Axis()
    backGround addChild new MainBox()
    reset
    canvas = new SCanvas3D(jCanvas)
    canvas
  }

  def defaultView(): Transform3D = {
    val tr = new Transform3D()
    tr.rotX(-Pi * 3.5 / 8)
    val scale = new Transform3D()
    scale.setScale(new Vector3d(0.35f, 0.35f, 0.35f))
    val tr2 = new Transform3D()
    tr2.rotZ(Pi * 0.1)
    tr.mul(scale)
    tr.mul(tr2)
    tr
  }
  /**
   * Reset the scene, delete every object, and
   * adjust the viewing position back to the default position
   */
  def reset() {
    u.cleanup()
    u = new SimpleUniverse(jCanvas)
    val sphere = new BoundingSphere(new Point3d(0, 0, 0), 1000)
    var sceneRoot = createSceneGraph()
    sceneRoot.setCapability(BranchGroup.ALLOW_DETACH)
    /* Transorm group for mouse rotation */
    val objRotate = new TransformGroup(new Transform3D())
    objRotate.setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE)
    objRotate.setCapability(TransformGroup.ALLOW_TRANSFORM_READ)
    val myMouseRotate = new MouseRotate(objRotate)
    myMouseRotate.setSchedulingBounds(sphere)

    /* Transform group for mouse zoom */
    val objZoom = new TransformGroup()
    objZoom.setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE)
    objZoom.setCapability(TransformGroup.ALLOW_TRANSFORM_READ)
    val myMouseZoom = new MouseZoom(objZoom)
    myMouseZoom.setSchedulingBounds(sphere)

    var sceneRoot1 = new BranchGroup()
    val transRoot = new TransformGroup(defaultView());
    scene.detach
    scene = new BranchGroup()
    scene.setCapability(javax.media.j3d.Group.ALLOW_CHILDREN_EXTEND);
    scene.setCapability(javax.media.j3d.Group.ALLOW_CHILDREN_READ);
    scene.setCapability(javax.media.j3d.Group.ALLOW_CHILDREN_WRITE);
    scene.setCapability(BranchGroup.ALLOW_DETACH);
    backGround.detach
    backGround = new BranchGroup()
    backGround.setCapability(BranchGroup.ALLOW_DETACH);
    backGround addChild new Axis()
    backGround addChild new MainBox()

    scene addChild backGround
    sceneRoot addChild scene
    transRoot addChild sceneRoot
    objRotate addChild transRoot
    objZoom addChild objRotate
    sceneRoot1 addChild objZoom
    sceneRoot1 addChild myMouseRotate
    sceneRoot1 addChild myMouseZoom
    u.getViewingPlatform().setNominalViewingTransform();
    u.addBranchGraph(sceneRoot1);
    u.getViewer.getView.setMinimumFrameCycleTime(1) // Fatest rendering
  }

  // Create the scene
  def createSceneGraph(): BranchGroup = {
    var Root = new BranchGroup();
    val sphere = new BoundingSphere(new Point3d(0, 0, 0), 1000)
    // Background color  
    val b = new javax.media.j3d.Background(0.8f, 0.8f, 0.8f)
    b.setApplicationBounds(sphere)
    // Ambient light
    val lightA = new AmbientLight(new Color3f(0.3f, 0.3f, 0.3f));
    lightA.setInfluencingBounds(sphere)
    // Directional light
    val lightD1 = new DirectionalLight()
    lightD1.setDirection(-4.076f, -1.005f, -5.904f)
    lightD1.setColor(new Color3f(1f, 1f, 1f))
    lightD1.setInfluencingBounds(sphere)
    Root.addChild(lightD1)
    val lightD3 = new DirectionalLight()
    lightD1.setDirection(-4.076f, 1.005f, -5.904f)
    lightD1.setColor(new Color3f(1f, 1f, 1f))
    lightD1.setInfluencingBounds(sphere)
    Root.addChild(lightD3)
    val lightD2 = new DirectionalLight()
    lightD2.setDirection(1, 1, -1)
    lightD2.setColor(new Color3f(0.2f, 0.2f, 0.2f))
    lightD2.setInfluencingBounds(sphere)
    Root.addChild(lightD2)
    var tr1 = new Transform3D()
    val objOrig = new TransformGroup(tr1)
    objOrig.setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE);
    objOrig.setCapability(TransformGroup.ALLOW_TRANSFORM_READ);
    var tr2 = new Transform3D()

    Root addChild objOrig
    Root addChild lightA
    Root addChild b
    Root
  }
  /* Turn on the axis and the transparent box */
  def axisOn: Unit = {
    /* Check if the axis is off now */
    if (scene.indexOfChild(backGround) == -1)
      scene addChild (backGround)
  }
  def axisOff: Unit = scene removeChild (backGround)
  /* Clean up the scene and canvas */
  def exit() {
    u.getViewer.getView.removeAllCanvas3Ds()
    u.getCanvas.stopRenderer()
    u.cleanup()
    canvas = null
    jCanvas = null
    u = null
  }
  /* Add an object to scene */
  def add(b: BranchGroup) {
    scene.addChild(b)
  }
  /* delete an object to scene */
  def delete(b: BranchGroup) {
    if (scene.indexOfChild(b) != -1)
      scene.removeChild(b)
  }
  def deleteAll = scene.removeAllChildren()
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

/* 3D renderer */
class _3DDisplay(app: ThreeDView, slider: Slider3d,
                 _3DDateBuffer: Map[CId, Map[Int, scala.collection.mutable.Buffer[List[_]]]],
                 lastFrame1: Double, endTime: Double) extends Publisher with Actor 
{
  var lastLook = Map[List[_], List[_]]() // Store the size and color of each object
  var currentFrame = 0 // FrameNumber
  var totalFrames = 2
  var lastFrame = 2.0
  var pause = false
  var destroy = false
  lastFrame = lastFrame1
  totalFrames = lastFrame.toInt
  val startFrameNumber = 2;

  def stop {
    if (app.scene.numChildren() != 0)
      app.scene.removeAllChildren()
  }

  def renderCurrentFrame() {
    for ((id, map) <- _3DDateBuffer) { // acumen objects
      for ((objectNumber, buffer) <- map) { // 3d objects within
        if (firstFrame(buffer) <= currentFrame && lastFrame(buffer) >= currentFrame) {
          if (!app.branches.contains(List(id, objectNumber))) {
            addToBranches(app.branches, app.trans, List(id, objectNumber))
            lastLook += List(id, objectNumber) -> List(bufferSize(buffer.head),
              bufferColor(buffer.head), bufferType(buffer.head))
            app.add(addObj(List(id, objectNumber), buffer, currentFrame))
          }
          val frame = (currentFrame - bufferFrame(buffer.head)).toInt
          transformObject(List(id, objectNumber), app.trans, buffer, currentFrame)
          if (frame >= 0 && frame < buffer.size)
            checkLook(List(id, objectNumber), lastLook, buffer, currentFrame, buffer(frame))
        } else {
          deleteObj(List(id, objectNumber))
        }
      }
    }
  }
  // Main execution loop
  def act() {
    loopWhile(!destroy) {
      if (destroy)
        exit
      react {
        case "go" => {
          renderCurrentFrame
          if (currentFrame == totalFrames) // Animation is over
            emitProgress(100)
          if (totalFrames > 0)
            emitProgress((currentFrame * 100 / totalFrames).toInt)
          if (currentFrame < totalFrames)
            currentFrame += 1
        }
      }
    }
  }

  // Reactions to the mouse events
  reactions += {
    case e: MouseDragged => {
      currentFrame = (slider.bar.value) * totalFrames / 100
      emitProgress(slider.bar.value.toInt)
      //publish(Playing3d())
      if (currentFrame < 2)
        currentFrame = startFrameNumber;
      if (currentFrame > totalFrames)
        currentFrame = totalFrames
      if (pause == true)
        renderCurrentFrame
    }
  }

  def bufferFrame(list: List[_]): Int = {
    list(5) match {
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
  // Return the first frame number of the object
  def firstFrame(buffer: scala.collection.mutable.Buffer[List[_]]): Int = {
    buffer.head(5) match {
      case first: Int => first
      case _ => throw ShouldNeverHappen()
    }
  }
  def lastFrame(buffer: scala.collection.mutable.Buffer[List[_]]): Int = {
    buffer.last(5) match {
      case last: Int => last
      case _ => throw ShouldNeverHappen()
    }
  }
  /**
   * Every object belongs to a corresponding branchGroup and a transformGroup
   * and key is a list of the class name and the object number.
   *
   */
  def addToBranches(branches: Map[List[_], BranchGroup],
    trans: Map[List[_], TransformGroup], key: List[_]) {
    var tr = new Transform3D()
    val id = key
    trans += (id -> new TransformGroup(tr))
    branches += (id -> new BranchGroup())
    branches(key).setCapability(BranchGroup.ALLOW_DETACH);
    trans(key).setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE);
    trans(key).setCapability(TransformGroup.ALLOW_TRANSFORM_READ);
  }
  /**
   * Moving and rotating the object
   */
  def transformObject(id: List[_], trans: Map[List[_], TransformGroup],
    buffer: scala.collection.mutable.Buffer[List[_]], currentFrame: Int) {
    var tempPosition = Array[Double](0.0, 0.0, 0.0)
    var tempAngle = Array[Double](0.0, 0.0, 0.0)
    /* Find the corresponding index of the object */
    val index = (currentFrame - bufferFrame(buffer.head)).toInt
    if (index >= 0 && index < buffer.size) {
      /* The position of the object at that frame	*/
      tempPosition = bufferPosition(buffer(index))
      /* The angle of the object at that frame */
      tempAngle = bufferAngle(buffer(index))
    }
    var transform = new Transform3D()
    var transAngle = new Transform3D()
    var transAngleX = new Transform3D()
    var transAngleY = new Transform3D()
    var transAngleZ = new Transform3D()
    transAngleX.rotZ(tempAngle(2))
    transAngleY.rotY(-tempAngle(1))
    transAngleZ.rotX(tempAngle(0))
    transAngle.mul(transAngleX)
    transAngle.mul(transAngleY)
    transAngle.mul(transAngleZ)
    transform.mul(transAngle)
    transform.setTranslation(new Vector3f((tempPosition(0)).toFloat,
      (tempPosition(1)).toFloat, (tempPosition(2)).toFloat))
    if (trans.contains(id))
      trans(id).setTransform(transform)
  }

  /**
   * Check if the object's look has changed(size, color, type)
   * If so, delete it and create a new one
   */
  def checkLook(id: List[_], lastLook: Map[List[_], List[_]],
    buffer: scala.collection.mutable.Buffer[List[_]], currentFrame: Int, frame: List[_]) {
    if (lastLook.contains(id)) {
      if (lastLook(id)(0) != bufferSize(frame) ||
        lastLook(id)(1) != bufferColor(frame) ||
        lastLook(id)(2) != bufferType(frame)) {
        deleteObj(id)
        app.add(addObj(id, buffer, currentFrame))
      }
    }
    val key = id
    lastLook -= key // Update last look
    lastLook += key -> List(bufferSize(frame), bufferColor(frame), bufferType(frame))
  }

  // Update the slider value
  private def emitProgress(p: Int) = Acumen.actor ! Progress3d(p)
  // Fix this code 
  var f3d = new Font3D(new Font("", Font.PLAIN, 1),
    new FontExtrusion());
  var text3d = new Text3D(f3d);
  text3d.setString("a");
  // 
  def addText(tg: TransformGroup, text: String, size: Int, color: List[Double]) {
    // Font for text	
    f3d = new Font3D(new Font("", Font.PLAIN, size),
      new FontExtrusion());
    text3d = new Text3D(f3d);
    text3d.setString(text);
    val color3d = new Color3f(color(0).toFloat, color(1).toFloat, color(2).toFloat);
    var a = new Appearance();
    var m = new Material(color3d, color3d, color3d, color3d, 80.0f);
    m.setLightingEnable(true);
    a.setMaterial(m);
    var sh = new Shape3D();
    sh.setGeometry(text3d);
    sh.setAppearance(a);
    tg addChild sh
  }

  /**
   * Delete an 3D-object from scene
   */
  def deleteObj(c: List[_]) {
    if (app.branches.contains(c) && app.trans.contains(c)) {
      app.delete(app.branches(c))
      app.branches -= c
      app.trans -= c
    }
  }
  /**
   * Add an 3D-object to the scene
   */
  def addObj(c: List[_], buffer: scala.collection.mutable.Buffer[List[_]],
    currentFrame: Int): BranchGroup = {
    var color = List[Double](1.0, 1.0, 1.0)
    var size = List[Double](1.0)
    var name = " "
    val index = (currentFrame - bufferFrame(buffer.head)).toInt
    if (index >= 0 && index < buffer.size) {
      val list = buffer(index);
      color = bufferColor(list) // Get the color and size of the object
      size = bufferSize(list)
      name = bufferType(list)
    }
    app.trans -= c
    app.branches -= c

    app.trans += c.toList -> new TransformGroup()
    app.trans(c).setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE)
    app.trans(c).setCapability(TransformGroup.ALLOW_TRANSFORM_READ)
    app.branches += c.toList -> new BranchGroup()
    app.branches(c).setCapability(BranchGroup.ALLOW_DETACH)
    app.branches(c).setCapability(javax.media.j3d.Group.ALLOW_CHILDREN_EXTEND)
    app.branches(c).setCapability(javax.media.j3d.Group.ALLOW_CHILDREN_WRITE)

    val mat = new Material() //  Define the material of the objcet
    mat.setAmbientColor(new Color3f(color(0).toFloat, color(1).toFloat, color(2).toFloat))
    mat.setDiffuseColor(new Color3f(color(0).toFloat, color(1).toFloat, color(2).toFloat))
    mat.setShininess(100);
    val ap = new Appearance()
    ap.setMaterial(mat)

    app.trans(c) match {
      case trans: Group => {
        name match {
          case "Box" => {
            app.trans(c).addChild(new Box(abs((size(0) * 0.5).toFloat), abs((size(1) * 0.5).toFloat),
              abs((size(2) * 0.5).toFloat), ap));
          }
          case "Cylinder" => {
            app.trans(c).addChild(new Cylinder(abs(size(0).toFloat), abs(size(1).toFloat), ap));
          }
          case "Cone" => {
            app.trans(c).addChild(new Cone(abs(size(0).toFloat), abs(size(1).toFloat), ap));
          }
          case "Sphere" =>
            app.trans(c).addChild(new Sphere(abs(size(0).toFloat),
              com.sun.j3d.utils.geometry.Primitive.GENERATE_NORMALS, 30, ap));
          case text: String => { addText(app.trans(c), text, size(0).toInt, color); }

          case _ => throw ShouldNeverHappen()
        }
      }
      case _ => throw ShouldNeverHappen()
    }
    // Once we added the object, we should also move the object to the position at that time
    if (app.branches.contains(c) && app.trans.contains(c)) {
      transformObject(c, app.trans, buffer, currentFrame);
      app.branches(c).addChild(app.trans(c));
    }
    return app.branches(c);
  }

}

// Transparent box 
object Glass extends Appearance {
  val mat = new Material()
  mat.setAmbientColor(new Color3f(1, 1, 1))
  mat.setDiffuseColor(new Color3f(1, 1, 1))

  val pa = new PolygonAttributes()
  pa.setCullFace(PolygonAttributes.CULL_NONE)

  val ca = new ColoringAttributes()
  ca.setShadeModel(ColoringAttributes.SHADE_FLAT)

  val ap = new Appearance()
  setMaterial(mat)
  setColoringAttributes(ca)
  setTransparencyAttributes(new TransparencyAttributes(TransparencyAttributes.NICEST, 0.9f))
  setPolygonAttributes(pa)
}
// The box
class MainBox extends TransformGroup {
  val tr = new Transform3D()
  val box = new Box(0.5f, 0.5f, 0.5f, Primitive.GENERATE_NORMALS, Glass)
  this.addChild(box)
}

class Axis extends Shape3D {

  val ap = new Appearance()
  ap.setLineAttributes(new LineAttributes(1f, LineAttributes.PATTERN_SOLID, true))

  setGeometry(createGeometry)
  setAppearance(Glass)

  private def createGeometry: Geometry = {
    val line = new LineArray(6, GeometryArray.COORDINATES | GeometryArray.COLOR_3)

    val red = new Color3f(0.5f, 0, 0)
    val green = new Color3f(0, 0.5f, 0)
    val blue = new Color3f(0, 0, 0.5f)

    line.setCoordinate(0, new Point3f(-10, 0, 0))
    line.setCoordinate(1, new Point3f(10, 0, 0))
    line.setColor(0, red)
    line.setColor(1, red)

    line.setCoordinate(2, new Point3f(0, -10, 0))
    line.setCoordinate(3, new Point3f(0, 10, 0))
    line.setColor(2, green)
    line.setColor(3, green)

    line.setCoordinate(4, new Point3f(0, 0, -10))
    line.setCoordinate(5, new Point3f(0, 0, 10))
    line.setColor(4, blue)
    line.setColor(5, blue)

    line
  }
}
  
