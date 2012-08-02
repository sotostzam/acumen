package acumen
package render

import interpreters.reference.Interpreter._

import util.Names._
import util.Filters._
import util.Canonical._

import java.applet.Applet
import java.awt.BorderLayout
import java.awt.Frame
import java.awt.event._
import com.sun.j3d.utils.applet.MainFrame
import com.sun.j3d.utils.universe._
import javax.media.j3d._
import javax.vecmath._

import com.sun.j3d.utils.behaviors.mouse._
import com.sun.j3d.utils.behaviors.keyboard._
import com.sun.j3d.utils.geometry._
import javax.media.j3d.ColoringAttributes

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import scala.math._

object ColorUtil {
  def getColor(id:CId) = new Color3f(0.95f,1.0f,0.2f)  
}

class PhysicalBehaviour(h:History, targetBG: BranchGroup) extends Behavior {
  
  class Wrapper(val tg:TransformGroup) extends BranchGroup {
    this.addChild(tg)
    this.setCapability(BranchGroup.ALLOW_DETACH)
  }
  
  private val balls = new HashMap[CId,Wrapper]()
  private val it = onlyAfterContinuous(h).iterator
  private val tmpTr = new Transform3D()
  
  def initialize() = {
    wakeupOn(new WakeupOnElapsedFrames(1))
  }

  def processStimulus(criteria: java.util.Enumeration[_]) : Unit = {
    val nx = name("x")
    val ny = name("y")
    val nz = name("z")
    val nm = name("m")
    def get(e:Env,n:Name) = { 
      e(n) match { 
        case VLit(GDouble(x)) => x
        case x => error("cannot extract a value from " + x)
      } 
    }
    
    val s = it.next
    var encountered = new HashSet[CId]()
    for ((id,e) <- s; if e contains nx; if e contains ny; if e contains nz; if e contains nm) {
    	encountered += id
    	val x = get(e,nx)
    	val y = get(e,ny)
    	val z = get(e,nz)
     	val m = get(e,nm)
    	tmpTr.set(new Vector3d(x,y,z))
        tmpTr.setScale(pow(m.toDouble,0.33))
    	if (balls contains id) {
    		balls(id).tg.setTransform(tmpTr)
    	} else {
    		//val bg = new BranchGroup()
    		val tg = new TransformGroup(tmpTr)
            val w = new Wrapper(tg)
    		tg.setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE)
    		val ball = new Ball(id)
    		tg.addChild(ball)
    		//bg.addChild(tg)
    		targetBG.addChild(w)
    		balls(id) = w
    	} 
    }
    // remove dead children
    for ((x,w) <- balls) {
      if (!(encountered contains x)) {
        w.detach()
        balls -= x 
      }
    }
    
    wakeupOn(new WakeupOnElapsedFrames(1))
  }
}

class Ball(id:CId) extends BranchGroup {
  val mat = new Material()
  mat.setAmbientColor(ColorUtil.getColor(id))
  mat.setDiffuseColor(ColorUtil.getColor(id))
    
  val ap = new Appearance()
  ap.setMaterial(mat)

  val s = new Sphere(0.05f, Primitive.GENERATE_NORMALS, 30, ap) 
  this.addChild(s)
}

object Glass extends Appearance {
  val mat = new Material()
  mat.setAmbientColor(new Color3f(1,1,1))
  mat.setDiffuseColor(new Color3f(1,1,1))
  
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


class Axis extends Shape3D {
  
  val ap = new Appearance()
  ap.setLineAttributes(new LineAttributes(1f,LineAttributes.PATTERN_SOLID,true))
  
  setGeometry(createGeometry)
  setAppearance(ap)
 
  private def createGeometry : Geometry = {
    val line = new LineArray(6, GeometryArray.COORDINATES | GeometryArray.COLOR_3)
    
    val red = new Color3f(0.5f,0,0)
    val green = new Color3f(0,0.5f,0)
    val blue = new Color3f(0,0,0.5f)
    
    line.setCoordinate(0, new Point3f(-10,0,0))
    line.setCoordinate(1, new Point3f(10,0,0))
    line.setColor(0,red)
    line.setColor(1,red)
    
    line.setCoordinate(2, new Point3f(0,-10,0))
    line.setCoordinate(3, new Point3f(0,10,0))
    line.setColor(2,green)
    line.setColor(3,green)

    line.setCoordinate(4, new Point3f(0,0,-10))
    line.setCoordinate(5, new Point3f(0,0,10))
    line.setColor(4,blue)
    line.setColor(5,blue)

    line
  }
}

class Plane() extends Shape3D {
  
  val ap = new Appearance()
  ap.setLineAttributes(new LineAttributes(1f,LineAttributes.PATTERN_SOLID,true))
  
  setGeometry(createGeometry)
  setAppearance(ap)
 
  private def createGeometry : Geometry = {
    val line = new LineArray(80, GeometryArray.COORDINATES | GeometryArray.COLOR_3)
    var count = 0
    for (x <- -10 until 0) {
      line.setCoordinate(count, new Point3f(x.toFloat/2,-5,0))
      count += 1
      line.setCoordinate(count, new Point3f(x.toFloat/2,5,0))
      count += 1
    }
    for (x <- 1 until 11) {
      line.setCoordinate(count, new Point3f(x.toFloat/2,-5,0))
      count += 1
      line.setCoordinate(count, new Point3f(x.toFloat/2,5,0))
      count += 1
    }
    for (y <- -10 until 0) {
      line.setCoordinate(count, new Point3f(-5,y.toFloat/2,0))
      count += 1
      line.setCoordinate(count, new Point3f(5,y.toFloat/2,0))
      count += 1
    }
    for (y <- 1 until 11) {
      line.setCoordinate(count, new Point3f(-5,y.toFloat/2,0))
      count += 1
      line.setCoordinate(count, new Point3f(5,y.toFloat/2,0))
      count += 1
    }
    var grey = new Color3f(0.25f,0.25f,0.25f)
    for (i <- 0 until 80)
      line.setColor(i, grey)
    line
  }
}

class MainBox extends TransformGroup {
  val tr = new Transform3D()
  //tr.set(new Vector3d(0.5f,0.5f,0.5f))
  val box = new Box(0.5f,0.5f,0.5f, Primitive.GENERATE_NORMALS, Glass)
  //this.setTransform(tr)
  this.addChild(box)
}
 
class Java3D(h:History) extends Applet {
  setLayout(new BorderLayout())
  
  // canvas creation
  val config = SimpleUniverse.getPreferredConfiguration()
  //val canvas3D = new CapturingCanvas3D(config)
  val canvas3D = new Canvas3D(config)
  add("Center", canvas3D)
  
  // SimpleUniverse is a convenience utility class
  val simpleU = new SimpleUniverse(canvas3D)
  
  // scene creation
  val scene = createSceneGraph(simpleU)
  scene.compile

  // set up transparency
  simpleU.getViewer().getView().setTransparencySortingPolicy(View.TRANSPARENCY_SORT_GEOMETRY)
  
  
  // move viewport to z = -2.4
  //simpleU.getViewingPlatform().setNominalViewingTransform();
  //val tr = new Transform3D()
  //tr.lookAt(new Point3d(4,3,2), new Point3d(0.5,0.5,0.5), new Vector3d(0,0,1))
  //tr.invert()
  //simpleU.getViewingPlatform().getMultiTransformGroup().getTransformGroup(0).setTransform(tr)
  
  
  // add the scene to the graph: this makes it "live"
  // and start the rendering process
  simpleU.addBranchGraph(scene)
  
  def createSceneGraph(su:SimpleUniverse) = {
    // root of the branch graph
    val objRoot = new BranchGroup()

    // transform group for mouse rotation
    val objRotate = new TransformGroup()
    objRotate.setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE)
    objRotate.setCapability(TransformGroup.ALLOW_TRANSFORM_READ)
    
    // transform group for mouse zoom
    val objZoom = new TransformGroup()
    objZoom.setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE)
    objZoom.setCapability(TransformGroup.ALLOW_TRANSFORM_READ)
    
    // transform group for mouse translate
    val objTrans = new TransformGroup()
    objTrans.setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE)
    objTrans.setCapability(TransformGroup.ALLOW_TRANSFORM_READ)
    
    // the actual scene
    val balls = new BranchGroup()
    balls.setCapability(Group.ALLOW_CHILDREN_WRITE)
    balls.setCapability(Group.ALLOW_CHILDREN_EXTEND)
    
    // the origin

    val tr1 = new Transform3D()
    tr1.rotX(-Pi/2)
//    val tr = new Transform3D()
//    tr.set(new Vector3f(-0.5f,-0.5f,-0.5f))
//    tr1.mul(tr)
    val objOrig = new TransformGroup(tr1)
    
    val mainbox = new MainBox()
    
    objOrig addChild new Plane()
    objOrig addChild new Axis()
    objOrig addChild mainbox
    objOrig addChild balls
    
    objRotate addChild objOrig
    objTrans addChild objRotate
    objZoom addChild objTrans
    objRoot addChild objZoom
    
      
    // where things happen
    val sphere = new BoundingSphere(new Point3d(0,0,0),1000)
        
    val myMouseRotate = new MouseRotate(objRotate)
    myMouseRotate.setSchedulingBounds(sphere)
    objRoot addChild myMouseRotate
    
    val myMouseZoom = new MouseZoom(objZoom)
    myMouseZoom.setSchedulingBounds(sphere)    
    objRoot addChild myMouseZoom
    
    val myMouseTrans = new MouseTranslate(objTrans)
    myMouseTrans.setSchedulingBounds(sphere)    
    objRoot addChild myMouseTrans
        
    val lightA = new AmbientLight(new Color3f(0.3f,0.3f,0.3f));
    lightA.setInfluencingBounds(sphere)
    objRoot.addChild(lightA)
    

    /* follows the view, only affects the box */
        
    /* static direction, affects the balls */
    val lightD1 = new DirectionalLight()
    lightD1.setDirection(-4.076f,-1.005f,-5.904f)
    lightD1.setColor(new Color3f(1f,1f,1f))
    lightD1.setInfluencingBounds(sphere)
    objOrig.addChild(lightD1)

    val lightD2 = new DirectionalLight()
    lightD2.setDirection(1,1,-1)
    lightD2.setColor(new Color3f(0.2f,0.2f,0.2f))
    lightD2.setInfluencingBounds(sphere)
    objOrig.addChild(lightD2)


    val phys = new PhysicalBehaviour(h, balls)
    phys.setSchedulingBounds(sphere)
    objRoot addChild phys
    
    val b = new javax.media.j3d.Background(0.3f,0.3f,0.3f)
    b.setApplicationBounds(sphere)
    objRoot addChild b
    
    objRoot
  }
}


/*
class Trace(id:Int, tr:(Int,Seq[Double],Seq[Double],Seq[Double])) extends Shape3D {
  
  val myColor = ColorUtil.getColor(id)
  setGeometry(createGeometry)
  
  private def createGeometry : Geometry = {
    val (_,xs,ys,zs) = tr
    val line = new LineStripArray(
      xs.length, 
      GeometryArray.COORDINATES | GeometryArray.COLOR_3, 
      Array(xs.length))
    for (i <- 0 until xs.length) {
      line.setCoordinate(i, new Point3d(xs(i)/5f, ys(i)/5f, zs(i)/5f))
      line.setColor(i,myColor)
    }
    line
  }
}
*/
