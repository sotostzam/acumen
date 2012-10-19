package acumen
package tests

import Errors._
import util.Filters._
import util.Names._
import util.Canonical._
import interpreters.reference.Interpreter
import ui.Controller
import ui.tl.Console
import scala.math._

import java.io.FileInputStream
import java.io.InputStreamReader

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Suite
import org.scalacheck.Arbitrary.arbitrary
import Pretty._
import Generators._

import org.scalacheck.Properties
import org.scalacheck.Prop._


class _3DTest extends Suite with ShouldMatchers {

  def run(file:String) = {
    throw new java.lang.Exception("Unimplemented")
    // val appModel = new Controller(file,new Console); // FIXME: This needs to be redone
    //     appModel.play
  }
  def getError(file:String) : Option[AcumenError] = {
    try {   run(file); None}
    catch { case e:AcumenError => Some(e) }
  }
    
  def testError1 = {
		val err = _3DError(VLit(GStr("")))
		val txt = """
      class Main(simulator)
				private
					_3D = "";
				end
      end
    """
    getError(txt) should be (Some(err))
  }
	def testError2 = {
		val err = _3DError(VVector(List()))
		val txt = """
      class Main(simulator)
				private
					_3D = [];
				end
      end
    """
    getError(txt) should be (Some(err))
  }
	def testError3 = {
		val err = _3DError(VLit(GDouble(1.0)))
		val txt = """
      class Main(simulator)
				private
					_3D = 1.0;
				end
      end
    """
    getError(txt) should be (Some(err))
  }
	def testError4 = {
		val err = _3DError(VVector(List(VLit(GStr("Sphere")), 
											 VVector(List(VLit(GInt(0)), VLit(GInt(0)),VLit(GInt(0)))))))
		val txt = """
      class Main(simulator)
				private
					_3D = ["Sphere",[0,0,0]];
				end
      end
    """
    getError(txt) should be (Some(err))
  }
  def testError5 = {
		val err = _3DVectorError(VVector(List(VLit(GInt(0)), VLit(GInt(0)))), "position")
		val txt = """
     class Main(simulator)
       private
        _3D = ["Sphere",[0,0],1,[0,0,0],[0,0,0]];
      end
    end
    """
     getError(txt) should be (Some(err))
  }
	def testError6 = {
		val err = _3DVectorError(VVector(List(VLit(GInt(0)), VLit(GInt(0)))), "color")
		val txt = """
     class Main(simulator)
       private
        _3D = ["Sphere",[0,0,0],1,[0,0],[0,0,0]];
      end
    end
    """
     getError(txt) should be (Some(err))
  }
	def testError7 = {
		val err = _3DVectorError(VLit(GDouble(3.14)), "angle")
		val txt = """
     class Main(simulator)
       private
        _3D = ["Box",[0,0,0],[1,1,1],[0,0,0],3.14];
      end
    end
    """
     getError(txt) should be (Some(err))
  }
	def testError8 = {
		val err = _3DSizeError(VLit(GBool(true)))
		val txt = """
     class Main(simulator)
       private
        _3D = ["Sphere",[0,0,0],true,[0,0,0],[0,0,0]];
      end
    end
    """
     getError(txt) should be (Some(err))
  }
	def testError9 = {
		val err = _3DSphereSizeError()
		val txt = """
     class Main(simulator)
       private
        _3D = ["Sphere",[0,0,0],[1,1,1],[0,0,0],[0,0,0]];
      end
    end
    """
     getError(txt) should be (Some(err))
  }
	def testError10 = {
		val err = _3DCylinderSizeError()
		val txt = """
     class Main(simulator)
       private
        _3D = ["Cylinder",[0,0,0],1,[0,0,0],[0,0,0]];
      end
    end
    """
     getError(txt) should be (Some(err))
  }
	def testError11 = {
		val err = _3DBoxSizeError()
		val txt = """
     class Main(simulator)
       private
        _3D = ["Box",[0,0,0],[1,1],[0,0,0],[0,0,0]];
      end
    end
    """
     getError(txt) should be (Some(err))
  }
	def testError12 = {
		val err = _3DConeSizeError()
		val txt = """
     class Main(simulator)
       private
        _3D = ["Cone",[0,0,0],[1,1,1],[0,0,0],[0,0,0]];
      end
    end
    """
     getError(txt) should be (Some(err))
  }
	def testError13 = {
		val err = _3DTextSizeError()
		val txt = """
     class Main(simulator)
       private
        _3D = ["Acumen3D",[0,0,0],[1,1,1],[0,0,0],[0,0,0]];
      end
    end
    """
     getError(txt) should be (Some(err))
  }
  }
	
  
	
   

