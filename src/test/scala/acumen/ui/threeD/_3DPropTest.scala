package acumen
package ui
package threeD

import ui.Controller

import ui.tl.Console
import Pretty._
import testutil.Generators._
import org.scalacheck.Gen._

import org.scalacheck.Properties
import org.scalacheck.Prop._

object _3DTests extends Properties("acumen") {
  val app = new Controller;
  /* Check if it is a int or a double */
	def isNumber(x:Value[CId]) : Boolean = {
		x match{
			case VLit(GInt(i))    => true;
			case VLit(GDouble(i)) => true;
			case _          => false; 
		}
	} 
	
	/* _3D Position,color,angle should all be a vector with 3 numbers */
	property("checkVecorContent Correctness") =
    forAll { (x:List[VLit]) =>
			if ((x.size == 3) && isNumber(x.head) && isNumber(x(1)) && isNumber(x(2)))
				app.threeDData.validVector(x)
      else
			  !app.threeDData.validVector(x)
    }
	
	/* _3D object's type should be a string or an int */
	property("extractType Correctness") =
		forAll { (x:Value[CId]) => {
		  var flag = true;
      try { 
        app.threeDData.extractType(x); 
      } catch {
        case e => {flag = false}
      }
			x match{
				case VLit(GStr(s)) => flag == true
				case VLit(GInt(s)) => flag == true
				case _             => flag == false	
			}
			}
    }
	/* _3D object's size should be a number or vector, and should bind with type.
   * For example: A sphere's size should be a number and a box's size should be
	 * a vector with 3 elements                                                  */
	val _3DType =  org.scalacheck.Gen.oneOf ("Sphere", "Cylinder", "Box", "Cone", "text")
	property("extractSize Correctness") =
		forAll { (x:Value[CId]) => {
		  var flag = true;
			app.threeDData._3DType = 
			_3DType.sample match {
												case Some(x) => x;
												case None    => "text";
												};
		
      try { 
        app.threeDData.extractSize(x); 
      } catch {
        case e => {flag = false}
      }
				//println(app.threeDData._3DType + "  " + app.threeDData._3DSize.length.toString  )
			x match{
				case VLit(GDouble(s)) => 
					{if (app.threeDData._3DType == "Sphere" || app.threeDData._3DType == "text") flag == true
					 else flag == false}
				case VLit(GInt(s))   =>
					{if (app.threeDData._3DType == "Sphere" || app.threeDData._3DType == "text") flag == true
					 else flag == false}
				case VVector(vs)     =>
					{if      (app.threeDData._3DType == "Box"      && app.threeDData._3DSize.length  == 3) flag == true
					 else if (app.threeDData._3DType == "Cylinder" && app.threeDData._3DSize.length  == 2) flag == true
					 else if (app.threeDData._3DType == "Cone"     && app.threeDData._3DSize.length  == 2) flag == true
					 /* "Sphere" => [1.0] is allowed */
					 else if ((app.threeDData._3DType == "Sphere" || app.threeDData._3DType == "text")
										&& app.threeDData._3DSize.length  == 1) flag == true
					 else flag == false}
				case _             => flag == false	
			}
			}
    }

	/* Can extract simulation end time correctly */	
        // FIXME: Reenable once fixed
        //property("extractSimulationTime Correctness") = throw new java.lang.Exception("Unimplemented")
    //     	forAll { (x:Double) => (x > 0 && x != 10) ==> {
    //     	  var flag = true;
    //     		val txt = """
    //     			class Main(simulator)
    //     				simulator.endTime = """ + x.toString + """
    //     			end
    //     		"""
    //     		//println(txt)
    //     		val appModel = new Controller(txt,new Console); // FIXME: This needs to be redone
    //     		appModel.play
    //     		/* Wait until simulation progress finish. Exclude 10 is because it's the 
    //     		 * default endTime and will leads to false result at first time step (initalization) */
    //     		while(appModel.state != ui.Stopped() || appModel.data.endTime == 10){
    //     			val a = "Busy waiting";
    //     		}
    //     		(appModel.data.endTime == x)	
    //     		}
    // }
}
