package acumen
package ui

import java.lang.Thread

import scala.actors._
import scala.math._
import collection.JavaConversions._
import collection.immutable.Queue
import scala.collection.mutable.Map
import scala.collection.mutable.Buffer

import java.awt.Font
import java.awt.BorderLayout
import java.awt.Color
import java.awt.RenderingHints
import java.awt.GraphicsEnvironment
import java.awt.Desktop
import java.io._
import javax.swing.JOptionPane
import javax.swing.JTabbedPane
import javax.swing.SwingUtilities
import javax.swing.undo._
import javax.swing.text._
import javax.swing.KeyStroke
import javax.swing.event.DocumentListener
import javax.swing.event.DocumentEvent
import javax.vecmath.{AxisAngle4d, Color3f, Point3d, Vector3d,Point3f,Vector3f}

import swing._
import swing.event._

import com.sun.j3d.utils.geometry.ColorCube
import com.sun.j3d.utils.geometry.Sphere
import com.sun.j3d.utils.behaviors.mouse._
import javax.media.j3d.{Appearance, AmbientLight, Background, BoundingBox, 
  BoundingSphere, Bounds, BranchGroup, Canvas3D, DirectionalLight, GeometryArray,
  Group, Geometry, Material, LineAttributes, LineArray, PolygonAttributes, 
  QuadArray, Screen3D, Shape3D, Transform3D, TransformGroup, TransparencyAttributes,
  View, ViewPlatform}

class ThreeDPane(val appModel:AppModel) extends BorderPanel {

  var threeDView  = new ThreeDView()
  var playSpeed = 1.0;
  val faster = new Action("faster"){
    icon = Icons.faster
    def apply = {        
			playSpeed = playSpeed * 2
			if (playSpeed > 4) playSpeed = 4   // maximum *4 speed
				timer3d.sleepTime =  timer3d.initSpeed/playSpeed 
			// Recalculate sleep time
			timer3d.extraTime = ((timer3d.sleepTime-timer3d.sleepTime.toLong)*1000000).toInt 
			statusZone3d.setSpeed(playSpeed.toString) // Show the speed 
    }  
  }
  val slower = new Action("slower"){
    icon = Icons.slower
    def apply = {      
			playSpeed = playSpeed / 2   
			timer3d.sleepTime = (1/playSpeed)*timer3d.initSpeed
			// Recalculate sleep time	
			timer3d.extraTime = ((timer3d.sleepTime-timer3d.sleepTime.toLong)*1000000).toInt 
			statusZone3d.setSpeed(playSpeed.toString) // show the speed
    }  
  }
  val threedpause=new Action("pause"){
    icon = Icons.pause
    toolTip = "pause"
    def apply = {
    if (toolTip == "pause"){
      // un-pause
      timer3d.pause  = false
      receiver.pause = true
      icon    = Icons.play
      toolTip = "resume"
    }
    else{
       // pause
      timer3d.pause  = true
      receiver.pause = false
      icon    = Icons.pause
      toolTip = "pause"
     }   
    }               
  }
  val stop3d = new Action("stop"){
    threeDView.canvas.stopRenderer
    threeDView.canvas.startRenderer
    icon = Icons.stop
    def apply= {
			threedpause.toolTip="pause"
			threedpause.icon = Icons.pause
			receiver.destroy = true
			timer3d.destroy  = true
			threeDView.reset
			check.selected   = true
   }
    toolTip = "Stop visualizing"
  }
  /* ----3D-Visulization---- */
  var played = false;
	
  val threedplay = new Action("play"){
    icon = Icons.play
    def apply = {
			threedpause.toolTip = "pause"
			threedpause.icon    = Icons.pause
			endTime = appModel.data.endTime
			if(played){    
				receiver.stop;
			timer3d.destroy = true;
			statusZone3d.setSpeed(playSpeed.toString)
			if(check.selected == true)
				threeDView.axisOn
    }
	// First time press "3D play" button, 
	// copy the data from list to buffer to speed up
    if(!played){
	    _3DDataBuffer.clear 
			lastFrame = 0;
			statusZone3d.setSpeed("1.0")
        for((id,map) <- appModel.data._3DData){
          var temp = Map[Int,Buffer[List[_]]]()
          for((objectNumber,l)<-map){
             temp += (objectNumber->l.reverse.toBuffer)
             temp(objectNumber).last(5) match {
			   // The animation's length
               case n:Int => if (n>lastFrame) {lastFrame=n}  
               case _=>               
             }                      
            }
          _3DDataBuffer += id->temp              
         }
			appModel.data.reset;
    }        
     threeDView.branches.clear  
     threeDView.trans.clear         
     receiver = new _3DDisplay(threeDView,statusZone3d,_3DDataBuffer,lastFrame,
			       appModel.data.endTime)
     timer3d  = new ScalaTimer(receiver,appModel.data.endTime,playSpeed)
     receiver.start()
     timer3d.start()
     listenTo(receiver)
     receiver.listenTo(statusZone3d.bar.mouse.moves)
     receiver.listenTo(timer3d)
     timer3d.listenTo(statusZone3d.bar.mouse.clicks)
     timer3d.listenTo(statusZone3d.bar.mouse.moves)
		 played = true;                    
    }
    toolTip = "play"  
  }

  // _3DDataBuffer: Where all the state is stored
  var _3DDataBuffer = Map[CId,Map[Int,scala.collection.mutable.Buffer[List[_]]]]()
  var lastFrame = 2.0
  var endTime = 10.0

  val s = new Dimension(50, 40)
  val b3dplay = new Button(threedplay) {
                peer.setHideActionText(true);preferredSize = s }
  val b3dpause = new Button(threedpause) { 
                peer.setHideActionText(true);preferredSize = s }
  val b3dstop = new Button(stop3d) {
                peer.setHideActionText(true);preferredSize = s }
  val b3dfaster = new Button(faster) { 
                peer.setHideActionText(true);preferredSize = s }
  val b3dslower = new Button(slower) { 
                peer.setHideActionText(true);preferredSize = s }
  val check = new CheckBox("") { 
      action = Action("Axis-On") { 
        if (selected) threeDView.axisOn 
        else threeDView.axisOff
      }
    }
  check.selected = true	
  def hide(button:Button){button.peer.setEnabled(false)}
  val threeDButtons = 
    new FlowPanel(FlowPanel.Alignment.Leading)(b3dplay, 
	                    b3dpause,b3dslower,b3dfaster,b3dstop,check)    

  val statusZone3d= new Slider3d

  val threeDBottomPane = new BoxPanel(Orientation.Horizontal) {
    contents += threeDButtons
    contents += statusZone3d
  }

  var receiver   =  new _3DDisplay(threeDView,statusZone3d,
                                   _3DDataBuffer,lastFrame,appModel.data.endTime)
  
  var timer3d      = new  ScalaTimer(receiver,appModel.data.endTime,playSpeed)

  //
  //
  //

  def reset = {
    receiver.stop; 
    played = false;
    receiver.destroy=true;
    check.selected = true;	
    timer3d.destroy=true;
    threeDView.reset
  }

  def setProgress(p:Int) = {
    statusZone3d.setProgress(p);
    statusZone3d.setTime((p.toFloat/100)*endTime.toFloat)
  }

  //
  // Final Init
  //

  add(threeDView.init(),BorderPanel.Position.Center)
  add(threeDBottomPane, BorderPanel.Position.South)  

}


