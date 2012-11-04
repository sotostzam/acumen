package acumen
package ui
package tl

import java.awt.Color
import swing._


import javax.swing.ListCellRenderer
import javax.swing.DefaultListCellRenderer
import javax.swing.JList
import javax.swing.JLabel
import java.awt.Component
import javax.swing.border.LineBorder

class Console extends ListView[(Boolean,String)] {

  class ConsoleCellRenderer extends ListCellRenderer {
    protected val defaultRenderer = new DefaultListCellRenderer
  
    override def getListCellRendererComponent(
      list:JList, value:Any, index:Int, 
      isSelected:Boolean, cellHasFocus:Boolean) : Component = { 
      val message = value match {
        case (normal:Boolean, message:String) =>
          if (normal) message
          else {
            "<html>"+
            (if (index>0) "ERROR:" else "<font color=red>ERROR:</font>") +
            "<pre>"+ 
            (message.replaceAll("<","&lt;")
                    .replaceAll(">","&gt;")
                    .replaceAll("\n","<br/>")) + 
            "</pre></html>"
          }
        case _ => ""
      }
      val renderer = (defaultRenderer.getListCellRendererComponent(list,message,index,false,false)).asInstanceOf[JLabel]
      //renderer.setBorder(LineBorder.createGrayLineBorder)
      if (index>0) renderer.setForeground(Color.LIGHT_GRAY)
      renderer
    }
  }

  val initColor = selectionBackground
  var done = false

  renderer = 
    ListView.Renderer.wrap[(Boolean,String)](new ConsoleCellRenderer)

  def log(message:String) = {
    selectionBackground = initColor
    if (done) {
      listData = ((true, message)) +: listData
      done = false
    }
    else {
      if (listData.isEmpty) listData = Seq((true,message))
      else listData = { val (ty,m) = listData.head; (ty,m+message) } +: listData.tail
    }
  }

  def newLine = { done = true }

  def logError(message:String) = {
    listData = ((false, message)) +: listData
    done = true
  }

}
