package acumen
package ui

import swing._
import swing.event._

class StatusZone extends BoxPanel(Orientation.Horizontal) { 
  val bar = new ProgressBar { min = 0; max = 100; minimumSize = new Dimension(200,0) }
  def setProgress(p:Int) = bar.value = p
  border = javax.swing.BorderFactory.createEmptyBorder(5,5,5,5)
  contents += new Label("Progress: ")
  contents += bar
}
