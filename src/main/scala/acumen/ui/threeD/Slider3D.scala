package acumen
package ui
package threeD
import swing._

class Slider3D extends BoxPanel(Orientation.Horizontal) {
  val bar = new Slider {
    value = 0
    min = 0
    max = 100
    minimumSize = new Dimension(100, 0)
    preferredSize = new Dimension(300, 0)
  }
  var firstPlayed = false
  var labelTime = new Label {
    text = "Time: "
  }
  val labelSpeed = new Label {
    text = "Speed: 1.0x"
  }
  val labels = new BoxPanel(Orientation.Vertical) {
    contents += labelTime
    contents += labelSpeed
    border = javax.swing.BorderFactory.createEmptyBorder(5, 5, 5, 5)
  }
  def setProgress3D(p: Int) = bar.value = p
  def setTime(p: Float) = {
    labelTime.text = f"Time: $p%.2f"
  }
  def setSpeed(p: String) = labelSpeed.text = "Speed:" + p + "x"
  border = javax.swing.BorderFactory.createEmptyBorder(5, 5, 5, 5)
//  contents += new Label("Time: ")
  contents += bar
  contents += labels
}