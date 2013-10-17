package acumen.ui;

import java.awt.Color
import java.awt.Component
import java.awt.event.ActionEvent
import java.awt.event.ActionListener
import java.util.concurrent.atomic.AtomicBoolean

import javax.swing.Timer

/** Swing and Scala Swing utilities. */
object SwingUtil {
  
  /** Class to store state used by flashComponentBackground. */
  class Flasher {
    val flashing = new AtomicBoolean(false)
    var time = 0
  }

  /** Temporarily animate the background color of a component, in order to draw attention to it. */
  def flashComponentBackground(c: Component, orig: Color, color: Color, flasher: Flasher) =
    flashFunction(c.setBackground(_), orig, color, flasher)
  
  /** Briefly vary the color passed to f according to a pulse. */
  def flashFunction(f: Color => Unit, orig: Color, color: Color, flasher: Flasher) {
    def pulse(x: Double) = 7.353 * x * Math.exp(-10 * x * x)
    val max = 200
    if (flasher.flashing.compareAndSet(false, true)) { // Block additional concurrent flashes
      val t = new Timer(4, null)
      t addActionListener new ActionListener() {
        def actionPerformed(evt: ActionEvent) = {
          val p = pulse(flasher.time / max.asInstanceOf[Double])
          val red   = (color.getRed   * p + orig.getRed   * (1 - p)).floor.intValue
          val green = (color.getGreen * p + orig.getGreen * (1 - p)).floor.intValue
          val blue  = (color.getBlue  * p + orig.getBlue  * (1 - p)).floor.intValue
          f (new Color(red, green, blue))
          if (flasher.time < max) flasher.time += 1
          else {
            t setRepeats false
            f (orig)
            flasher.time = 0
            flasher.flashing set false
        }}
        t start
      }
    }
    else flasher.time = 0
  }

}
