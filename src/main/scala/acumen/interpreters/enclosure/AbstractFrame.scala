package acumen.interpreters.enclosure

import org.jfree.ui.ApplicationFrame
import javax.swing.JComponent

trait AbstractFrame {
	def add(c: JComponent)
	def invalidate
}

object AbstractFrame {
  def wrap(f: ApplicationFrame) = {
    new AbstractFrame {
      def add(c: JComponent) = f.getContentPane().add(c)
      def invalidate = f.invalidate
    }
  }
}
