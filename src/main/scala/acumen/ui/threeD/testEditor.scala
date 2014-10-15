package acumen.ui.threeD

import java.awt.{Graphics, Color, BorderLayout}
import javax.swing.JPanel;
import javax.swing.border.EmptyBorder;
import javax.swing.JButton;
import acumen.CId
import acumen.ui.{Controller, Icons, App}
import com.threed.jpct._
import scala.collection.mutable.{Buffer, Map}
import swing._
import javax.swing.{JPopupMenu, SwingUtilities, JPanel, JTabbedPane}
import java.awt.event.{ComponentEvent, ComponentAdapter}

/**
 * Created by xufei on 9/22/14.
 */
class testEditor extends BorderPanel{
  val canvasPanel = new JPanel
  val editorPanel = new RenderPanel

  def cCanvas() = {
    editorPanel.renderPanel()
    canvasPanel.setLayout(new BorderLayout())
    canvasPanel.add(editorPanel, BorderLayout.CENTER)
    peer.add(canvasPanel, BorderLayout.CENTER)
    peer.setVisible(true)

  }

  cCanvas()
}
