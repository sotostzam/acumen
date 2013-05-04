package acumen.ui.tl

import java.awt.event.ActionEvent
import java.io.File
import scala.Array.canBuildFrom
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Buffer
import scala.swing.BorderPanel
import scala.swing.Component
import scala.swing.ScrollPane
import acumen.ui.GraphicalMain
import acumen.ui.Icons
import javax.swing.AbstractAction
import javax.swing.JButton
import javax.swing.JToggleButton
import javax.swing.JToolBar
import javax.swing.JTree
import javax.swing.event.ChangeEvent
import javax.swing.event.ChangeListener
import javax.swing.event.TreeModelEvent
import javax.swing.event.TreeModelListener
import javax.swing.tree.TreeModel
import javax.swing.tree.TreePath
import java.awt.event.MouseAdapter
import java.awt.event.MouseEvent

/** Wrapper for FileTree, provides navigation and configuration buttons. */
class FileBrowser(initialPath: File, editor: CodeArea) extends BorderPanel {

  val fileTree = new FileTree(initialPath)

  /* When synchronization is not enabled, double-clicking a model opens it in the editor */
  fileTree.peer.addMouseListener(new MouseAdapter {
    override def mousePressed(e: MouseEvent) {
      val clicked = fileTree.peer.getPathForLocation(e.getX, e.getY).getLastPathComponent
      if (!GraphicalMain.syncEditorWithBrowser && clicked != null && e.getClickCount == 2) {
        val f = clicked.asInstanceOf[File]
        if (f.isFile) editor.loadFile(f)
      }
    }
  })

  val goUpButton = new JButton()
  goUpButton.setAction(new AbstractAction("goUp", Icons.goUp) {
    override def actionPerformed(e: ActionEvent) { fileTree.goUp }
  })
  goUpButton.setToolTipText("Browse the parent directory")

  val goIntoButton = new JButton()
  goIntoButton.setAction(new AbstractAction("goInto", Icons.goInto) {
    override def actionPerformed(e: ActionEvent) { fileTree.goInto }
  })
  goIntoButton.setToolTipText("Focus file browser on selected directory")

  val syncButton = new JToggleButton()
  syncButton.setAction(new AbstractAction("sync", Icons.sync) {
    override def actionPerformed(e: ActionEvent) {
      GraphicalMain.syncEditorWithBrowser = !GraphicalMain.syncEditorWithBrowser
      fileTree.getSelectedFile match {
        case Some(_) => fileTree.refresh
        case None => editor.currentFile match {
          case Some(file) => fileTree.reset(file.getParentFile)
          case None       => fileTree.refresh
        }
      }
    }
  })
  syncButton.setSelected(GraphicalMain.syncEditorWithBrowser)

  syncButton.setToolTipText("Synchronize editor with file browser")

  val toolbar = new JToolBar()
  toolbar.setFloatable(false)

  for (b <- List(goUpButton, goIntoButton, syncButton)) {
    b.setHideActionText(true)
    b.setFocusable(false)
    b.setBorderPainted(false)
    toolbar.add(b)
  }
  syncButton.setBorderPainted(true) // Show toggle status

  add(Component.wrap(toolbar), BorderPanel.Position.North)
  add(new ScrollPane(fileTree), BorderPanel.Position.Center)
}

/**
 * Through a listener defined in CodeArea, this view (the FileTree component)
 * is used to select files for editing in Acumen. Selecting a file (called
 * README or with suffix .acm) will either open it directly or, if there is
 * unsaved work, propmt the user.
 * GraphicalMain.syncEditorWithBrowser can be toggled to disable the
 * synchronization of the editor an the file tree.
 */
class FileTree(initialPath: File) extends Component with ChangeListener {

  override lazy val peer = init(initialPath)

  private def init(path: File) = {
    val fileSystemModel = new FileSystemModel(initialPath)
    val tree = new JTree(fileSystemModel) with SuperMixin
    tree.setEditable(false)
    tree.setRootVisible(false)
    tree
  }

  /** Focus the file tree on a path */
  def reset(path: File) {
    // save expanded nodes
    val expanded = ArrayBuffer[TreePath]()
    val expDescendants = peer.getExpandedDescendants(new TreePath(peer.getModel.getRoot))
    if (expDescendants != null)
      while (expDescendants.hasMoreElements) {
        val nextPath = expDescendants.nextElement.asInstanceOf[TreePath]
        if (nextPath.getLastPathComponent != peer.getModel.getRoot)
          expanded += nextPath
      }
    // focus model on path
    peer.setModel(new FileSystemModel(path))
    // restore expanded nodes
    for (i <- (peer.getRowCount - 1) to 0 by -1)
      peer collapseRow i
    for (path <- expanded)
      peer expandPath path
  }
  
  /** Reload the contents of the model's folder into the tree view */
  def refresh() = reset(peer.getModel.getRoot.asInstanceOf[File])

  /** Focus the browser on the parent folder */
  def goUp() = {
    val parent = peer.getModel.getRoot.asInstanceOf[File].getAbsoluteFile.getParentFile
    if (parent != null) reset(parent)
  }

  /** Focus the browser on the currently selected folder */
  def goInto() = getSelectedFile match {
    case Some(file) => reset(file)
    case None       => // Do nothing
  }
  
  def getSelectedFile(): Option[File] = {
    val selectionPath = peer.getSelectionModel.getSelectionPath
    if (selectionPath != null)
      Some(selectionPath.getLastPathComponent.asInstanceOf[TreeFile])
    else
      None
  }

  /* If the user opened or saved the current file to a new name, check if the path needs to be adjusted. */
  override def stateChanged(e: ChangeEvent) {
    if (GraphicalMain.syncEditorWithBrowser) {
      val ca = e.getSource.asInstanceOf[CodeArea]
      ca.currentFile match {
        case Some(file) => // Check what was selected through File > Open
          if (file.isDirectory) reset(file) // Directory..
          else if (file.getParentFile != null) reset(file.getParentFile) // File..
        case _ =>
      }
    }
    else refresh
  }

}

private class TreeFile(parent: File, child: String) extends File(parent, child) {
  /** Show file name (and not the absolute path) in the tree */
  override def toString() = getName
}

class FileSystemModel(rootDirectory: File) extends TreeModel {

  private val listeners: Buffer[TreeModelListener] = new ArrayBuffer

  override def getRoot() = rootDirectory

  override def getChild(parent: Object, index: Int): Object = {
    val directory = parent.asInstanceOf[File]
    val children = directory.listFiles.filter(CodeArea.acumenFileFilter.accept(_)).map(_.getName)
    new TreeFile(directory, children(index))
  }

  def getChildCount(parent: Object): Int = {
    val file = parent.asInstanceOf[File]
    if (file.isDirectory) {
      val fileList = file.listFiles.filter(CodeArea.acumenFileFilter.accept(_))
      if (fileList == null) 0
      else fileList.length
    }
    else 0
  }

  def isLeaf(node: Object) = node.asInstanceOf[File].isFile;

  def getIndexOfChild(parent: Object, child: Object): Int = {
    val directory = parent.asInstanceOf[File]
    val file = child.asInstanceOf[File]
    directory.listFiles.filter(CodeArea.acumenFileFilter.accept(_)).indexWhere(_ == file.getName)
  }

  def valueForPathChanged(path: TreePath, value: Object) {
    val oldFile = path.getLastPathComponent.asInstanceOf[File]
    val fileParentPath = oldFile.getParent
    val newFileName = value.asInstanceOf[String]
    val targetFile = new File(fileParentPath, newFileName)
    oldFile.renameTo(targetFile)
    val parent = new File(fileParentPath)
    val changedChildrenIndices = List(getIndexOfChild(parent, targetFile)).toArray
    val changedChildren = List(targetFile.asInstanceOf[Object]).toArray
    fireTreeNodesChanged(path.getParentPath, changedChildrenIndices, changedChildren);
  }

  private def fireTreeNodesChanged(parentPath: TreePath, indices: Array[Int], children: Array[Object]) = {
    val event = new TreeModelEvent(this, parentPath, indices, children)
    for (l <- listeners) l.treeNodesChanged(event)
  }

  def addTreeModelListener(listener: TreeModelListener) { listeners += listener }

  def removeTreeModelListener(listener: TreeModelListener) { listeners -= listener }

}