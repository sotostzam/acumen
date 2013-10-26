package acumen.ui.tl

import java.awt.event.ActionEvent
import java.awt.event.KeyAdapter
import java.awt.event.KeyEvent
import java.awt.event.MouseAdapter
import java.awt.event.MouseEvent
import java.io.File

import scala.Array.canBuildFrom
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Buffer
import scala.swing.BorderPanel
import scala.swing.Component
import scala.swing.ScrollPane

import acumen.Main
import javax.swing.JCheckBox
import javax.swing.JToolBar
import javax.swing.JTree
import javax.swing.event.ChangeEvent
import javax.swing.event.ChangeListener
import javax.swing.event.TreeModelEvent
import javax.swing.event.TreeModelListener
import javax.swing.text.Position
import javax.swing.tree.TreeModel
import javax.swing.tree.TreePath

/** Wrapper for FileTree, provides navigation and configuration buttons. */
class FileBrowser(initialPath: File, editor: CodeArea) extends BorderPanel {

  val fileTree = new FileTree(initialPath)

  /* When synchronization is not enabled, double-clicking a model opens it in the editor */
  fileTree.peer.addMouseListener(new MouseAdapter {
    override def mousePressed(e: MouseEvent) {
      val clicked = fileTree.peer.getPathForLocation(e.getX, e.getY)
      if (!Main.synchEditorWithBrowser && e.getClickCount == 2 &&
        clicked != null && clicked.getLastPathComponent != null) {
        val f = clicked.getLastPathComponent.asInstanceOf[File]
        if (f.isFile) editor.loadFile(f)
      }
    }
  })
  
  add(new ScrollPane(fileTree), BorderPanel.Position.Center)
  
  /** Reload the contents of the tree view */
  def refresh() = fileTree.refresh()

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

  private def init(path: File): JTree with SuperMixin = {
    val fileSystemModel = new FileSystemModel(initialPath)
    val tree = new JTree(fileSystemModel) with SuperMixin
    tree setEditable false
    tree setRootVisible false
    /* Override behavior of the and right keyboard arrows.
     * When a top level folder is selected, pressing the left arrow 
     * causes the root to be reset to the parent of the current root.
     * If an expanded folder is selected, pressing the right arrow 
     * causes the root to be reset to selected node.
     */
    tree addKeyListener new KeyAdapter {
      override def keyPressed(e: KeyEvent) = {
        val leftPressed = e.getKeyCode == KeyEvent.VK_LEFT
        val selectedFile = getSelectedFile 
        selectedFile match {
          case Some(f) =>
            if (leftPressed &&
              (f == tree.getModel.getRoot || // Directory is empty (no visible node is selected)
                (!peer.isExpanded(peer.getSelectionPath) && // A top-level node is selected
                  f.getParentFile.getCanonicalFile == tree.getModel.getRoot.asInstanceOf[File].getCanonicalFile)))
              goUp
            else if (e.getKeyCode == KeyEvent.VK_RIGHT && peer.isExpanded(peer.getSelectionPath))
              goInto
          case None => if (leftPressed) goUp
        }
      }
    }
    tree
  }

  /**
   * Focus the file tree on a path.
   * Current node selection and expansion state will be restored.
   * Unlike reset(), this will not reset the root if the path is
   * a descendant of the root. In this case the tree node
   * corresponding to the node will be expanded instead.
   */
  def focus(path: File) {
    val didExpand = expandDescendant(peer.getModel.getRoot.asInstanceOf[File], path)
    if (!didExpand) reset(path) 
  }

  /**
   * If the path is a descendant of some root node, a tree node 
   * corresponding to path will be expanded. Returns true if the 
   * path is a descendant of the root and false otherwise. 
   */
  def expandDescendant(root: File, path: File): Boolean = {
    descendantOf(root, path) match {
      case Some(treePath) => // Path is a descendant of the root 
        peer.expandPath(treePath)
        selectPath(treePath)
        true
      case None => 
        false
    }
  }
  
  /** Selects and scrolls the path into view */
  def selectPath(treePath: TreePath) {
    peer.getSelectionModel.setSelectionPath(treePath)
    peer.scrollPathToVisible(treePath)
  }
  
  /**
   * Reset the file tree on a path.
   * Current node selection and expansion state will be restored.
   */
  def reset(path: File) {
    // save expanded/selected nodes
    val expanded = ArrayBuffer[TreePath]()
    val expDescendants = peer.getExpandedDescendants(new TreePath(peer.getModel.getRoot))
    if (expDescendants != null)
      while (expDescendants.hasMoreElements) {
        val nextPath = expDescendants.nextElement.asInstanceOf[TreePath]
        if (nextPath.getLastPathComponent != peer.getModel.getRoot)
          expanded += nextPath
      }
    val selected = peer.getSelectionModel.getSelectionPaths
    // focus model on path
    peer.setModel(new FileSystemModel(
      if (path.isDirectory) path
      else path.getParentFile))
    // restore expanded/selected nodes
    for (i <- (peer.getRowCount - 1) to 0 by -1)
      peer collapseRow i
    for (path <- expanded)
      peer expandPath path
    if (selected  != null) 
      for (s <- selected)
    	selectPath(s)
  }

  /**
   * If path is a sub-directory of the current tree's root, returns a TreePath to
   * this sub-directory wrapped in Some, otherwise returns None
   */
  private def descendantOf(someRoot: File, path: File): Option[TreePath] = {
    val r = someRoot.getCanonicalFile.getAbsolutePath
    val p = path.getCanonicalFile.getAbsolutePath
    if (r != p && p.startsWith(r)) {
      val pathElems = p.substring(r.length).split("/").tail //FIXME Make sure this work on Windows
      var treePath: TreePath = null
      for (e <- pathElems) {
        val row = if (treePath == null) 0 else peer.getRowForPath(treePath)
        treePath = peer.getNextMatch(e, row, Position.Bias.Forward)
        peer.expandPath(treePath)
      }
      Some(treePath)
    }
    else None
  }

  /** Reload the contents of the model's folder into the tree view */
  def refresh(): Unit = focus(peer.getModel.getRoot.asInstanceOf[File])

  /** Focus the browser on the parent folder */
  def goUp(): Unit = {
    val oldRoot = peer.getModel.getRoot.asInstanceOf[File]
    val parent = oldRoot.getAbsoluteFile.getParentFile
    if (parent != null) {
      reset(parent)
      expandDescendant(parent, oldRoot)
    }
  }

  /** Focus the browser on the currently selected folder */
  def goInto(): Unit = getSelectedFile.foreach(reset(_))

  /** Returns the file currently selected in the tree */
  def getSelectedFile(): Option[File] = {
    val selectionPath = peer.getSelectionModel.getSelectionPath
    if (selectionPath != null && selectionPath.getPathCount > 0 && selectionPath.getLastPathComponent != null)
      Some(selectionPath.getLastPathComponent.asInstanceOf[TreeFile])
    else None
  }

  /** 
   * If the user opened or saved the current file to a new name, check if the 
   * path needs to be adjusted. 
   */
  override def stateChanged(e: ChangeEvent) {
    // Check what was selected through File > Open
    silently(e.getSource.asInstanceOf[CodeArea].currentFile match { 
      case Some(file) =>
        if (Main.synchEditorWithBrowser)
          focus(file)
        if (file.isDirectory) peer.clearSelection
        refresh
      case None =>
        peer.clearSelection
    })
  }

  /**
   * Perform a without firing any TreeSelectionEvents.
   * This is useful e.g. when opening a new file, and the clearing of the FileTree's  
   * path should not fire any additional events.
   **/
  def silently[T](a: => T): T = {
    val tsls = peer.getTreeSelectionListeners()
    tsls.foreach(peer.removeTreeSelectionListener)
    val res = a
    tsls.foreach(peer.addTreeSelectionListener)
    res
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
    val children = listAcumenFilesSorted(directory).map(_.getName)
    new TreeFile(directory, children(index))
  }

  override def getChildCount(parent: Object): Int = {
    val file = parent.asInstanceOf[File]
    if (file.isDirectory) {
      val fileList = listAcumenFiles(file)
      if (fileList == null) 0
      else fileList.length
    }
    else 0
  }

  override def isLeaf(node: Object) = node.asInstanceOf[File].isFile;

  override def getIndexOfChild(parent: Object, child: Object): Int = {
    val directory = parent.asInstanceOf[File]
    val file = child.asInstanceOf[File]
    listAcumenFilesSorted(directory).indexWhere(_ == file.getName)
  }
  
  /** Returns the list of Acumen files (*.acm and README*) under path. */
  def listAcumenFiles(path: File) = path.listFiles.filter(CodeArea.acumenFileFilter.accept(_))

  /** Returns the list of Acumen files (*.acm and README*) under path, ordered by filename */
  def listAcumenFilesSorted(path: File) = listAcumenFiles(path).sortWith(_.getName.toLowerCase < _.getName.toLowerCase)

  override def valueForPathChanged(path: TreePath, value: Object) {
    val oldFile = path.getLastPathComponent.asInstanceOf[File]
    val fileParentPath = oldFile.getParent
    val newFileName = value.asInstanceOf[String]
    val targetFile = new File(fileParentPath, newFileName)
    oldFile.renameTo(targetFile)
    val parent = new File(fileParentPath)
    val changedChildrenIndices = List(getIndexOfChild(parent, targetFile)).toArray
    val changedChildren = List(targetFile.asInstanceOf[Object]).toArray
    fireTreeNodesChanged(path.getParentPath, changedChildrenIndices, changedChildren)
  }

  private def fireTreeNodesChanged(parentPath: TreePath, indices: Array[Int], children: Array[Object]) = {
    val event = new TreeModelEvent(this, parentPath, indices, children)
    for (l <- listeners) l.treeNodesChanged(event)
  }

  override def addTreeModelListener(listener: TreeModelListener) { listeners += listener }

  override def removeTreeModelListener(listener: TreeModelListener) { listeners -= listener }

}
