package acumen.ui.tl

import java.awt.event.MouseAdapter
import java.awt.event.MouseEvent
import java.io.File

import scala.Array.canBuildFrom
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Buffer
import scala.swing.Component

import javax.swing.JTree
import javax.swing.event.ChangeEvent
import javax.swing.event.ChangeListener
import javax.swing.event.TreeModelEvent
import javax.swing.event.TreeModelListener
import javax.swing.tree.TreeModel
import javax.swing.tree.TreePath

/**
 * Through a listener defined in CodeArea, this view is used to select files
 * for editing in Acumen. Selecting a file (called README or with suffix .acm)
 * will either open it directly or, if there is unsaved work, propmt the user.
 */
class FileTree(initialPath: String) extends Component with ChangeListener {

  override lazy val peer: JTree = init(initialPath)

  private def init(path: String) = {
    val fileSystemModel = new FileSystemModel(new File(initialPath))
    val tree = new JTree(fileSystemModel) with SuperMixin
    tree.setEditable(false)
    tree.setRootVisible(false)
    tree
  }

  def reset(path: String) { peer.setModel(new FileSystemModel(new File(path))) }

  /* If the user opened or saved the current file to a new name, check if the path needs to be adjusted. */
  override def stateChanged(e: ChangeEvent) {
    val ca = e.getSource.asInstanceOf[CodeArea]
    ca.currentFile match {
      case Some(file) => // Check what was selected through File > Open
        if (file.isDirectory) reset(file.getAbsolutePath) // Directory..
        else if (file.getParentFile != null) reset(file.getParentFile.getAbsolutePath) // File..
      case _ =>
    }
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