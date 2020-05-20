package acumen.ui.tl

import java.io.File
import acumen.Main

/** Class including the file tree structure */
class FileTree() {
  var root: File = null
  var uID: Int = 0
  var treeData: Map[Int, File] = Map()

  def serializeFileTree(initialPath: File): Unit = {
    root = initialPath
    Main.webInterface.socketSend(ujson.write(createFileTree(root)))
  }

  def createFileTree(currentPath: File, index:Int = 0): ujson.Arr = {
    val files = currentPath.listFiles()
    val childrenArr = ujson.Arr()
    for (file <- files) {
      if (file.isDirectory) {
        uID+=1
        childrenArr.arr.append(createFileTree(file, uID)(0))
      }
      else {
        uID+=1
        childrenArr.arr.append(ujson.Obj("id" -> uID, "name" -> file.getName))
        treeData += (uID -> file)
      }
    }
    if (index==0) {
      ujson.Arr(ujson.Obj("action" -> "filetree"), ujson.Obj("id" -> index, "name" -> ujson.Str(currentPath.getName), "children" -> childrenArr))
    }
    else{
      //TODO This need to be fixed! THis has to return a json object.
      ujson.Arr(ujson.Obj("id" -> index, "name" -> ujson.Str(currentPath.getName), "children" -> childrenArr))
    }
  }

  def getFile(fileID: Int): File = {
    treeData(fileID)
  }
}
