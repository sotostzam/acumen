package acumen.ui.tl

import acumen.ui.App
import java.awt.Color

/**
 * Search for resources in the work space.
 * 
 *  - Views 
 *  - Class definitions in the current model.
 *  - Files and directories in the current FileTree root.
 *  - Book marks (e.g. the examples directory) 
 *  
 */
object SearchUtil {
  
  def classDefRegExp(className: String) = ("class\\s*" + className + "\\s*\\(").r
  val ClassNameCaptor = classDefRegExp("(\\w*)")
  
  /** Represents a resource that can be jumped to from this dialog */
  sealed abstract class SearchResult(val result: String) {
    def description = this match {
      case _: View      => "View"
      case _: ClassName => "Class Name"
      case _: File      => "File"
      case _: Directory => "Directory"
      case _: Bookmark  => "Bookmark"
    }
  }
  sealed case class ClassName(override val result: String) extends SearchResult(result)
  sealed case class File(override val result: String, f: java.io.File) extends SearchResult(result)
  sealed case class Directory(override val result: String, d: java.io.File) extends SearchResult(result)
  sealed abstract class Bookmark(override val result: String) extends SearchResult(result)
  case object ExamplesBookmark extends Bookmark("Examples")
  sealed abstract class View(override val result: String) extends SearchResult(result)
  case object ConsoleView extends View("Console")
  case object EditorView extends View("Editor")
  case object FileBrowserView extends View("File Browser")
  
  /** Returns resources matching term, i.e. whose name contains term as a sub-string. */
  def search(term: String): List[SearchResult] = {
    val modelText = App.ui.codeArea.textArea.getText
    def containsTermIgnoreCase(s: String) = s.toLowerCase contains term.toLowerCase
    val views = List(ConsoleView, EditorView, FileBrowserView) filter (sr => containsTermIgnoreCase(sr.result))
    val classNames = for {
      ClassNameCaptor(name) <- ClassNameCaptor.findAllIn(modelText)
      if containsTermIgnoreCase(name) && name.nonEmpty
    } yield ClassName(name)
    val (rootFiles, rootDirectories) =
      App.ui.fileBrowser.fileTree.rootFile.listFiles().foldLeft(
        (List[SearchResult](), List[SearchResult]())) {
          case (cs @ (fs, ds), f) if containsTermIgnoreCase(f.getName) => // Found match
            ( if (f.isFile)      fs :+ File     (f.getName, f) else fs
            , if (f.isDirectory) ds :+ Directory(f.getName, f) else ds )
          case (cs, _) => cs
        }
    val bookmarks = List(ExamplesBookmark) filter (sr => containsTermIgnoreCase(sr.result))
    views ++ classNames.toList ++ rootDirectories ++ rootFiles ++ bookmarks
  }
  
  /**
   * Highlight the result in the corresponding component.
   * For Class name results, the line containing the corresponding class definition
   * is highlighted in the code editor.
   */
  def highlightResult(searchResult: SearchResult) = searchResult match {
    case _: ClassName =>
      val modelText = App.ui.codeArea.textArea.getText
      classDefRegExp(searchResult.result).findFirstIn(modelText) match {
        case Some(classDefText) =>
          App.ui.codeArea.focusOnCaretPosition(modelText indexOf classDefText)
          App.ui.codeArea.textArea.setHighlightCurrentLine(true)
        case None =>
          App.ui.codeArea.textArea.setHighlightCurrentLine(false)
      }
    case File(_,f) =>
      App.ui.fileBrowser.fileTree.focus(f)
    case Directory(_,d) =>
      App.ui.fileBrowser.fileTree.focus(d)
    case _ =>
  }

}