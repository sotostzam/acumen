package acumen.ui.tl

import java.awt.Color
import java.awt.Dimension
import java.awt.Font
import scala.swing.BorderPanel
import scala.swing.Frame
import scala.swing.Label
import scala.swing.ListView
import scala.swing.ScrollPane
import scala.swing.TextField
import scala.swing.event.Key
import scala.swing.event.KeyPressed
import scala.swing.event.KeyReleased
import SearchUtil.Bookmark
import SearchUtil.ClassName
import SearchUtil.ConsoleView
import SearchUtil.Directory
import SearchUtil.EditorView
import SearchUtil.ExamplesBookmark
import SearchUtil.File
import SearchUtil.FileBrowserView
import SearchUtil.View
import SearchUtil.SearchResult
import SearchUtil.highlightResult
import SearchUtil.search
import acumen.ui.App
import acumen.ui.SwingUtil
import javax.swing.BorderFactory
import javax.swing.DefaultListModel
import acumen.ui.Files

/**
 * Used for quickly opening files and jumping to
 * definitions in the currently open model.
 */
object JumpDialog extends Frame {

  this.title = "Jump To"

  // State used to restore the caret position, current file and file browser root after an aborted jump
  private var previousCaretPosition = 0 
  private var previousModel: Option[java.io.File] = None
  private var previousTreeRoot: java.io.File = null

  val searchResultsModel = new DefaultListModel

  val searchField = new TextField
  
  minimumSize = new Dimension(250,250)

  /** Save the current model, caret position, and file browser root. */
  def saveWorkspace() {
    previousCaretPosition = App.ui.codeArea.textArea.getCaretPosition
    previousModel = App.ui.codeArea.currentFile
    previousTreeRoot = App.ui.fileBrowser.fileTree.rootFile
  }
  
  /** Restore the current model, caret position, and file browser root. */
  def restoreWorkspace() {
      App.ui.codeArea.textArea.setCaretPosition(previousCaretPosition)
      App.ui.codeArea.currentFile = previousModel
      App.ui.fileBrowser.fileTree.focus(previousTreeRoot) 
  }

  /**
   * Toggles the visibility of the jump dialog.
   * Also restores the (caret, highlight) state of the editor when disabling the dialog.
   */
  def toggle() {
    if (visible)
      App.ui.codeArea.textArea setHighlightCurrentLine false
    else {
      saveWorkspace()
      peer.setLocationRelativeTo(App.ui.views.peer)
      searchAndHighlight(searchField text)
      searchField requestFocus
    }
    visible = !visible
  }

  val searchResults = new ListView[SearchResult] {
    peer setModel searchResultsModel
    selection.intervalMode = ListView.IntervalMode.Single // Select single results
    val resultBorder = BorderFactory.createEmptyBorder(0, 0, 0, 5)
    val resultTypeFont = new Font(font.getName, java.awt.Font.ITALIC, font.getSize)
    val selectedColor = new Color(47, 95, 202)
    listenTo(keys)
    reactions += {
      case KeyPressed(_, Key.Up, _, _) if peer.getSelectedIndex == 0 =>
        searchField.requestFocus
      case KeyPressed(_, Key.Enter, _, _) =>
        jumpToSelected()
      case _: KeyReleased =>
        val selectedIndex = peer.getSelectedIndex
        if (searchResultsModel.size > 0 && selectedIndex >= 0) {
          val sr = searchResultsModel.get(selectedIndex).asInstanceOf[SearchResult]
          if (sr.result.nonEmpty) {
            highlightResult(sr)
            viewSelected()
          }
        }
    }
    renderer = new ListView.Renderer[SearchResult] {
      def componentFor(list: ListView[_], isSelected: Boolean,
                       hasFocus: Boolean, sr: SearchResult, index: Int) = {
        new BorderPanel {
          background = if (isSelected) selectedColor else Color.WHITE
          add(new Label(sr.result) {
            foreground = if (isSelected) Color.WHITE else sr match {
              case _: View      => new Color(0, 40, 209)
              case _: ClassName => new Color(116, 5, 81)
              case _: File      => Color.BLACK
              case _: Directory => new Color(0, 150, 60)
              case _: Bookmark  => new Color(200, 90, 0)
            }
          }, BorderPanel.Position.West)
          add(new Label(sr.description) {
            font = resultTypeFont
            foreground = if (isSelected) Color.LIGHT_GRAY else Color.GRAY
            border = resultBorder
          }, BorderPanel.Position.East)
        }
      }
    }
  }

  listenTo(searchField.keys)
  reactions += {
    case KeyPressed(_, Key.Down, _, _) if searchResultsModel.size > 0 =>
      searchResults selectIndices 0
      searchResults.requestFocus
    case KeyPressed(_, Key.Enter, _, _) if searchResultsModel.size > 0 =>
      jumpToSelected()
    case _: KeyReleased =>
      searchAndHighlight(searchField.text)
      viewSelected()
  }

  def jumpToSelected(): Unit =
    if (searchResults.peer.getSelectedIndex >= 0) {
      searchResultsModel.get(searchResults.peer.getSelectedIndex) match {
        case _: ClassName | _: File =>
          toggle()
        case Directory(_, d) =>
          App.ui.fileBrowser.fileTree.goInto
          searchField.text = ""
          searchAndHighlight(searchField.text)
        case ConsoleView =>
          toggle()
          App.ui.console.requestFocus
        case EditorView =>
          toggle()
          App.ui.codeArea.textArea.requestFocus
        case FileBrowserView =>
          toggle()
          App.ui.fileBrowser.fileTree.requestFocus
        case ExamplesBookmark =>
          toggle()
          App.ui.fileBrowser.fileTree.reset(Files.examplesDir)
          App.ui.fileBrowser.fileTree.requestFocus
      }
      viewSelected()
    }

  val codeAreaFlasher = new SwingUtil.Flasher
  val fileBrowserFlasher = new SwingUtil.Flasher
  val consoleFlasher = new SwingUtil.Flasher

  def viewSelected(): Unit =
    if (searchResults.peer.getSelectedIndex >= 0) {
      val (flashFun, flasher) =
        searchResultsModel.get(searchResults.peer.getSelectedIndex) match {
          case _: ClassName =>
            (App.ui.codeArea.background_=_, codeAreaFlasher)
          case File(_, f) =>
            App.ui.tabs.selection.index = App.ui.tabs.pages.indexOf(App.ui.fileBrowserPage)
            App.ui.fileBrowser.fileTree.focus(f)
            (App.ui.fileBrowserPage.background_=_, fileBrowserFlasher)
          case Directory(_, d) =>
            App.ui.tabs.selection.index = App.ui.tabs.pages.indexOf(App.ui.fileBrowserPage)
            App.ui.fileBrowser.fileTree.focus(d)
            (App.ui.fileBrowserPage.background_=_, fileBrowserFlasher)
          case ConsoleView =>
            App.ui.tabs.selection.index = App.ui.tabs.pages.indexOf(App.ui.consolePage)
            (App.ui.consolePage.background_=_, consoleFlasher)
          case EditorView =>
            (App.ui.codeArea.background_=_, codeAreaFlasher)
          case FileBrowserView =>
            App.ui.tabs.selection.index = App.ui.tabs.pages.indexOf(App.ui.fileBrowserPage)
            (App.ui.fileBrowserPage.background_=_, fileBrowserFlasher)
          case ExamplesBookmark =>
            App.ui.fileBrowser.fileTree.focus(Files.examplesDir)
            App.ui.tabs.selection.index = App.ui.tabs.pages.indexOf(App.ui.fileBrowserPage)
            (App.ui.fileBrowserPage.background_=_, fileBrowserFlasher)
        }
        SwingUtil.flashFunction(flashFun, Color.WHITE, Color.BLUE, flasher)
    } 

  /** Searches for term and highlights the result in the corresponding component. */
  def searchAndHighlight(term: String) = searchResultsModel.synchronized {
    searchResultsModel.removeAllElements()
    App.ui.codeArea.textArea.setHighlightCurrentLine(false)
    val results = search(term toLowerCase)
    if (results nonEmpty) {
      results.foreach(searchResultsModel.addElement)
      if (term nonEmpty) {
        highlightResult(results head)
        searchResults selectIndices 0
      }
    }
    pack()
  }

  contents = new BorderPanel {
    val sp = new ScrollPane(searchResults) {
      horizontalScrollBarPolicy = ScrollPane.BarPolicy.Never
      verticalScrollBarPolicy = ScrollPane.BarPolicy.AsNeeded
    }
    add(searchField, BorderPanel.Position.North)
    add(sp, BorderPanel.Position.Center)
  }
  
  pack() 

}