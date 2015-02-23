package acumen
package util
import java.io.File
import java.io.FileInputStream

object System {
 
  val FILE_SUFFIX_MODEL = ".acm"
  private val osName = java.lang.System.getProperty("os.name").toLowerCase()
  
  val version = {
    val md = getClass.getClassLoader.getResourceAsStream("acumen/version")
    if (md == null) "unknown version"
    else { scala.io.Source.fromInputStream(md).mkString }
  }

  val buildId = {
    val md = getClass.getClassLoader.getResourceAsStream("acumen/build_id")
    if (md == null) None
    else Some(scala.io.Source.fromInputStream(md).mkString)
  }
  
  sealed abstract class OS
  case object Windows extends OS
  case object Unix extends OS
  case object Mac extends OS
  case object Other extends OS

  def detectOperatingSystem() =
    if (osName contains "win") Windows
    else if (List("nix","nux","aix").exists(osName contains _)) Unix
    else if (osName contains "mac") Mac
    else Other
  
  //val version = "10.10.25"
  val tutorialUrl = 
    new java.net.URI("http://www.acumen-language.org/p/tutorials.html")
  
  /** 
   * Reads each file in the folder at path that ends with extensionFilter into a String.
   * Returns an Iterable of pairs, with the first String equal to the filename prefix
   * and the second equal to the text contents of the string (read as UTF-8).
   **/
  def readFiles(path: String, extensionFilter: String): Map[String, String] = {
    new File(path).listFiles.filter(_.getName.endsWith(extensionFilter)).map { 
      f: File => {
        val n = f getName
        val in = new FileInputStream(path + File.separator + n)
        val s = scala.io.Source.fromInputStream(in).getLines().mkString("\n")
        in close()
        val prefix = n.substring(0, n.lastIndexOf(extensionFilter))
        (prefix, s)
      }
    }.toMap
  }

  /** Reads the file on path into a String. */
  def readFile(path: String): String = {
    val in = new FileInputStream(path)
    val s = scala.io.Source.fromInputStream(in).getLines().mkString("\n")
    in.close()
    s
  }
  
  /** Depending on the operating system, returns the appropriate mask key. */
  def shortcutMask() = util.System.detectOperatingSystem match {
    case util.System.Windows | util.System.Unix | util.System.Other => java.awt.event.InputEvent.CTRL_MASK
    case util.System.Mac => java.awt.event.InputEvent.META_MASK
  }
  
}

