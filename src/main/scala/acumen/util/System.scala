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
  
  sealed abstract class OS
  case class Windows extends OS
  case class Unix extends OS
  case class Mac extends OS
  case class Other extends OS

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
   * Returns an Iterable of pairs, with the first String equal to the file's contents
   * and the second equal to the text contents of the string (read as UTF-8).
   **/
  def readFiles(path: String, extensionFilter: String): Map[String, String] = {
    new File(path).listFiles.filter(_.getName.endsWith(extensionFilter)).map { 
      f: File => {
        val n = f getName
        val in = new FileInputStream(path + File.separator + n)
        val s = scala.io.Source.fromInputStream(in).getLines().mkString("\n")
        in close
        val prefix = n.substring(0, n.lastIndexOf(extensionFilter))
        (prefix, s)
      }
    }.toMap
  }
  
}

