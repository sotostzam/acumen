package acumen
package util
import java.io.File
import java.io.FileInputStream

object System {
 
  val ACUMEN_FILE_SUFFIX = ".acm"
  
  val version = {
    val md = getClass.getClassLoader.getResource("acumen/version")
    if (md == null) "unknown version"
    else { io.Source.fromFile(md.getFile).mkString }
  }
  
  //val version = "10.10.25"
  val tutorialUrl = 
    new java.net.URI("http://www.acumen-language.org/p/tutorials.html")
  
  /** 
   * Reads each file in the folder at path that end with extensionFilter into a String.
   * Returns an Iterable of pairs, with the first String equal to the file's contents
   * and the second equal to the text contents of the string (read as UTF-8).
   **/
  def readFiles(path: String, extensionFilter: String): Iterable[(String, String)] = {
    new File(path).listFiles.filter(_.getName.endsWith(extensionFilter)).map { 
      f: File => {
        val n = f getName
        val in = new FileInputStream(path + File.pathSeparator + n)
        val s = scala.io.Source.fromInputStream(in).getLines().mkString("\n")
        in close
        val prefix = n.substring(0, n.lastIndexOf(ACUMEN_FILE_SUFFIX))
        (prefix, s)
      }
    }
  }
  
}

