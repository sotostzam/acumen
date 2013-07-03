package acumen

import Errors._
import java.io.File
import util.Canonical._
import util.Filters._
import util.Names._

object Examples {

  def cstoreExamplesAction(action: (String, File) => Unit) : Unit = {
    def filter = new java.io.FilenameFilter {
      def accept(d: File, fn: String) = {
        fn.substring(0,3)        != "XXX" && // Ignore internal files
        d.getName.substring(0,3) != "XXX" && // Ignore internal directories
        d.getName                != "01_Enclosures" && //FIXME Support enclosure sim. params in CStore interpreters 
        d.getName                != "02_Robust_Simulation" //FIXME Support enclosure sim. params in CStore interpreters 
      }
    }
    def helper(d: File, relPath: List[String]) : Unit = 
      for (f <- d.listFiles(filter)) {
        val fn = f.getName
        if (f.isDirectory) helper(f, relPath :+ fn)
        else if (fn.endsWith(".acm")) action(relPath.mkString(File.separator), f)
      }
    helper(new File("examples"), Nil)
  }

}
