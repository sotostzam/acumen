package acumen

import Errors._
import java.io._
import util.Canonical._
import util.Filters._
import util.Names._

abstract class Examples {

  def cstoreExamplesAction(action: (String, File) => Unit) : Unit = {
    def filter = new java.io.FileFilter {
      def accept(f: File) = {
        true
        if (f.isDirectory()) true // Accept all directories so that we
                                  // can filter based on the full path
        else if (f.getName.startsWith("XXX")) false // Ignore files that start with XXX
        //else if (f.getName == "02_Passive_walking.acm") false // This file needs to be fixed to use the new semantics
        else {
          val path = f.getPath()
          def withDir(dirs: String*) = 
            path.contains(File.separator + dirs.mkString(File.separator) + File.separator)
        //  if      (withDir("XXX_internal","misc")) true // Test examples in misc directory even though it is in XXX_internal
        //  else if (withDir("XXX_internal","test")) true // Special examples just for testing
        //  else if (withDir("XXX_internal","0_Demos")) true // Old demos
          if (path.contains(File.separator + "XXX")) false // Ignore internal directories
          else if (withDir("01_Enclosures") || withDir("05_More_Enclosure_Examples")) false //FIXME Support enclosure sim. params in CStore interpreters 
        //  else if (withDir("02_Robust_Simulation")) false //FIXME Support enclosure sim. params in CStore interpreters 
          else if (withDir("14_Parallel")) false // These take way to long
          else true
        }
      }
    }
    def helper(d: File, relPath: List[String]) : Unit = 
      for (f <- d.listFiles(filter).sorted) {
        val fn = f.getName
        if (f.isDirectory) helper(f, relPath :+ fn)
        else if (fn.endsWith(".acm")) action(relPath.mkString(File.separator), f)
      }
    helper(new File("examples"), Nil)
  }

  // FIXME: Get these locations from scala/java/sbt some how...
  val expectLoc : String
  val gotLoc = "target/tmp/examples-res"

  def resultFile(loc: String, dn: String, f: File) =
    new File(new File(loc, dn), f.getName+".res")

  def writeExampleResult(loc: String, dn: String, f: File, semantics: SemanticsImpl.CStore) : Unit = {
    val d2 =new File(loc,dn)
    d2.mkdirs()
    val f2 = new File(d2, f.getName+".res")
    val out = new PrintStream(f2)
    val in = new InputStreamReader(new FileInputStream(f))
    try {
      val ast = semantics.parse(in, f.getParentFile(), Some(f.getName()))
      val tr = semantics.applyPasses(ast, Nil)
      val intr = semantics.interpreter()
      intr.run(tr, new DumpSample(out)).last
    } catch {
      case e => out.close; f2.delete; throw e
    } finally {
      out.close
      in.close
    }
  }
}

object Examples2012 extends Examples {
  override val expectLoc = "src/test/resources/acumen/data/examples-2012-res"
}

object Examples2013 extends Examples {
  override val expectLoc = "src/test/resources/acumen/data/examples-2013-res"
}

object Examples2014 extends Examples {
  override val expectLoc = "src/test/resources/acumen/data/examples-2014-res"
}
