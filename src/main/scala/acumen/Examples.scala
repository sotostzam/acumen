package acumen

import Errors._
import java.io._
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

  // FIXME: Get these locations from scala/java/sbt some how...
  val expectLoc = "src/test/resources/acumen/data/examples-res" 
  val gotLoc = "target/tmp/examples-res"

  def resultFile(loc: String, dn: String, f: File) =
    new File(new File(loc, dn), f.getName+".res")

  def writeExampleResult(loc: String, dn: String, f: File, run: Prog => CStoreRes) : Unit = {
    val d2 =new File(loc,dn)
    d2.mkdirs()
    val f2 = new File(d2, f.getName+".res")
    val out = new PrintStream(f2)
    val in = new InputStreamReader(new FileInputStream(f))
    val ast = Parser.run(Parser.prog, in)
    val tr = util.Transform.transform(ast)
    val trace = run(tr).ctrace
    CStoreRes(trace).dumpSample(out)
    out.close
    in.close
  }

}
