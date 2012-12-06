package acumen

import java.io._

object Models {

  lazy val examplesDir = new File(getClass.getClassLoader.getResource("acumen/examples").getFile,
                                  "enclosure")

  def apply(name: String) = {
    val fn = name + ".acm"
    val src = scala.io.Source.fromFile(new File(examplesDir, fn))
    val str = src.mkString
    src.close
    str
  }

}
