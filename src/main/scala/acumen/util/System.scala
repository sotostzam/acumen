package acumen.util

object System {
  val version = {
    val md = getClass.getClassLoader.getResource("acumen/version")
    if (md == null) "unknown version"
    else { io.Source.fromFile(md.getFile).mkString }
  }
  //val version = "10.10.25"
  val tutorialUrl = 
    new java.net.URI("http://www.acumen-language.org/p/tutorials.html")
}

