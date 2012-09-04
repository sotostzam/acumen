name := "acumen"

version := "10-devel"

scalaVersion := "2.9.2"

mainClass in (Compile,packageBin) := Some("acumen.ui.GraphicalMain")

mainClass in (Compile,run) := Some("acumen.ui.GraphicalMain")

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-swing" % "2.9.2",
  "org.scalatest" %% "scalatest" % "2.0.M1" % "test",
  "org.scalacheck" %% "scalacheck" % "1.10.0" % "test"
)

resolvers += "tuxfamily" at "http://download.tuxfamily.org/arakhne/maven/"

libraryDependencies ++= Seq (
  "javax" % "j3d-core" % "1.5.2",
  "javax" % "j3d-core-utils" % "1.5.2",
  "javax" % "vecmath" % "1.5.2"
)

resolvers ++= Seq(
   "snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
   "releases"  at "http://oss.sonatype.org/content/repositories/releases"
)

libraryDependencies ++= Seq( 
   "org.jfree" % "jfreechart" % "1.0.14",
   "org.jfree" % "jcommon" % "1.0.17",
   "com.itextpdf" % "itextpdf" % "5.3.0"
)

// FIXME: Is this necessary
retrieveManaged := true

/* SCCT */
seq(ScctPlugin.instrumentSettings : _*)

