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

// enable proguard

seq(ProguardPlugin.proguardSettings :_*)

proguardOptions ++= Seq(
  keepMain("acumen.ui.GraphicalMain")
)

makeInJarFilter ~= {
  (makeInJarFilter) => {
    (file) => makeInJarFilter(file) + ",!**/SUN_MICR.RSA,!**/SUN_MICR.SF,!**/ARAKHNE_.DSA,!**/ARAKHNE_.SF"
    }
  }

