name := "acumen"

version := "10-devel"

scalaVersion := "2.9.2"

mainClass in (Compile,run) := Some("acumen.ui.GraphicalMain")

mainClass in (Compile,packageBin) <<= mainClass in (Compile,run)

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

proguardOptions += keepMain("acumen.ui.GraphicalMain")

proguardDefaultArgs := Seq("-dontwarn", "-dontobfuscate")

// for faster jar creation (but larger file)
proguardDefaultArgs += "-dontoptimize"

// this filters out sun's and arakhne signature files that are otherwise understood at
// acumen's jar signature, as they are copied in META-INF by proguard */
makeInJarFilter ~= {
  (makeInJarFilter) => {
    (file) => makeInJarFilter(file) + ",!**/SUN_MICR.RSA,!**/SUN_MICR.SF,!**/ARAKHNE_.DSA,!**/ARAKHNE_.SF"
    }
  }


// modify package(-bin) jar file name
artifactPath in (Compile, packageBin) <<= (crossTarget, moduleName, version) {
  (path, name, ver) => path / (name + "-" + ver + ".pre.jar")
}

// modify proguard jar file name
minJarPath <<= (crossTarget, moduleName, version) {
  (path, name, ver) => path / (name + "-" + ver + ".jar")
}
