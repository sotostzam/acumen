name := "acumen"

version := "10-devel"

scalaVersion := "2.9.2"

theMainClass := "acumen.ui.GraphicalMain"

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-swing" % "2.9.2",
  "org.scalatest" %% "scalatest" % "2.0.M1" % "test",
  "org.scalacheck" %% "scalacheck" % "1.10.0" % "test"
)

resolvers += "tuxfamily" at "http://download.tuxfamily.org/arakhne/maven/"

libraryDependencies ++= Seq (
  "javax" % "j3d-core" % "1.5.2",
  "javax" % "j3d-core-utils" % "1.5.2",
  "javax" % "vecmath" % "1.5.2",
  "com.fifesoft" % "rsyntaxtextarea" % "2.0.2",
  "com.fifesoft" % "autocomplete" % "2.0.2"
)

resolvers ++= Seq(
   "snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
   "releases"  at "http://oss.sonatype.org/content/repositories/releases"
)

libraryDependencies ++= Seq( 
   "org.jfree" % "jfreechart" % "1.0.14",
   "org.jfree" % "jcommon" % "1.0.17"
)

// FIXME: Is this necessary
retrieveManaged := true

/* SCCT */
seq(ScctPlugin.instrumentSettings : _*)

//
// Exclude files that start with XXX from the jar file
//

mappings in (Compile,packageBin) ~= { (ms: Seq[(File, String)]) =>
  ms filter { case (file, toPath) =>
    !toPath.contains("/XXX")
  }
}

//
// enable proguard
//

seq(ProguardPlugin.proguardSettings :_*)

proguardDefaultArgs := Seq("-dontwarn", "-dontobfuscate")

// don't shrink as proguard gets it wrong in some cases
proguardDefaultArgs += "-dontshrink"

// make sure all target specific Java 3d dependencies are included
proguardOptions += "-keep class javax.media.j3d.**"

// temporary hack to get proguard working with enclosure code
proguardOptions ++= Seq("-keep class org.jfree.resources.**",
                        "-keep class org.jfree.chart.resources.**",
                        "-keep class org.fife.**")

// for faster jar creation (but larger file)
proguardDefaultArgs += "-dontoptimize"

// Do not include any signature files from other jars, they cause
// nothing but problems.
makeInJarFilter ~= {
  (makeInJarFilter) => {
    (file) => makeInJarFilter(file) + ",!**/*.RSA,!**/*.SF,!**/*.DSA"
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

//
// set main based on theMainClass setting
//

mainClass in (Compile,run) <<= theMainClass map { m => Some(m) }

mainClass in (Compile,packageBin) <<= mainClass in (Compile,run)

proguardOptions <<= (proguardOptions, theMainClass) {
  (prev, main) => prev :+ (keepMain(main))
}

fork in run := true

javaOptions in run += "-Xmx1G"

// disable parallel test Execution for now due to a possible race
// condition that needs to be tracked down and causes a hang
parallelExecution in Test := false

logBuffered in Test := false
