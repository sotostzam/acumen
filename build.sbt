name := "acumen"

version := "10-devel"

scalaVersion := "2.11.5"

theMainClass := "acumen.Main"

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-swing" % "1.0.1",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.2",
  "org.scala-lang.modules" %% "scala-xml" % "1.0.2",
  "org.scala-lang" % "scala-actors" % "2.11.5",
  "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test",
  "org.scalacheck" %% "scalacheck" % "1.10.1" % "test"
)

resolvers += "tuxfamily" at "http://download.tuxfamily.org/arakhne/maven/"

libraryDependencies ++= Seq (
  "com.fifesoft" % "rsyntaxtextarea" % "2.5.3",
  "com.fifesoft" % "autocomplete" % "2.5.0"
)

resolvers += "acumen" at "https://bitbucket.org/effective/acumen_maven-repo/raw/master/repository/"

libraryDependencies ++= Seq (
  "com.rapid_i" % "jpct" % "1.28"
)

resolvers ++= Seq(
   "snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
   "releases"  at "http://oss.sonatype.org/content/repositories/releases"
)

libraryDependencies ++= Seq( 
   "org.jfree" % "jfreechart" % "1.0.14",
   "org.jfree" % "jcommon" % "1.0.17"
)

// Add resources in project root directory to class path
unmanagedResources in Compile ++= Seq("AUTHORS","LICENSE","LICENSE-Rice","LICENSE-AIC").map(new File(_))

// FIXME: Is this necessary
retrieveManaged := true

// SCCT
// seq(ScctPlugin.instrumentSettings : _*)

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

mainClass in Compile <<= theMainClass map { m => Some(m) }

proguardOptions <<= (proguardOptions, theMainClass) {
  (prev, main) => prev :+ (keepMain(main))
}

fork in run := true

javaOptions in run += "-Xmx1G"

// disable parallel test Execution for now due to a possible race
// condition that needs to be tracked down and causes a hang
parallelExecution in Test := false

logBuffered in Test := false

// enable improved (experimental) incremental compilation algorithm called "name hashing"
incOptions := incOptions.value.withNameHashing(true)
