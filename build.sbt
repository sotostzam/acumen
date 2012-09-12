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
  "javax" % "vecmath" % "1.5.2"
)

// enable proguard

seq(ProguardPlugin.proguardSettings :_*)

proguardDefaultArgs := Seq("-dontwarn", "-dontobfuscate")

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

