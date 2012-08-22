version := "0.0"

scalaVersion := "2.9.2"

resolvers ++= Seq(
   "snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
   "releases"  at "http://oss.sonatype.org/content/repositories/releases"
)

libraryDependencies ++= Seq( 
   "org.scalacheck" %% "scalacheck" % "1.10.0" % "test",
   "org.scalatest" %% "scalatest" % "2.0.M3" % "test"
)

