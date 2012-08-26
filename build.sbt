version := "0.0"

scalaVersion := "2.9.2"

resolvers ++= Seq(
   "snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
   "releases"  at "http://oss.sonatype.org/content/repositories/releases"
)

libraryDependencies ++= Seq( 
   "org.scalacheck" %% "scalacheck" % "1.10.0" % "test" classifier "sources",
   "org.scalacheck" %% "scalacheck" % "1.10.0" % "test",
   "org.scalatest" %% "scalatest" % "2.0.M3" % "test",
   "org.jfree" % "jfreechart" % "1.0.14",
   "org.jfree" % "jcommon" % "1.0.17",
   "org.scala-lang" % "scala-swing" % "2.9.2"
)

retrieveManaged := true
