import sbt._
import Keys._

object AcumenProject extends Build {
       val theMainClass = SettingKey[String]("the-main-class")

       lazy val root = Project(id = "acumen", base = file("."))
}
