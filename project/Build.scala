import sbt._
import Keys._

object AcumenProject extends Build {
       val theMainClass = SettingKey[String]("the-main-class")

       lazy val root = Project(id = "acumen", base = file("."))

  override lazy val settings = super.settings ++ (
    if (System.getProperty("os.name").toLowerCase == "mac os x") {
      Seq(javaOptions in run += "-Dapple.awt.graphics.UseQuartz=true")
    } else {
      Seq()
    })
}
