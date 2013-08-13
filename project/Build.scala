import sbt._
import Keys._

object AcumenProject extends Build {
  val theMainClass = SettingKey[String]("the-main-class")

  lazy val root = Project(id = "acumen", base = file("."))
    .configs( FullTest )
    .settings( inConfig(FullTest)(Defaults.testTasks) : _*)
    .settings( testOptions in Test := Seq(Tests.Filter(testFilter)) )
    .settings( testOptions in FullTest := Seq(Tests.Filter(fullTestFilter)) )
  
  def slowTest(name:String) = name == "acumen.RandomTests" || name == "acumen.ExtractTest"
  def testFilter(name:String) = !slowTest(name)
  def fullTestFilter(name:String) = true
  
  lazy val FullTest = config("full") extend(Test)

  override lazy val settings = super.settings ++ (
    if (System.getProperty("os.name").toLowerCase == "mac os x") {
      Seq(javaOptions in run += "-Dapple.awt.graphics.UseQuartz=true")
    } else {
      Seq()
    })
}
