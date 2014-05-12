import sbt._
import Keys._

object AcumenProject extends Build {
  val theMainClass = SettingKey[String]("the-main-class")
  
  // all: All tests broken or otherwise, mainly used as all:test-only
  lazy val AllTest = config("all") extend(Test)

  // full: The main testsuite
  lazy val FullTest = config("full") extend(Test)
  def fullTestFilter(name: String) = !(brokenTests ++ experimentalTests).contains(name)

  // quick: Quick tests that test for essential functionality
  lazy val QuickTest = config("quick") extend(Test)
  def quickTestFilter(name: String) = !(slowTests ++ brokenTests ++ experimentalTests).contains(name)

  // slow: Tests that may take awhile
  lazy val SlowTest = config("slow") extend(Test)
  val slowTests = Set("acumen.testutil.TransformationTestUtilTest",
                      // ^^ not really a "slow" test but doesn't work after the XXX
                      //    examples are removed (i.e. in the release script), so 
                      //    put it here to prevent from being in the "quick" test
                      //    that the release script uses.
                      "acumen.ExtractBaseTest", "acumen.ExtractEnclosureTest")
  def slowTestFilter(name: String) = slowTests.contains(name)

  // broken: Currently broken tests that should eventually be fixed
  lazy val BrokenTest = config("broken") extend(Test)
  val brokenTests = Set("acumen.RandomTests","acumen.testutil.ProgGeneratorTest","acumen.ConsistencyTest")
  def brokenTestFilter(name: String) = brokenTests.contains(name)

  // experimental: Failing tests that relate to experimental code
  lazy val ExperimentalTest = config("reference") extend(Test)
  val experimentalTests = Set("acumen.FlattenSimpleTest")
  def experimentalTestFilter(name: String) = experimentalTests.contains(name)

  lazy val root = Project(id = "acumen", base = file("."))

    .configs( AllTest,FullTest,QuickTest,SlowTest,BrokenTest,ExperimentalTest )

    .settings( testOptions in Test := Seq(Tests.Filter(fullTestFilter)) )

    .settings( inConfig(AllTest)(Defaults.testTasks) : _*)

    .settings( inConfig(FullTest)(Defaults.testTasks) : _*)
    .settings( testOptions in FullTest := Seq(Tests.Filter(fullTestFilter)) )

    .settings( inConfig(QuickTest)(Defaults.testTasks) : _*)
    .settings( testOptions in QuickTest := Seq(Tests.Filter(quickTestFilter)) )

    .settings( inConfig(SlowTest)(Defaults.testTasks) : _*)
    .settings( testOptions in SlowTest := Seq(Tests.Filter(slowTestFilter)) )

    .settings( inConfig(BrokenTest)(Defaults.testTasks) : _*)
    .settings( testOptions in BrokenTest := Seq(Tests.Filter(brokenTestFilter)) )

    .settings( inConfig(ExperimentalTest)(Defaults.testTasks) : _*)
    .settings( testOptions in ExperimentalTest := Seq(Tests.Filter(experimentalTestFilter)) )
  
  override lazy val settings = super.settings ++ (
    if (System.getProperty("os.name").toLowerCase == "mac os x") {
      Seq(javaOptions in run += "-Dapple.awt.graphics.UseQuartz=true")
    } else {
      Seq()
    })
}
