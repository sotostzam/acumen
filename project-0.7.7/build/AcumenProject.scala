import sbt._
import java.util.jar.Attributes.Name._
import BasicScalaProject._

class AcumenProject(info: ProjectInfo) extends DefaultProject(info) 
  with ProguardProject 
//  with AutoCompilerPlugins 
  with GoogleCodeTasks  {

  // dependencies
  val sonatypeR = "releases"  at "http://oss.sonatype.org/content/repositories/releases"
  val sonatypeS = "snapshots" at "http://oss.sonatype.org/content/repositories/snapshots"  
//  val mavenRepo = "http://mvnrepository.com/artifact"

  val scalaSwing = "org.scala-lang" % "scala-swing" % "2.9.2"
  val scalaTest = "org.scalatest" %% "scalatest" % "2.0.M1" % "test"
  val scalaCheck = "org.scalacheck" %% "scalacheck" % "1.10.0" % "test"
//  val jFreeChart = "org.jfree" %% "jfreechart" % "1.0.14"
//  val iTextPDF = "com.itextpdf" %% "itextpdf" % "5.3.0"

  //val sxr = compilerPlugin("org.scala-tools.sxr" %% "sxr" % "0.2.7")
  
  /* proguard is used for 
       1. easily creating a standalone jar (thanks to proguardInJars)
       2. getting rid of the unused classes in that jar (13Mb -> 2 Mb) */
  override def proguardOptions = List(
    "-dontobfuscate",
    "-keep public class acumen.ui.GraphicalMain { public static void main(java.lang.String[]); }"
  ) 

  // java 3d deps.

  val tuxfamily = "tuxfamily" at "http://download.tuxfamily.org/arakhne/maven/"  

  val j3dCore = "javax" % "j3d-core" % "1.5.2"
  val j3dCoreUtils = "javax" % "j3d-core-utils" % "1.5.2"
  val vecmath = "javax" % "vecmath" % "1.5.2"
  
  /* this filters out sun's and arakhne signature files that are otherwise understood at
     acumen's jar signature, as they are copied in META-INF by proguard */
  override def makeInJarFilter (file :String) =
    super.makeInJarFilter(file) + ",!**/SUN_MICR.RSA,!**/SUN_MICR.SF,!**/ARAKHNE_.DSA,!**/ARAKHNE_.SF"
 
  // add scala libraries to the generated jar
  override def proguardInJars = super.proguardInJars +++ scalaLibraryPath
  override def minJarName = artifactBaseName + ".jar"

  override def testOptions = 
    // restrict size of randomly generated programs to '20'
    super.testOptions ++ Seq(TestArgument(TestFrameworks.ScalaCheck, "-x", "20")) 

  // link api doc to source -- doesn't work
  //override def documentOptions =
    //super.documentOptions ++ Seq(LinkSource)

  override def mainClass = Some("acumen.ui.GraphicalMain")
  
  override def compileOptions = 
    super.compileOptions ++ 
    Seq(Deprecation
       ,Unchecked 
       /*, Optimize */
       ,target(Target.Java1_5)
       /* enable X-Ray */
       //,CompileOption("-P:sxr:base-directory:" +mainScalaSourcePath.absolutePath)
    )

  override def compileAction = super.compileAction dependsOn(writeVersion)

  /* modified package-project to produce a zip as we intend it */
	override def moduleID = normalizedName
	override def jarPath = outputPath / (artifactBaseName + ".pre.jar")
  protected def acumenZip = outputPath / (artifactBaseName + ".zip")

  protected def atZipRoot = 
    (((outputPath ##) / defaultJarName)
     +++ path("README")
     +++ path(".") * "LICENSE*"
     +++ (mainResourcesPath / "acumen" ##) / "examples" * "*.acm")

  /* we override the version number with one based on date */
  override def version = {
    import java.util.Calendar
    val c = Calendar.getInstance
    val day = c.get(Calendar.DAY_OF_MONTH)
    val month = c.get(Calendar.MONTH) + 1
    val year = c.get(Calendar.YEAR) - 2000
    BasicVersion(year, Some(month), Some(day), None) 
  }

  /* write the version in the right file */
  lazy val versionFile = mainResourcesPath / "acumen" / "version"
  def writeVersionAction = task {
    try {
      val out = new java.io.FileWriter(versionFile.absolutePath)
      out.write(version.toString)
      out.close
      None
    } catch {
      case e => Some(e.toString)
    }
  }
  lazy val writeVersion = writeVersionAction

  /* we override the default packaging behaviour of sbt with ours */
  override def packageProjectAction = fileTask(Seq(acumenZip)) {
    import FileUtilities._
    createTemporaryDirectory(info.logger) match {
      case Left(m) => Some(m)
      case Right(tmp) => 
        val tmpPath = Path.fromFile(tmp)  
        val acm = tmpPath / artifactBaseName
        createDirectory(acm, info.logger) match {
          case Some(m) => Some(m)
          case None => 
            val sources = acm / "sources"
            createDirectory(sources, info.logger) match {
              case Some(m) => Some(m)
              case None => 
                copy(atZipRoot.get, acm, info.logger) match {
                  case Left(m) => Some(m)
                  case Right(_) =>
                    copy(packageProjectPaths.get, sources , info.logger) match {
                      case Left(m) => Some(m)
                      case Right(_) =>
                        zip (((tmpPath ##) / artifactBaseName ** "*").get, 
                             acumenZip, false, info.logger) match {
                          case Some(m) => Some(m)
                          case None =>
                            FileUtilities.clean(tmpPath, info.logger)
                        }
                    }
                }
            }
        }
    } 
  } dependsOn(proguard)

  /* google code upload */
  val uploadUsage = "Usage: upload <username> <password>"
  val uploadDescription = 
    "Upload acumen to google code (creates a new release). " + uploadUsage
  def uploadAction = task { args =>
    if(args.length == 2) {
      // run tests before allowing for any upload
      test && googleCodeUploadTask(
        args(0), args(1), acumenZip.absolutePath, "acumen-language", 
        acumenZip.name, new java.util.Date + " release", "OpSys-All, Type-Archive, Featured").
      dependsOn(packageProject).
      describedAs(uploadDescription)
    } else task {
      Some(uploadUsage)
    }
  }

  lazy val upload = uploadAction

}
