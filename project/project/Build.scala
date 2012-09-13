import sbt._
object PluginDef extends Build {
  override def projects = Seq(root)
  lazy val root = Project("plugins", file(".")) dependsOn(proguard)
  lazy val proguard = uri("git://github.com/effective-modeling/xsbt-proguard-plugin.git#4727f95d016024bf2eb1c2e9e3660e33263c364")
}
