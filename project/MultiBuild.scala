import sbt._
import Keys._

object MultiBuild extends Build with Version {
  val specs2_core       = "org.specs2"      %% "specs2-core"        % "3.6.2"   % "test"
  val specs2_scalacheck = "org.specs2"      %% "specs2-scalacheck"  % "3.6.2"   % "test"
  val scalacheck        = "org.scalacheck"  %% "scalacheck"         % "1.12.4"  % "test"
  val scalaz_core       = "org.scalaz"      %% "scalaz-core"        % "7.1.2"

  implicit class ProjectOps(self: Project) {
    def standard: Project = {
      self
          .settings(organization := "io.john-ky")
          .settings(scalacOptions := Seq("-feature", "-deprecation", "-unchecked", "-Xlint", "-Yrangepos", "-encoding", "utf8"))
          .settings(scalacOptions in Test := Seq("-Yrangepos"))
          .settings(resolvers += "scalaz-bintray" at "http://dl.bintray.com/scalaz/releases")
    }

    def libs(modules: ModuleID*) = self.settings(libraryDependencies ++= modules)

    def notPublished: Project = self.settings(publish := {}).settings(publishArtifact := false)

    def published: Project = self
        .settings(publishTo in ThisBuild := Some("Scalap Releases" at "s3://dl.john-ky.io/maven/releases"))
        .settings(isSnapshot in ThisBuild := true)
  }

  lazy val `pico-twiddle` = Project(id = "pico-twiddle", base = file("pico-twiddle"))
      .standard.published
      .libs(specs2_core, specs2_scalacheck, scalacheck)

  lazy val `pico-hash` = Project(id = "pico-hash", base = file("pico-hash"))
      .standard.published
      .libs(scalaz_core)
      .libs(specs2_core, specs2_scalacheck, scalacheck)

  lazy val `pico-cuckoo-hashtable` = Project(id = "pico-cuckoo-hashtable", base = file("pico-cuckoo-hashtable"))
      .standard.published
      .libs(scalaz_core)
      .libs(specs2_core, specs2_scalacheck, scalacheck)
      .dependsOn(`pico-twiddle`, `pico-hash`)

  lazy val `pico-cuckoo-filter` = Project(id = "pico-cuckoo-filter", base = file("pico-cuckoo-filter"))
      .standard.published
      .libs(specs2_core, scalaz_core)
      .dependsOn(`pico-hash`)

  lazy val root = Project(id = "all", base = file("."))
      .notPublished
      .aggregate(`pico-cuckoo-hashtable`, `pico-cuckoo-filter`, `pico-twiddle`)
}
