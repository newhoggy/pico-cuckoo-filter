import sbt._
import Keys._

object Multibuild extends Build with Version {
  implicit class ProjectOps(self: Project) {
    def standard: Project = {
      self
          .settings(organization := "io.john-ky")
          .settings(scalacOptions := Seq(
            "-feature",
            "-deprecation",
            "-unchecked",
            "-Xlint",
            "-Yrangepos",
            "-encoding",
            "utf8"))
          .settings(scalacOptions in (console) += "-Yrangepos")
          .settings(resolvers += "scalaz-bintray" at "http://dl.bintray.com/scalaz/releases")
    }

    def notPublished: Project = {
      self
          .settings(publish := {})
          .settings(publishArtifact := false)
    }

    def published: Project = {
      self
          .settings(publishTo in ThisBuild := Some("Scalap Releases" at "s3://dl.john-ky.io/maven/releases"))
          .settings(isSnapshot in ThisBuild  := true)
    }

    def specs2: Project = {
      self
          .settings(libraryDependencies ++= Seq(
              "org.specs2"      %% "specs2-core"        % "3.6.2"   % "test",
              "org.specs2"      %% "specs2-scalacheck"  % "3.6.2"   % "test",
              "org.scalacheck"  %% "scalacheck"         % "1.12.4"  % "test"))
          .settings(scalacOptions in Test ++= Seq("-Yrangepos"))
    }

    def dependsOnAndAggregates(projects: Project*): Project = {
      val dependencies = projects.map(pr => pr: sbt.ClasspathDep[sbt.ProjectReference])
      val aggregates = projects.map(pr => pr: sbt.ProjectReference)
      self.dependsOn(dependencies: _*).aggregate(aggregates: _*)
    }
  }

  lazy val `pico-twiddle` = Project(id = "pico-twiddle", base = file("pico-twiddle"))
      .standard
      .published
      .specs2

  lazy val `pico-cuckoo-hashtable` = Project(id = "pico-cuckoo-hashtable", base = file("pico-cuckoo-hashtable"))
      .standard
      .published
      .specs2
      .settings(libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.1.2")
      .settings(addCompilerPlugin("org.spire-math" % "kind-projector" % "0.6.0" cross CrossVersion.binary))

  lazy val `pico-cuckoo-filter` = Project(id = "pico-cuckoo-filter", base = file("pico-cuckoo-filter"))
      .standard
      .published
      .specs2
      .settings(libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.1.2")
      .settings(addCompilerPlugin("org.spire-math" % "kind-projector" % "0.6.0" cross CrossVersion.binary))

  lazy val root = Project(id = "all", base = file("."))
      .notPublished
      .aggregate(`pico-cuckoo-hashtable`, `pico-cuckoo-filter`)
}
