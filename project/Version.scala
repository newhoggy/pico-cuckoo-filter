import sbt._
import Keys._

trait Version {
  lazy val releaseVersion = List("0.0.1")

  lazy val gitSha = Process("git rev-parse --short HEAD").lines.take(1).toList

  lazy val username = Option(System.getProperty("user.name")).toList

  lazy val localChanges: List[String] = {
    val stagedChanges = ("git diff-index --quiet HEAD" !) != 0
    val untrackedChanges = "git ls-files --other --exclude-standard".!! != ""
    if (stagedChanges || untrackedChanges) {
      username :+ "-SNAPSHOT"
    } else {
      List.empty[String]
    }
  }

  lazy val buildVersion = {
    implicit class ListStringOps(self: List[String]) {
      def ?:(delim: String): List[String] = if (self.nonEmpty) delim :: self else self
    }
    (releaseVersion ++ ("-" ?: gitSha) ++ ("-" ?: localChanges)).mkString
  }
}
