scalaVersion in ThisBuild := "2.11.6"

crossScalaVersions := Seq("2.10.5", "2.11.6")

resolvers in ThisBuild ++= Seq(
  "bintray/non"           at "http://dl.bintray.com/non/maven",
  "dl-john-ky-releases"   at "http://dl.john-ky.io/maven/releases",
  "dl-john-ky-snapshots"  at "http://dl.john-ky.io/maven/snapshots")

version in ThisBuild := buildVersion
