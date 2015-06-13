lazy val releaseVersion = "0.0.1"

lazy val githash = Process("git rev-parse --short HEAD").lines.head

version in ThisBuild := s"$releaseVersion-$githash"
