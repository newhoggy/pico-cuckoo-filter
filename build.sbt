name := "pico-cuckoo-filter"

organization := "io.john-ky"

crossScalaVersions := Seq("2.10.5", "2.11.6")

resolvers += "Scalap Releases" at "s3://dl.john-ky.io/maven/snapshots"

publishTo := Some("Scalap Releases" at "s3://dl.john-ky.io/maven/snapshots")

isSnapshot := true
