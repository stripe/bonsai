scalaVersion := "2.11.7"

scalacOptions += "-feature"

lazy val root = project.
  in(file(".")).
  aggregate(bonsaiCore)

lazy val bonsaiCore = project.
  in(file("bonsai-core"))

lazy val bonsaiExample = project.
  in(file("bonsai-example")).
  dependsOn(bonsaiCore)

