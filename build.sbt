organization in ThisBuild := "com.stripe"
scalaVersion in ThisBuild := "2.11.7"
crossScalaVersions in ThisBuild := Seq("2.10.6", "2.11.7")

scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-unchecked",
  "-optimize"
)

autoAPIMappings in ThisBuild := true
maxErrors in ThisBuild := 8

val unpublished = Seq(publish := (), publishLocal := (), publishArtifact := false)

lazy val root = project.
  in(file(".")).
  aggregate(bonsaiCore).
  settings(unidocSettings: _*).
  settings(unpublished: _*)

lazy val bonsaiCore = project.
  in(file("bonsai-core"))

lazy val bonsaiExample = project.
  in(file("bonsai-example")).
  dependsOn(bonsaiCore)
