import sbt._

object Deps {

  // Test
  val scalaTest = "org.scalatest" %% "scalatest" % "3.0.1" % "test"
  val scalaCheck = "org.scalacheck" %% "scalacheck" % "1.13.4" % "test"

  // Example
  val jamm = "com.github.jbellis" % "jamm" % "0.3.1"
}
