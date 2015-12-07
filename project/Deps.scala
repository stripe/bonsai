import sbt._

object Deps {

  // Test
  val scalaTest = "org.scalatest" %% "scalatest" % "2.2.4" % "test"
  val scalaCheck = "org.scalacheck" %% "scalacheck" % "1.12.5" % "test"

  // Example
  val jamm = "com.github.jbellis" % "jamm" % "0.3.1"
}
