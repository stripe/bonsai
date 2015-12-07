name := "bonsai-example"

libraryDependencies += Deps.jamm

fork in run := true

// Add the JAMM jar as an agent.
javaOptions in run := {
  val classPath = (dependencyClasspath in Compile).value
  val jammJar = classPath.collectFirst {
    case sbt.Attributed(jarFile) if jarFile.getName.endsWith("jamm-0.3.1.jar") =>
      jarFile.getAbsolutePath()
  }.get
  val oldOptions = (javaOptions in run).value
  val newOptions = oldOptions :+ s"-javaagent:${jammJar}"
  newOptions
}
