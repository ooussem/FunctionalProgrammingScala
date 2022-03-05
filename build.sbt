lazy val root = project
  .in(file("."))
  .settings(
    name := "Functional-Programming-Scala",
    version := "0.1",
    scalaVersion := "2.13.3"
)

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.2" % "test"