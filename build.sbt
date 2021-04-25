ThisBuild / scalaVersion := "2.13.5"
ThisBuild / version := "0.0.1"

lazy val mlgo = (project in file("."))
  // .enablePlugins(WriteOutputToFile)
  .settings(
    name := "Go via Machine Learning",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.2" % Test
  )
