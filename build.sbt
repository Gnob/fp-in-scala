libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.1"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"

lazy val root = (project in file("."))
  .settings(
    name := "fp-in-scala",
    scalaVersion := "2.12.1",
    version := "1.0",
    organization := "fpinscala"
  )

