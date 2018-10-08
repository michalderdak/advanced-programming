import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.example",
      scalaVersion := "2.12.6",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "050-par",
    //libraryDependencies += scalaTest % Test

    libraryDependencies += "org.scalamock" %% "scalamock" % "4.1.0" % Test,
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.4" % Test,
  )
