ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.2.0"

lazy val root = (project in file("."))
  .settings(
    name := "fileswebserver",
    libraryDependencies ++= Seq(
      "com.electronwill.night-config" % "core" % "3.6.6",
      "com.electronwill.night-config" % "toml" % "3.6.6",
    )
  )
