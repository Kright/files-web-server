ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.6.4"

lazy val root = (project in file("."))
  .settings(
    name := "fileswebserver",
    libraryDependencies ++= Seq(
      "com.electronwill.night-config" % "core" % "3.8.2",
      "com.electronwill.night-config" % "toml" % "3.8.2",
    )
  )
