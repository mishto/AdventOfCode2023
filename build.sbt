name := "AdventOfCode"
version := "0.0.1-SNAPSHOT"

scalaVersion := "2.13.6"
lazy val CirceVersion           = "0.14.2"
val monocleVersion = "2.0.4"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % "2.6.1",
  "org.typelevel" %% "cats-effect" % "3.3.14",
  "org.scalatest" %% "scalatest" % "3.2.13" % "test",
  "co.fs2" %% "fs2-core" % "3.6.1",
  "co.fs2" %% "fs2-io" % "3.6.1",
  "org.typelevel" %% "spire" % "0.18.0",
)

lazy val advent = (project in file("."))
