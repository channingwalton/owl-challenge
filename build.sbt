organization := "channing"

name := "pwl-challenge"

version := "0.0.1"

scalaVersion := "2.11.6"

scalaBinaryVersion := "2.11"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-language:_", "-Xfatal-warnings", "-Xlint")

scalacOptions in Test ++= Seq("-Yrangepos")

libraryDependencies ++= Seq (
  "org.scalaz" %% "scalaz-core" % "7.1.3",
  "org.scalatest" %% "scalatest" % "2.2.4" % "test",
  "org.scalacheck" %% "scalacheck" % "1.12.4" % "test"
)