organization := "channing"

name := "pwl-challenge"

version := "0.0.1"

scalaVersion := "2.11.7"

scalaBinaryVersion := "2.11"

scalacOptions ++= Seq(
  "-target:jvm-1.8",
  "-Ydelambdafy:method",
  "-Ybackend:GenBCode",
  "-Yopt:l:classpath",
  "-deprecation",
  "-encoding", "UTF-8",
  "-feature",
  "-language:_",
  "-unchecked",
  "-Xfatal-warnings",
  "-Xlint",
  "-Yno-adapted-args",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
  "-Ywarn-value-discard",
  "-Xfuture")

scalacOptions in Test ++= Seq("-Yrangepos")

libraryDependencies ++= Seq (
  "org.scala-lang.modules" %% "scala-java8-compat" % "0.5.0" % "compile",
  "org.scalaz" %% "scalaz-core" % "7.1.3",
  "org.scalatest" %% "scalatest" % "2.2.4" % "test",
  "org.scalacheck" %% "scalacheck" % "1.12.4" % "test"
)