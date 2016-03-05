organization := "channing"

name := "owl-challenge"

version := "0.0.1"

scalaVersion := "2.11.7"

scalaBinaryVersion := "2.11"

scalacOptions ++= Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-feature",
  "-language:existentials",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-language:postfixOps",
  "-unchecked",
  "-Xfatal-warnings",
  "-Xlint",
  "-Yinline-warnings",
  "-Yno-adapted-args",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
  "-Ywarn-value-discard",
  "-Xfuture")

scalacOptions in Test ++= Seq("-Yrangepos")

mainClass in Compile := Some("owl.OwlChallenge")

libraryDependencies ++= Seq (
  "org.scala-lang.modules" %% "scala-java8-compat" % "0.5.0" % "compile",
  "org.typelevel" %% "cats" % "0.4.1",
  "org.scalatest" %% "scalatest" % "2.2.5" % "test",
  "org.scalacheck" %% "scalacheck" % "1.12.5" % "test"
)
