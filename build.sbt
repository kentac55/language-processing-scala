import Dependencies._

name := "language-processing-scala"

scalaVersion := "2.12.2"

libraryDependencies ++= Seq(
  scalaTest % "test",
  scalaCheck % "test"
)

val unusedWarnings = "-Ywarn-unused" ::
  "-Ywarn-unused-import" ::
  Nil

scalacOptions ++= (
  "-deprecation" ::
    "-unchecked" ::
    "-Xlint" ::
    "-Xfuture" ::
    "-language:existentials" ::
    "-language:higherKinds" ::
    "-language:implicitConversions" ::
    "-Yno-adapted-args" ::
    Nil
  ) ::: unusedWarnings

Seq(Compile, Test).flatMap(c =>
  scalacOptions in (c, console) --= unusedWarnings)

enablePlugins(ScalafmtPlugin)
