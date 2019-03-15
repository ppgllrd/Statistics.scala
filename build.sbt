name := "statistics"

version := "0.1"


val commonSettings = Seq(
  scalaVersion := "2.11.0",
  scalacOptions ++= Seq("-deprecation", "-feature"),
  libraryDependencies ++= Seq(
    "org.scala-lang" % "scala-reflect" % scalaVersion.value,
    "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.1"
  )
)

lazy val macros = project.in(file("macros")).settings(commonSettings : _*)

lazy val statistics = project.in(file(".")).settings(commonSettings : _*).dependsOn(macros)
