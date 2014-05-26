import sbt._
import Keys._
import sbtassembly.Plugin._
import AssemblyKeys._ 

object ChessBuild extends Build {
  lazy val recursive = Project(id = "recursive", base = file("."), settings = projectSettings)
      .settings(jarName in assembly := "recursive.jar")
      .settings(libraryDependencies += scalaTest)
  
  val projectSettings = Project.defaultSettings ++ assemblySettings ++ 
    Seq(
      scalaVersion := "2.10.4",
      crossPaths := false,
      testOptions in Test += Tests.Argument("-oD"),
      test in assembly := ()
    ) 
  
  val scalaTest = "org.scalatest" %% "scalatest" % "2.1.6" % "test"
} 