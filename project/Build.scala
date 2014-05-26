import sbt._
import Keys._
import sbtassembly.Plugin._
import AssemblyKeys._ 

object ChessBuild extends Build {
  lazy val root = Project(id = "root", base = file("."), settings = projectSettings)
      .settings(libraryDependencies ++= Seq(scalaTest, akkaActor))
  
  val projectSettings = Project.defaultSettings ++ assemblySettings ++ 
    Seq(
      scalaVersion := "2.10.4",
      crossPaths := false,
      testOptions in Test += Tests.Argument("-oD"),
      test in assembly := (),
      jarName in assembly := "chess.jar"
    ) 
  
  val scalaTest = "org.scalatest" %% "scalatest" % "2.1.6" % "test"
  
  val akkaActor = "com.typesafe.akka" %% "akka-actor" % "2.3.3"
  
  val akkaRemote = "com.typesafe.akka" %% "akka-remote" % "2.3.3"
} 