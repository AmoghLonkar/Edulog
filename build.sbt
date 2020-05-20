organization := "edu.ucsc.soe"

version := "0.1"

name := "edulog"

scalaVersion := "2.12.4"

//scalacOptions ++= Seq("-deprecation", "-unchecked")

//libraryDependencies += "com.github.scopt" %% "scopt" % "3.6.0"

//libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.7.2"

//libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.3"

//libraryDependencies += "org.json4s" %% "json4s-native" % "3.5.3"

libraryDependencies += "org.scala-lang.modules"  %% "scala-parser-combinators" % "1.1.2"
libraryDependencies += "edu.berkeley.cs" %% "firrtl" % "1.1.3"


// Assembly

//assemblyJarName in assembly := "edulog.jar"

//assemblyOutputPath in assembly := file("./utils/bin/essent.jar")