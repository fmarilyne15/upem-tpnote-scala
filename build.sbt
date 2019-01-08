name := "exam"

version := "0.1"

scalaVersion := "2.12.8"

libraryDependencies += "com.typesafe.akka" %% "akka-stream" % "2.5.19"
libraryDependencies += "com.typesafe.play" %% "play-json" % "2.7.0-RC2"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % Test
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.14.0" % Test
libraryDependencies += "com.typesafe.akka" %% "akka-stream-testkit" % "2.5.19" % Test