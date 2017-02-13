name := "ArjenBot"

version := "1.0"

scalaVersion := "2.12.1"

resolvers += "Typesafe Releases" at "http://repo.typesafe.com/typesafe/releases"

libraryDependencies +=
  "com.typesafe.akka" %% "akka-actor" % "2.4.16"

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.8"
libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.5"

