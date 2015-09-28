name := "interview-problems"

version := "1.0"

scalaVersion := "2.10.4"

libraryDependencies ++= Seq(
  // "net.liftweb" % "lift-json_2.10" % "2.6.2",
  "io.spray" % "spray-can" % "1.1-M8",
  "io.spray" % "spray-http" % "1.1-M8",
  "io.spray" % "spray-routing" % "1.1-M8",
  "net.liftweb" %% "lift-json" % "2.5.1",
  // "com.typesafe.slick" %% "slick" % "1.0.1",
  // "mysql" % "mysql-connector-java" % "5.1.25",
  "com.typesafe.akka" %% "akka-actor" % "2.1.4",
  "com.typesafe.akka" %% "akka-slf4j" % "2.1.4",
  "ch.qos.logback" % "logback-classic" % "1.0.13",
  // "org.scalatest" % "scalatest_2.10" % "2.2.1",
  // "org.scalatest" %% "scalatest" % "2.2.1",
  "org.scalatest" %% "scalatest" % "latest.release" % "test",
  "org.specs2" %% "specs2-core" % "3.6.4" % "test",
  // "net.databinder.dispatch" %% "dispatch-core" % "0.11.2"
  "com.twitter" %% "algebird-core" % "0.9.0",
//  "org.scalaz"  %% "scalaz-core" % "7.0.0",
//  "org.scalaz"  %% "scalaz-concurrent" % "7.0.0"
  "org.scalaz"  %% "scalaz-core" % "latest.release",
  "org.scalaz"  %% "scalaz-concurrent" % "latest.release"
)

resolvers ++= Seq(
  "Spray repository" at "http://repo.spray.io",
  "Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/"
)
    