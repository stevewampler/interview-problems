name := "interview-problems"

version := "1.0"

scalaVersion := "2.11.6"

libraryDependencies ++= {
  val akkaVersion = "2.3.9"
  val sprayVersion = "1.3.4"
  val liftVersion = "3.3.0"
  val scalazVersion = "7.2.25"

  Seq(
    "io.spray" %% "spray-can" % sprayVersion,
    "io.spray" %% "spray-http" % sprayVersion,
    "io.spray" %% "spray-routing" % sprayVersion,
    "com.typesafe.akka" %% "akka-actor" % akkaVersion,
    "com.typesafe.akka" %% "akka-slf4j" % akkaVersion,
    "net.liftweb" %% "lift-json" % liftVersion,
    "ch.qos.logback" % "logback-classic" % "1.0.13",
    "org.scalatest" %% "scalatest" % "2.2.6" % "test",
    "org.specs2" %% "specs2-core" % "3.6.4" % "test",
    "com.twitter" %% "algebird-core" % "0.11.0",
    "org.slf4j" % "slf4j-api" % "1.7.25",
    "org.scalaz" %% "scalaz-core" % scalazVersion,
     "org.scalaz"  %% "scalaz-concurrent" % scalazVersion,
//    "org.scalaz.stream" %% "scalaz-stream" % "0.7a",
    "junit" % "junit" % "4.12" % Test,
    "org.scala-lang" % "scala-parser-combinators" % "2.11.0-M4"
  )
}

resolvers += "spray repo" at "http://repo.spray.io"

//resolvers += DefaultMavenRepository

//resolvers ++= Seq(
////  "Spray repository" at "http://repo.spray.io",
////  "Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/",
////  "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases"
//)
    