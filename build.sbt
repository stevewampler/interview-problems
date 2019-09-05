name := "interview-problems"

version := "1.0"

scalaVersion := "2.12.7"

libraryDependencies ++= {
  val scalazVersion = "7.2.25"
  val algebird = "0.13.5"
  val specs2 = "4.5.1"
  val scalatest = "3.0.8"
  val logback = "1.2.3"
  val slf4j = "1.7.26"
  val junit = "4.12"
  val scalaParserCombinators = "2.11.0-M4"

  Seq(
    "ch.qos.logback" % "logback-classic" % logback,
    "org.scalatest" %% "scalatest" % scalatest % Test,
    "org.specs2" %% "specs2-core" % specs2 % Test,
    "com.twitter" %% "algebird-core" % algebird,
    "org.slf4j" % "slf4j-api" % slf4j,
    "org.scalaz" %% "scalaz-core" % scalazVersion,
     "org.scalaz"  %% "scalaz-concurrent" % scalazVersion,
    "junit" % "junit" % junit % Test,
    "org.scala-lang" % "scala-parser-combinators" % scalaParserCombinators
  )
}
    