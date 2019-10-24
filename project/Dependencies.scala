import sbt._

object Dependencies {
  val magnolia = "com.propensive" %% "magnolia" % "0.12.0"

  val protobufJava = "com.google.protobuf" % "protobuf-java" % "3.10.0"

  val commonsIo = "commons-io" % "commons-io" % "2.6"

  val scalaLogging = "com.typesafe.scala-logging" %% "scala-logging" % "3.9.2"
  val log4jApi = "org.apache.logging.log4j" % "log4j-api" % "2.11.2"
  val log4jSlf4j = "org.apache.logging.log4j" % "log4j-slf4j-impl" % "2.11.2"
  val slf4j = "org.slf4j" % "slf4j-api" % "1.7.26"
  val lmaxDisruptor = "com.lmax" % "disruptor" % "3.4.2"
  val allLogging = Seq(slf4j, log4jApi, log4jSlf4j, lmaxDisruptor, scalaLogging)

  val specs2Version = "4.5.1"
  val specsCore = "org.specs2" %% "specs2-core" % specs2Version
  val specsScalaCheck = "org.specs2" %% "specs2-scalacheck" % specs2Version
  val allSpecs = Seq(specsCore, specsScalaCheck)

  val cats = "org.typelevel" %% "cats-core" % "2.0.0"
  val allCats = Seq(cats)

}
