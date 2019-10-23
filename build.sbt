import Dependencies._

name := "scala-first-protobuf-parent"
ThisBuild / organization := "pl.shumikowo.s1pb"

ThisBuild / version := "0.1.2"
ThisBuild / scalaVersion := "2.12.10"

Global / cancelable := true

lazy val commonSettings = Seq(
  Compile / packageDoc / publishArtifact := false,
  Test / packageBin / publishArtifact := false,

  crossPaths := true,
  update / aggregate := false,
  update / evictionWarningOptions := EvictionWarningOptions.default
    .withWarnTransitiveEvictions(false)
    .withWarnDirectEvictions(false),

  // Compilation options
  scalacOptions ++= Seq(
    "-target:jvm-1.8",
    "-encoding", "UTF-8",
    "-unchecked",
    "-deprecation",
    "-Xfuture",
//    "-Ywarn-unused",
    "-Xfatal-warnings",
    "-language:higherKinds",
    "Ypartial-unification"
  ),
)

addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full)

lazy val core = Project(id = "scala-first-protobuf", base = file("modules/s1pb"))
  .settings(commonSettings: _*)
  .settings(libraryDependencies ++= Seq(magnolia, protobufJava, protobufJava % "protobuf") ++
    (Seq(commonsIo) ++ allLogging ++ allSpecs ).map(_ % Test))

lazy val tests = Project(id = "scala-first-protobuf-tests", base = file("modules/s1pb-tests"))
  .enablePlugins(AkkaGrpcPlugin)
  .settings(commonSettings: _*)
  .settings(libraryDependencies ++= Seq(magnolia, protobufJava, protobufJava % "protobuf") ++
    (Seq(commonsIo) ++ allLogging ++ allSpecs ).map(_ % Test))
  .settings(Compile / PB.protoSources := Seq(core.base / "target" / "protobuf"))
  .settings(Compile / PB.targets := (Compile / PB.targets).value.take(1))
  .settings(Compile / PB.generate := (Compile / PB.generate dependsOn core/Test/test).value)
  .dependsOn(core % "compile->compile;test->test")

