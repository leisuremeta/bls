
val scala3Version = "3.2.2"

lazy val root = project
  .in(file("."))
  .settings(
    name := "test_bls",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies += "org.web3j" % "core" % "5.0.0",
    libraryDependencies += "com.google.protobuf" % "protobuf-java" % "3.18.1",

    libraryDependencies += "qa.hedgehog" %% "hedgehog-munit" % "0.10.0",

    testFrameworks += TestFramework("munit.runner.Framework"),
  )
