val V = new {
  val organiseImports = "0.6.0"
  val iron = "2.3.0"
}

val scala3Version = "3.3.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "test_bls",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test,
    libraryDependencies += "io.github.iltotore" %% "iron" % V.iron,

  )

ThisBuild / scalafixDependencies += "com.github.liancheng" %% "organize-imports" % V.organiseImports
