name := "tictactoe"
version := "0.0.1-SNAPSHOT"

scalaVersion := "2.12.7"

scalacOptions ++= Seq(
  "-encoding", "UTF-8",   // source files are in UTF-8
  "-deprecation",         // warn about use of deprecated APIs
  "-unchecked",           // warn about unchecked type parameters
  "-feature",             // warn about misused language features
  "-language:higherKinds",// allow higher kinded types without `import scala.language.higherKinds`
  "-Xlint",               // enable handy linter warnings
  "-Ypartial-unification" // allow the compiler to unify type constructors of different arities
)

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % "2.0.0",
  "org.typelevel" %% "cats-free" % "2.0.0",
  "dev.profunktor" %% "console4cats" % "0.8.0",
  "org.scalacheck" %% "scalacheck" % "1.14.1" % Test,
  "org.typelevel" %% "cats-laws" % "2.0.0" % Test,
  "org.typelevel" %% "discipline-core" % "1.0.0" % Test,
  "org.typelevel" %% "discipline-scalatest" % "1.0.0-RC1" % Test,
  "org.scalatest" %% "scalatest" % "3.0.8" % Test
)

scalacOptions in Test ++= Seq("-Yrangepos")

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3")


