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

libraryDependencies += "org.typelevel" %% "cats-core" % "1.4.0"
libraryDependencies += "org.typelevel" %% "cats-free" % "1.4.0"
libraryDependencies += "org.typelevel" %% "discipline-core" % "1.0.0"
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.14.1" % "test"
libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-laws" % "2.0.0" % Test,
  "com.github.alexarchambault" %% "scalacheck-shapeless_1.14" % "1.2.3" % Test
)
libraryDependencies +=
  "org.typelevel" %% "discipline-core" % "1.0.0" % Test
libraryDependencies ++= Seq(
  "org.typelevel" %% "discipline-specs2" % "1.0.0" % Test
)
libraryDependencies ++= Seq("org.specs2" %% "specs2-core" % "4.6.0" % "test")
libraryDependencies += "org.typelevel" %% "kittens" % "2.0.0"

scalacOptions in Test ++= Seq("-Yrangepos")

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3")


