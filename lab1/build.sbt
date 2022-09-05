
val parserCombinators = "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"

lazy val root = (project in file(".")).
  settings(
    name := "lab1",
    scalaVersion := "2.13.8",
    libraryDependencies += parserCombinators
  )
