name := "SatSolverProject"

version := "1.0"

autoScalaLibrary := true

scalaVersion := "2.12.1"

resolvers += "OSS Sonatype" at "https://repo1.maven.org/maven2/"

lazy val scalasmtlib = RootProject(file("../scala-smtlib"))
val main = Project(id = "SatSolverProject", base = file(".")).dependsOn(scalasmtlib) 