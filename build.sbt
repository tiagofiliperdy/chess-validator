name := "chess-validator"

version := "0.1"

scalaVersion := "2.13.9"

libraryDependencies += "org.typelevel" %% "cats-core" % "2.8.0"
libraryDependencies += "org.typelevel" %% "cats-laws" % "2.8.0"
libraryDependencies += "org.typelevel" %% "cats-effect" % "3.3.14"
libraryDependencies += "org.typelevel" %% "discipline-core" % "1.5.1"
libraryDependencies += "org.typelevel" %% "discipline-scalatest" % "2.2.0" % Test
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.14" % Test