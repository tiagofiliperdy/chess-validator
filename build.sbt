name := "ProgTest1"

version := "0.1"

scalaVersion := "2.12.8"

unmanagedJars in Compile += file(
  "D:/IdeaProjects/WHGtest/WHG_Platform Developer_Programming_Test_v1/ProgTest/libs/userinput.jar"
)

libraryDependencies += "org.scalatest" %% "scalatest" % "3.1.0" % Test
