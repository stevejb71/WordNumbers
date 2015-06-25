name := "WordsNumbers"

version := "1.0"

scalaVersion := "2.11.6"

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0-M5" cross CrossVersion.full)

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-language:_")

libraryDependencies ++= Seq(
  "com.github.mpilquist" %% "simulacrum" % "0.3.0",
  "org.spire-math" %% "spire" % "0.10.1"
)

initialCommands in console := """import wordsnumbers._,part1._,part2._"""