name := "farce"

organization := "gsd"

version := "0.0.1"

scalaVersion := "2.9.1"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies ++= Seq(
"junit" % "junit" % "4.8.2" % "test",
//"com.googlecode.linux-variability-analysis-tools" % "lvat" %% "1.0-SNAPSHOT"
"org.slf4j" % "slf4j-simple" % "1.6.5",
"log4j" % "log4j" % "1.2.17",
"com.typesafe.akka" % "akka-actor" % "2.0.5",
"org.clapper" %% "argot" % "0.4"
)

checksums in update := Nil

TaskKey[File]("mkrun") <<= (baseDirectory, fullClasspath in Runtime, mainClass in Runtime) map { (base, cp, main) =>
  val template = """#!/bin/sh
java -ea -Xmx2G -Xms128m -Xss10m -classpath "%s" %s "$@"
"""
  val mainStr = ""
  val contents = template.format(cp.files.absString, mainStr)
  val out = base / "run.sh"
  IO.write(out, contents)
  out.setExecutable(true)
  out
}
