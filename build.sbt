
name := "affinity-client"
scalaVersion := "2.11.8"

lazy val `affinity-client` = (project in file("."))

libraryDependencies ++= Seq(

  "com.typesafe.play" %% "play-json" % "2.6.6",
  "com.typesafe.play" %% "play-ws" % "2.6.6",
  "com.typesafe.play" %% "play-ahc-ws" % "2.6.6",
  "com.google.guava" % "guava" % "22.0",
  "com.kifi" %% "json-annotation" % "0.2",
  "com.netaporter" %% "scala-uri" % "0.4.16",
  "com.github.scopt" %% "scopt" % "3.5.0",
  "ch.qos.logback" % "logback-classic" % "1.1.7",
  "org.apache.commons" % "commons-lang3" % "3.4",
  "commons-io" % "commons-io" % "2.4",
  "org.apache.commons" % "commons-compress" % "1.10",
  "org.apache.commons" % "commons-math3" % "3.0",
  //"org.ocpsoft.prettytime" % "prettytime" % "3.2.7.Final",
  "com.typesafe" % "config" % "1.3.1",
  "com.lihaoyi" %% "sourcecode" % "0.1.3",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.7.2",
  //"com.google.inject" % "guice" % "4.1.0",
  "com.lihaoyi" %% "fansi" % "0.2.5",

  "org.scalatest" %% "scalatest" % "2.2.4" % "test",
  "org.scalamock" %% "scalamock-scalatest-support" % "3.6.0" % "test"
)

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)


