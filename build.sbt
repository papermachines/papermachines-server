name := """papermachines-server"""

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.10.4"

libraryDependencies ++= Seq(
  jdbc,
  cache,
  ws,
  "org.webjars" % "bootstrap" % "3.2.0",
  "com.typesafe.slick" %% "slick" % "2.0.2",
  "com.typesafe.play" %% "play-slick" % "0.6.0.1",
  "org.scalatest" %% "scalatest" % "2.0" % "test",
  "org.scalatestplus" % "play_2.10" % "1.0.0" % "test"
)
