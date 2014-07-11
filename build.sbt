name := """papermachines-server"""

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.10.4"

resolvers += Resolver.mavenLocal

libraryDependencies ++= Seq(
  jdbc,
  cache,
  ws,
  "org.webjars" %% "webjars-play" % "2.3.0",
  "org.webjars" % "bootstrap" % "3.2.0",
  "org.webjars" % "bootswatch-simplex" % "3.2.0",
  "org.webjars" % "html5shiv" % "3.7.2",
  "org.webjars" % "respond" % "1.4.2",
  "com.typesafe.slick" %% "slick" % "2.0.2",
  "com.typesafe.play" %% "play-slick" % "0.6.0.1",
  "org.apache.tika" % "tika-parsers" % "1.5",
  "com.chuusai" % "shapeless_2.10.4" % "2.0.0",
  "org.chrisjr" % "topic-annotator" % "latest.revision",
  "org.scalatest" %% "scalatest" % "2.0" % "test",
  "org.scalatestplus" % "play_2.10" % "1.0.0" % "test"
)
