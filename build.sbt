name := "creek-xml-binary"

version := "0.0.1"

scalaVersion in ThisBuild := "2.11.8"

organization := "com.github.rgafiyatullin"

publishTo := {
  val nexus = "http://nexus.in-docker.localhost:8081/"
  Some("releases"  at nexus + "content/repositories/releases")
}
credentials += Credentials(Path.userHome / ".ivy2" / ".credentials")

lazy val commonSettings = Seq()

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.2.6",
  "com.github.rgafiyatullin" %% "creek-xml" % "0.1.1"
)

lazy val creekXmpp = Project("creek-xml-binary", file("."))
