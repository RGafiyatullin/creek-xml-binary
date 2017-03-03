name := "creek-xml-binary"

version := "0.1.0"

scalaVersion in ThisBuild := "2.11.8"

organization := "com.github.rgafiyatullin"

publishTo := {
  val nexus = "http://am3-v-perftest-xmppcs-1.be.core.pw:8081/"
  Some("releases"  at nexus + "content/repositories/sbt-releases")
}
credentials += Credentials(Path.userHome / ".ivy2" / ".credentials")

lazy val commonSettings = Seq()

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.2.6",
  "com.github.rgafiyatullin" %% "creek-xml" % "0.1.6"
)

lazy val creekXmpp = Project("creek-xml-binary", file("."))
