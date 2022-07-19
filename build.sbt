organization := "com.github.wangzaixiang"

name := "wsql"

version := "3.0.0"

scalaVersion := "3.1.3"

scalacOptions := Seq( "-Yexplicit-nulls" )

libraryDependencies ++= Seq(
  "org.slf4j" % "slf4j-api" % "1.7.9",
  "com.github.jsqlparser" % "jsqlparser" % "1.2",
  // "org.scala-lang" %% "scala3-compiler" % scalaVersion.value,

  "com.h2database" % "h2" % "1.4.184" % "test",
  "junit" % "junit" % "4.12" % "test",

  "org.scalatest" %% "scalatest" % "3.2.12" % "test",

  "ch.qos.logback" % "logback-classic" % "1.2.11" % "test",
  "ch.qos.logback" % "logback-core" % "1.2.11" % "test",
  "mysql" % "mysql-connector-java" % "5.1.38" % "test"
)
publishMavenStyle := true

publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (version.value.endsWith("SNAPSHOT"))
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

publishConfiguration  := publishConfiguration.value.withOverwrite(true)


publishArtifact in Test := false

pomIncludeRepository := { _ => false }

pomExtra := (
  <url>http://github.com/wangzaixiang/scala-sql</url>
    <licenses>
      <license>
        <name>BSD-style</name>
        <url>http://www.opensource.org/licenses/bsd-license.php</url>
        <distribution>repo</distribution>
      </license>
    </licenses>
    <developers>
      <developer>
        <id>wangzaixiang</id>
        <name>wangzaixiang</name>
        <url>http://wangzaixiang.github.io</url>
      </developer>
    </developers>
    <scm>
      <connection>scm:git:https://github.com/wangzaixiang/scala-sql</connection>
      <developerConnection>scm:git:https://github.com/wangzaixiang/scala-sql</developerConnection>
      <url>github.com/wangzaixiang/scala-sql</url>
      <tag>v2.0.7</tag>
  </scm>
)
