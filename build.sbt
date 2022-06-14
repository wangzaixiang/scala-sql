organization := "com.github.wangzaixiang"

name := "scala-sql"

version := "3.0.0"

scalaVersion := "3.1.2"

// crossScalaVersions := Seq("2.11.12", "2.12.15")

libraryDependencies ++= Seq(
  "org.slf4j" % "slf4j-api" % "1.7.9",
  // "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  "com.github.jsqlparser" % "jsqlparser" % "1.2",

  "com.h2database" % "h2" % "1.4.184" % "test",
  "junit" % "junit" % "4.12" % "test",
  "ch.qos.logback" % "logback-classic" % "1.1.3" % "test",
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


// publishSignedConfiguration := publishConfiguration.value.withOverwrite(true)

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
