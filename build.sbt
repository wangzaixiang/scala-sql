organization := "com.github.wangzaixiang"

name := "scala-sql"

version := "1.0.4"

scalaVersion := "2.11.6"

libraryDependencies ++= Seq(
  "org.slf4j" % "slf4j-api" % "1.7.9",
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
    <scm>
      <url>https://github.com/wangzaixiang/scala-sql.git</url>
      <connection>scm:git:git@github.com:wangzaixiang/scala-sql.git</connection>
    </scm>
    <developers>
      <developer>
        <id>wangzaixiang</id>
        <name>wangzaixiang</name>
        <url>http://wangzaixiang.github.io</url>
      </developer>
    </developers>)
