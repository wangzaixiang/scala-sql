organization := "com.github.wangzaixiang"

name := "scala-sql"

version := "1.0.3-SNAPSHOT"

isSnapshot := true

scalaVersion := "2.11.6"

libraryDependencies +=
  "com.h2database" % "h2" % "1.4.184" % "test"

publishMavenStyle := true

publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value)
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
