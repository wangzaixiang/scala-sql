utility scala-sql
=================

sbt usage:
```sbt
resolvers += "wangzx repository" at "https://raw.githubusercontent.com/wangzaixiang/repository/master/"

libraryDependencies ++= Seq(
  "wangzx" %% "scala-sql" % "1.0.0-beta",
  "com.h2database" % "h2" % "1.4.184"
)

scalaVersion := "2.11.4"
}
```

Changelog
=========

* 2014-12-30 add sbt support, remove macro support feature and maybe ported back for 2.11
