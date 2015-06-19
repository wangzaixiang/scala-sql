utility scala-sql
=================

scala has published to Maven Central as com.github.wangzaixiang/scala-sql/1.0.0-beta

sbt usage:
```sbt
libraryDependencies +=  "com.github.wangzaixiang" %% "scala-sql" % "1.0.0-beta"
scalaVersion := "2.11.6"

```

Changelog
=========

* 2014-12-30 add sbt support, remove macro support feature and maybe ported back for 2.11

TODO
=====
* refract executeUpdateWithGenerateKey(sql)(processGenerateKeys) to what?
