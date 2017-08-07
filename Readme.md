utility scala-sql
=================

scala-sql 2.0 is a redefine of the scala-sql 1.0 version.

# Planning Features

- [X] support customize types which can be mapped to SQL( means it can pass into SQL 
like `select * from users where name = ${name}` and can be passed out from SQL like `row.get[T]("field)` )
- [X] flexible mapping ResultSet to Bean via ResultSetMapper
- [X] using macro to generate Case class ResultSetMapper.
- [ ] using macro to generate JavaBean、ScalaBean ResultSetMapper
- [X] using macro to parse the SQL grammar so can we validate SQL grammar in compile time. (this is a 
long-term designed feature. )

# Compile-Time grammar checker
1. write a scala-sql.properties in current directory 
2. provide a default.url, default.user, default.password, default.driver for the default configuration
3. write sql statement using `SQL"select * from table"`
4. If you need to access muti-database, you can define a `@db(name="some")` in the enclosing class, and 
define `some.url, some.user, some.password, some.driver` 


sbt usage:
```sbt
libraryDependencies +=  "com.github.wangzaixiang" %% "scala-sql" % "2.0.0-SNAPSHOT"
scalaVersion := "2.11.6"

```


