utility scala-sql
=================

scala-sql 2.0 is a rewrite of the previous 1.0, It is not fully compatiable with the old API 
but it is 

# Planning Features

- [X] support customize types which can be mapped to SQL( means it can pass into SQL 
like `select * from users where name = ${name}` and can be passed out from SQL like `row.get[T]("field)` )
- [X] flexible mapping ResultSet to Bean via ResultSetMapper
- [X] using macro to generate Case class ResultSetMapper.
- [ ] using macro to generate JavaBean、ScalaBean ResultSetMapper
- [X] using macro to parse the SQL grammar so can we validate SQL grammar in compile time. (this is a 
long-term designed feature. ) 
- [ ] utils to support REPL.  

# Compile-Time grammar checker
1. write a scala-sql.properties in current directory 
2. provide a default.url, default.user, default.password, default.driver for the default configuration
3. write sql statement using `SQL"select * from table"`
4. If you need to access muti-database, you can define a `@db(name="some")` in the enclosing class, and 
define `some.url, some.user, some.password, some.driver` 

# JdbcValue[T]， JdbcValueAccessor[T]
scala-sql defines type class `JdbcValueAccessor[T]`, any type which has an implicit context bound of `JdbcValueAccessor`
can be passed into query, and passed out from ResultSet. 
This include:
- primary SQL types, such as `byte`, `short`, `int`, `string`, `date`, `time`, `timestamp`, `bigdecimal`
- scala types: such as `scala.BigDecimal`
- optional types. Now you can pass a `Option[BigDecimal]` into statement which will auto support the `null`
- customize your type via define a implicit value `JdbcValueAccessor[T]`

# ResultSetMapper[T]
scala-sql define type class `ResultSetMapper[T]`, any type which has an implicit context of `ResultSetMapper`
can be mapped to a ResulSet, thus, can be used in the `rows[T]`, `row[T]`, `foreach[T]` operations.

instead of writing the ResultSetMapper yourself, scala-sql provide a Macro which automate generate the
mapper for Case Class. 

TODO we will support normal JavaBeans and ScalaBeans(not getter/setter, but `var name` style) 


sbt usage:
=====
```sbt
libraryDependencies +=  "com.github.wangzaixiang" %% "scala-sql" % "2.0.0-SNAPSHOT"
scalaVersion := "2.11.6"

```


