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

# Basic usgae
scala-sql enhance `java.sql.Connection` & `java.sql.DataSource` with more methods:
- executeUpdate
  ```scala
    dataSource executeUpdate sql"""update table set name = "$name" and age = ${age} where id = ${id}"""
  ```
  you may compare the code with the Java Version that do the same thing:
  ```java
    DataSource dataSource = ...;
    Connection conn = null;
    PreparedStatement ps = null;
    try {
      conn = dataSource.getConnection();
      ps = conn.prepareStatement("update table set name = ? and age = ? where id = ?")
      ps.setString(1, name);
      ps.setInt(2, age);
      ps.setInt(3, id);
    
      ps.executeUpdate
    }
    finally {
      try {
          if(ps != null) ps.close();
      }
      catch(SQLException ex){}
      try {
          if(conn != null) conn.close();
      }
      catch(SQLException ex) {}
    }
  }
    ```
  That is scala-sql, which has the same sematics as the JDBC framework, but with better encapsulation 
  and more scala style.
  
  scala-sql provide a powerful sql/SQL string interpolation `sql"sql-statement"` and `SQL"sql-statement"`
  It has features:
  - `${expr}` is not injected as string concat but SQL parameter, or the `?` style in prepared statement. 
  so by using scala-sql, you can avoid SQL-Inject problems automated
  - also `${expr}` is type checked at compile time, you can't pass a wrong type such as a `java.sql.Connection`
  or `java.swing.JFrame` value, since which can't saved to table columns.
  - scala-sql support all baisc jdbc primitive SQL types such as `boolean`, `byte`, `short`, `int`, `float`,
  `double`, `string`, `bigdecimal`, `date`, `time`, `timestamp` etc.
  - scala-sql also support scala types such as `scala.BigDecimal`, 
  `scala.Option[T] where T is valid sql type`
  - you can define your pass-in Type `T` by define a implict value `JdbcValueAccessor[T]`
  - **MOST INTERESTING FEATURE, the `SQL"sql-statement"` can even check the statement 
  grammar at compile time, include the sql grammar, wrong table name, wrong fieldname, and etc.**
  to using this feature, please see usage on the `Compile-Time grammar check` section.
    
- rows
  ```scala
    case class User(name: String, age: Int)
  
    val users: List[User] = dataSource.rows[User](s"select * from users where name like ${name}")
  ```
  scala-sql provide a Simple ORM mechanism, any object of type `T` which has a implict `ResultSetMapper[T]`
  can be used in rows, row, foreach method.
  
  Currently, the scala-sql provide a macro which support automated generate the `ResultSetMapper` of a 
  case class, so you need not writing the mapping code by hand, the macro will automate generate it.
  
  `Row` exists when you don't provide a mapping type, you can think `Row` is a deattached `ResultSet` row.
  so a simple `rows[Row](sql"statement")` can used to recieve data from database.
  
  - if your statment only select 1 column such as `select count(*) from table`, you can even using the 
  primitive sql types such as `rows[Int](sql"statement"")` 
  
  *I will enhance the macro to support normal Java Beans, Scala Beans later.*
- foreach
- generateKey
- withStatement
- withPreparedStatement
- withConnection
- withTransaction


# Compile-Time grammar check
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


