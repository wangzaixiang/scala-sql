utility scala-sql
=================

scala-sql 2.0 是一个轻量级的 scala jdbc 库，它是一个简单的JDBC的封装，以提供类型安全的、简洁的scala API。
- 没有新概念。你会用JDBC，就会发现scala-sql很自然。只需要1-2小时，你就会完全熟悉。
- scala风格，强类型支持、case class支持（不可变风格）。
- 可以扩展的数据类型支持。
- 通过macro提供强大的类型支持。
- 编译时期的SQL语法检查。
- 提供 Row 类型，无需定义case class也可以读取数据。
- 提供强类型的 batch API。


# 基本用法
scala-sql 为 `java.sql.Connection` & `java.sql.DataSource` 提供了如下增强的方法:
- executeUpdate
  ```scala
    dataSource executeUpdate sql"""update table set name = ${name} and age = ${age} where id = ${id}"""
  ```
  你可以理解上面的代码等同于如下的Java代码:
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
  这个例子给出了 scala-sql 的最基本用法，他就是一个JDBC的简单封装，而没有引入更多的概念。
  
  在这个例子中，展示了scala-sql的最重要的一个特性：sql插值。他有如下特点：
  - `${expr}` 不是字符串拼接，而是作为参数传递，因此，使用插值，没有SQL注入风险。
  - `${expr}` 是强类型检查的。诸如 `java.sql.Connection` 或者 `java.swing.JFrame` 这样的值，在编译期间就会报错。
  - scala-sql 支持基本的数据类型，包括：`boolean`, `byte`, `short`, `int`, `float`,
  `double`, `string`, `java.math.BigDecimal`, `java.sql.Date`, `java.sql.Time`, `java.sql.Timestamp` 等.
  - 支持 scala友好的数据类型 `scala.BigDecimal`, 
  - 支持 `scala.Option[T]` 这里T是上述合法的类型。
  - 可以扩展支持新的类型 T，只需要提供一个隐士值 `JdbcValueAccessor[T]` 就可以像上述的基本类型一样的作为 `${expr}`传递给sql，以及使用在下面
  需要映射的`case class`中。
  - 如果你使用 SQL"" 字符串插值，还可以享受到在编译期间的SQL语法检查功能。 这个检查功能是通过连接到编译时期的一个数据库进行验证的，可以检查
  包括语法、字段名在列的一系列错误。
    
- rows
  ```scala
    case class User(name: String, age: Int)
  
    val users: List[User] = dataSource.rows[User](sql"select * from users where name like ${name}")
  ```
  scala-sql 提供了一个简单的 ORM 机制，在这个例子中，我们只需要定义一个 `case class`，就可以完成从ResultSet到 case class的映射工作。而且，
  有别于其他的框架，scala-sql是通过 Macro，在编译时期就自动生成了从 ResultSet 到 Case Class 的转换代码，不会使用到反射方式，性能非常高。

  `rows[T](sql)` 这里的 T 可以是： 
  - Case Class. 
  case class, so you need not writing the mapping code by hand, the macro will automate generate it.
  - `Row` 可以理解为 Row 是一个离线的 ResultSet 行，它提供了和ResultSet一样的API，如 getInt(index) 、getString(name)等。
  case class, so you need not writing the mapping code by hand, the macro will automate generate it.
  - 基础类型. 如果我们的SQL语句只查询单个字段，那么可以直接使用 `rows[Int](sql"statement"")` 这种形式。
  
- foreach
  ```scala
  dataSource.foreach(sql"select * from users where name like ${name}" { u: User =>
    ...
  }
  ```
  与 rows 相似。foreach 在迭代中执行代码，而不是返回一个 List[T]。
- generateKey
- withStatement
  ```scala
  dataSource.withStatement { stmt: Statement => ...
  }
  ```
- withPreparedStatement
- withConnection
  ```scala
  dataSource.withConnection { conn: Connection => ...
  }
  ```
- withTransaction
  ```scala
  dataSource.withTransaction { conn: Conntion => ...
  }
  ```

# 编译期语法检查
1. 在当前目录下编辑 scala-sql.properties 文件。 
2. 设置 default.url, default.user, default.password, default.driver 信息，使之指向一个用于进行类型检查的数据库。
3. 使用 SQL"" 插值。
4. 如果我们的项目中会访问多个数据库，我们可以在最外层的类上加上 `@db(name="some")` 注释, 在配置文件中定义：`some.url, some.user, some.password, some.driver` 


sbt 依赖:
=====
```sbt
libraryDependencies +=  "com.github.wangzaixiang" %% "scala-sql" % "2.0.7"
```
