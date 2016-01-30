# About Scala-SQL
scala-sql is a library designed for scala language and for JDBC-based database access.
It aimed to provide a simple and directive API to replace JDBC's complex, low-level,
complicated code.

## Simple Example
Let's take an example for simple JDBC access:

```scala
val conn = DriverManager.getConnection(url, user, password)

val stmt = conn.prepareStatement("select * from users where id = ?")
val userId = 10

try {
    stmt.setInt(1, userId)
    val rs = stmt.executeQuery()
    while(rs.next()){
      println(s"""name: ${rs.getString("name")}  age: ${rs.getInt("age")}""")
    }
}
finally {
   stmt.close
}

```
and how scala-sql help us:
```scala
val conn =  DriverManager.getConnection(url, user, password)
val userId = 10

conn.eachRow(sql"select * from users where id = $userId") { rs: ResultSet =>
    println(s"""name: ${rs.getString("name")}  age: ${rs.getInt("age")}""")
}

```

via this example, we show you some features of scala-sql:

1. scala-sql is a simple encapsulation of jdbc api.
1. scala-sql enhances java.sql.Connection/javax.sql.DataSource with some method to
direct execute SQL statement and return results. you dont need to worry about resource
allocate and free yourself.
1. scala-sql provide a powerful string interpolation structure sql, which support
 saffe dynamic sql statement.(no sql inject)

## SQL string interpolation
when using JDBC, we need to seperate the dynamic sql stament(having parameter) and
paramteries ourself with a lot of setString/setDate/setInt operation, make the code
very noisy.

based on Scala's String-Interpolation feature, scala-sql provides a powerful sql""
constructor like:
```scala
sql"select * from users where id = ${userId}"

sql"""update users set name = $userName,
    email = ${userEmail}
    where id = $userId
"""
```
don't worry about sql-inject security problem here, since the sql string interpolation is
not simple String-Concat operation, It's really paramterized dynamic SQL, so this exmaple
will really like follows:

```scala
val stmt = prepareStatement("select * from users where id = ?")
stmt.setInt(1, userId)

val stmt2 = prepareStatement("update users set name = ?, email = ? where id = ? ")
 stmt2.setString(1, userName)
 stmt2.setString(2, email)
 stmt2.setInt(3, userId)

```

## Mapping
scala-sql is not an ORM frameworkd or library, it is just a JDBC wrapper. but it still
provide some mapping functions.

1. mapping a row(ResultSet) as Row object. since ResultSet is a temporary cursor which
 will be invalid after cursor move or cursor closed. scala-sql provide a deattached
 value object `Row`. the `Row` API is so familiar as the `ResultSet` with `getXXX(index)`
 or `getXXX(columnName)` methods, your can think that `Row` is a deattached `ResultSet`
 snapshot.
2. mapping a row as an Java(Scala) POJO. when iterate a query or execute a query as results,
   scala-sql support mapping the ResultSet as an Java(Scala) POJO for your convenient.
    1. basicly, scala-sql follows the Java Beans conversation, so a `name` column will
    mapped to a `name` property(either java's `getName/setName` or scala's `name/name_=`).
    2. any transient property marked as @transient will be ignored
    3. when a rename mapping is needed, you need mark a `@Column(name=)` annotation,
    eg. a column `user_email` mapping to a property `userEmail`, you need mark `@Column(name="user_email")`
    4. You can put a `@Table` annotation on the POJO class when it mapps to another table(
    default the POJO's simple class name)
    5. also, You can put a `@Table(camelToUnderscore=true)` to mark that the POJO follows
    mapping underscore column name to camel property name. if you have SQL DDL conversation
    like this, you can put such a annoation to avoid mark each property with `@Column`
3. if you querys only 1 columns, you can directly mapping the row to a primitive type such as
    `Int/String/Date/BigDecimal` etc.
4. scala-sql is a scala library so it provide special support for scala types, so you can
    using scala.BigDecimal just as java.math.BigDecimal. Be sure, scala.BigDecimal is much
    simple than jBigDecimal so i would like to advice using scala.BigDeciaml in our works.


## Enhanced java.sql.Connection
scala-sql enhanced java.sql.Connection with useful methods.

### val count = Connection.executeUpdate(sql)
execute a update statment, and return the update count(same as jdbc's executeUpdate)

### Connection.eachRow(sql) { row: TYPE => doSth }
execute a query statement, and iterate the resultset

1. mapping each row as TYPE(see `Mapping`). for simple usage, you can using `Row`
   instead a `POJO`
2. execute the closure code `doSth`
3. Since we are iterate the Result, we can direct process the ResultSet without mapping.
       so here, the TYPE can be `ResultSet` which maybe best performance.

### val results: List\[TYPE\] = Connection.rows\[TYPE\](sql)
execute a query and result the resultset.

1. mapping each row as TYPE(see `Mapping`). for simple usage, you can using `Row`
   instead a `POJO`
2. Since we return a result list, which must be dettaced data, so `ResultSet` is not
   supported.

### val result: Option\[TYPE\] = Connection.row\[TYPE\](sql)
execute a query and return a Optional result. either Some(value) if having 1+ rows or
 None if the query returns no rows.


## Enhanced javax.sql.DataSource
scala-sql enhanced javax.sql.DataSource, the api is familar as java.sql.Connection.
unlike the Connection operations, the DataSource's operation always follows:

1. get a connection from datasource
2. execute the update/query on the connection
3. close the connection(return to datasource).
