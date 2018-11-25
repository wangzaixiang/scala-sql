# scala-sql Batch 支持方案

> 本文介绍了一个基于宏替换的方式，为scala-sql提供更为优美的批处理支持能力的方法。
> 
> 本特性将在实验一段时间后，合并到 scala-sql 的正式版本之中，当然，如果没有达到预期
> 也可能会废弃掉。

scala-sql 一直缺乏一个对 batch 操作的直接支持，因此，在编写代码时，仍然需要采用 JDBC 的底层API，涉及到
批处理时，代码的可读性不是很好。

这个问题为什么难以解决呢？这与scala-sql的设计哲学时相关的：

1. `sql"insert into table values (${a}, ${b})" ` 插值计算是将 sql 语句 与 传入变量强行绑定的。
2. 而 Batch 操作时， sql 语句需要在多次绑定值时重用。

当然，我们可以简单的在每次 addBatch 时，进行这个绑定，然后在运行期后续操作复用 sql 语句。但这种方式
我感觉并不是很"自然"， 于是设计了如下的方式：

```scala
  case class User(name:String, age:Int, email: String)

  def main(args: Array[String]): Unit = {

    val conn = SampleDB.conn

    val batch = conn.createBatch[User] { u =>
      val name = u.name.toUpperCase()
      sql"insert into users(name, age, email) values(${name}, ${u.age}, ${u.email})"
    }
    
    val users = User("u1", 10, "u1") :: User("u2", 20, "u2") :: Nil

    users.foreach { u =>
      batch.addBatch(u)
    }

    batch.close()

    // print the rows for test
    conn.rows[User]("select * from users").foreach(println)

  }

```

在如上示例中，
1. `createBatch[T]( process: T=>SQLWithArgs )`会返回一个 Batch 对象，
2. 在后续每次批量插入时， 调用 `batch.addBatch(user)`
3. 使用 `batch.close` 提交并关闭批处理。

有两种实现方式：

1. 方式一：

    - 在首次 addBatch 时，根据函数返回的 字符串插值， 准备 PreparedStatement。
    - 在每次 addBatch 时，执行 setParameter(idx, value) 操作，并调用 praparedStatement.addBatch

2. 方式二：

    - 在 createBatch 时准备好 PreparedStatement （此时无法调用 process 函数）
    - 每次 addBatch 时，调用 process 函数，计算出绑定的值，`setParameter + addBatch`
  
方式一可以接受，但不自然，而且，每次 addBatch 操作都需要计算一个不必要的 SQLWithArgs 插值，是一个多余操作。

方式二直觉上看，是无法实现的。不过，由于有万能的"macro"，我们可以自动的重写上述代码为：

```scala

val batch = new BatchImpl[User](conn, "insert into users set name = ?, age = ?, email = ?") { u =>
  val name = u.name.toUpperCase()
  List(name, u.age, u.email) 
}

```

通过宏替换，我们可以使用更为直观的方式来编写代码，而系统会按照规则替换为优化执行的版本，避免不必要
的运行期开销。

# MySQL 增强
对MySQL而言，要使用 batch 处理，你必须在 URL 中加上如下选项：rewriteBatchStatements=true。
此外，MySQL仅支持对 insert into table(field1,field2,..) values(a,b,..)的语句进行批处理。如果一条insert语句中存在较多的字段时，
这种语法比较难以阅读和维护，我们在开发中推荐使用 insert into table set field1 = a, field2 = b,... 的语法。但后者MySQL JDBC是
不支持批处理的。

为此，scala-sql提供了一个方法，createMySQLBatch，使用如下：
```scala
    val batch = conn.createMySqlBatch[User] { u =>
      val name = u.name.toUpperCase()
      sql"insert into users set name = ${name}, age = ${u.age}, email = ${u.email}"
    }
```
在这里，createMySqlBatch 会在编译期间，识别 insert set 语法，并自动的转换成为 insert values 语法，从而实现既代码可读，同时，又支持批处理能力。这也是 Macro 带来的额外便利吧。

## 题外话
集成了对SQL的编译时期检查，可能会给scala-sql带来其它有价值的特性，比如：
- [ ] 对 `delete from table`这样的未加where的语句进行编译期报错。

