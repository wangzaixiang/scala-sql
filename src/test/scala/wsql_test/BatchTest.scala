package wsql_test

import org.scalatest.funsuite.*
import wsql.{given, *}

object BatchTest:
  case class User(name: String, age: Int, email: String) derives ResultSetMapper

class BatchTest extends AnyFunSuite  {
  import BatchTest.*

  val conn = SampleDB.conn
  val users = List(
    User("John", 30, "john@qq.ckm"),
    User("Rose", 20, "rose@qq.com")
  )

  test("basic batch") {

    val batch = conn.createBatch[User] { user =>
      val name = user.name.toUpperCase
      sql"insert into users(name, age, email) values(${name}, ${user.age}, ${user.email})"
    }

    conn.executeUpdate("delete from users where 1=1")
    users.foreach(batch.addBatch)
    batch.close()

    val rows = conn.rows[User]("select * from users")
    assert(rows == users.map(u=>u.copy(name = u.name.toUpperCase.nn)))

  }

  test("mysql batch") {
    val batch = conn.createMysqlBatch[User] { user =>
      val name = user.name.toUpperCase
      SQLWithArgs("insert into users set name = ?, age = ?, email = ?", Seq(name, user.age, user.email))
//      sql"insert into users set name = $name, age = ${user.age}, email = ${user.email}"
    }
    conn.executeUpdate("delete from users where 1=1")
    users.foreach(batch.addBatch)
    batch.close()

    val rows = conn.rows[User]("select * from users")
    assert(rows == users.map(u => u.copy(name = u.name.toUpperCase.nn)))
  }

}
