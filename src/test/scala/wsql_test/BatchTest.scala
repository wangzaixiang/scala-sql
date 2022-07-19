package wsql_test

import org.scalatest.funsuite.*
import wsql.{given, *}

class BatchTest extends AnyFunSuite  {

  val conn = SampleDB.conn

  test("basic batch") {
    case class User(name: String, age: Int, email: String) derives ResultSetMapper
    val users = List(
      User("John", 30, "john@qq.ckm"),
      User("Rose", 20, "rose@qq.com")
    )
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

}
