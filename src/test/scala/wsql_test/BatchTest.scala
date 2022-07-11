package wsql_test

import org.scalatest.funsuite.*
import wsql.{given, *}

class BatchTest extends AnyFunSuite {

  val conn = SampleDB.conn

  test("Simple batch") {

    case class User(name: String, age: Int, email: String)
    val batch = conn.createBatch[User] { u =>
      val name = u.name.toUpperCase()
      sql"insert into users(name, age, email) values(${name}, ${u.age}, ${u.email})"
    }

    // rewrite as
    val batch2 = BatchImpl[User](conn, "insert into users(name, age, email) values(?, ?, ?)") { (u: User) =>
      val name = u.name.toUpperCase()
      List(name, u.age, u.email)
    }

    println()

  }

}
