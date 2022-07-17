package wsql_test

import org.scalatest.funsuite.*
import wsql.{given, *}

class BatchTest {

  val conn = SampleDB.conn
  case class User(name: String, age: Int, email: String)

  def main(args: Array[String]): Unit = {

    val list = List(User("John", 30, "john@qq.ckm"))
    val batch = conn.createBatch[User] { u =>
      val name = u.name.toUpperCase()
      sql"insert into users(name, age, email) values(${name}, ${u.age}, ${u.email})"
    }

//     rewrite as
//    val batch2 = BatchImpl.apply[User](conn, "insert into users(name, age, email) values(?, ?, ?)", { (u: User) =>
//      val name = u.name.toUpperCase()
//      List(name, u.age, u.email)
//    })

  }


}
