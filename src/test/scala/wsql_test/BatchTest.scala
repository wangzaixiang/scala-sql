package wsql_test

import org.scalatest.funsuite.*
import wsql.{given, *}

class BatchTest {

  val conn = SampleDB.conn
  case class User(name: String, age: Int, email: String)

  def main(args: Array[String]): Unit = {

    val list = List(User("John", 30, "john@qq.ckm"))
    val batch = conn.createBatch[User] { user =>
      val age = user.age + 10

      class Demo { def test(s:String) = s.toUpperCase }

      def test(s: String)(s2: String) : String =
        s.toUpperCase.nn + s2.toUpperCase

//      val name = test(user.name)(user.email)
      val name = new Demo().test(user.name)

      sql"insert into users(name, age, email) values(${name}, ${age}, ${null})"
    }

  }


}
