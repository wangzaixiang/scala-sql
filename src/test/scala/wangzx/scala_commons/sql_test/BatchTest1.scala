package wangzx.scala_commons.sql_test

import wangzx.scala_commons.sql._

object BatchTest1 {

  case class User(name:String, age:Int, email: String)

  def main(args: Array[String]): Unit = {

    val conn = SampleDB.conn

    val batch2 = conn.createMysqlBatch[User] { u =>
      val name = u.name.toUpperCase()
      sql"insert into users set name = ${name}, age = ${u.age}, email = ${u.email}"
    }

    val users = User("u1", 10, "u1") :: User("u2", 20, "u2") :: Nil

    users.foreach { u =>
      batch2.addBatch(u)
    }

    batch2.close()

    conn.rows[User]("select * from users").foreach(println)

  }

}
