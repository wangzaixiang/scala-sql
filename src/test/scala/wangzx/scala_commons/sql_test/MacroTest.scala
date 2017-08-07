package wangzx.scala_commons.sql_test

import java.sql.ResultSet


object MacroTest {

  case class User(name: String, age: Int, classRoom: Int = 1)

  class Student {
    var name: String = _
    var age: Int  = _
    var classRoom: Int  = 1
  }
//
//  def main(args: Array[String]): Unit = {
//    val rs = null
//    row[User](rs)
//  }

//  def row[T : ResultSetMapper](rs: ResultSet): T = {
//    val rsm = implicitly[ResultSetMapper[T]]
//    rsm.from(rs)
//  }

}
