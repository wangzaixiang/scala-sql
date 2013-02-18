package wangzx.scala_commons.sql_test

import wangzx.scala_commons.sql._
import scala.reflect.macros.Context
import scala.language.experimental.macros

object TestStaticCheck {

  def main(args: Array[String]) {

    import MultiDbSQLStringContext._
    val name = "wangzx"
    val age = 40

    //val s1 = db1"select * from student where name = ${name} and age = ${age}"
    //val s2 = db2"select * from student where name = ${name} and age = ${age}"
    

  }

}