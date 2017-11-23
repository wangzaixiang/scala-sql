package wangzx.scala_commons.sql_test

import wangzx.scala_commons.sql.BeanBuilder

object BuilderTest {

  case class User
  (
    id: Int,
    name: String,
    opId: List[Int],
    address: UserAddress,
    score: Option[Int]
  )
  case class UserAddress
  (
    province: String,
    city: String
  )

  case class Person
  (
    id: String,
    name: String,
    opId: List[String],
    address: PersonAddress,
    //dady: Person
  )
  case class PersonAddress
  (
    province: String,
    city: String
  )

  implicit def str2Int(str: String) = str.toInt


  def main(args: Array[String]): Unit = {

    val someStr = Some("123")
    //val someInt = someStr.copyTo[Int]

    //val dady = Person("120", "wangzx", Some("120"), PersonAddress("gd", "gz"))
    val person = Person("123", "wangzhx", List("123"), PersonAddress("gd", "gz"))
    val user = BeanBuilder.build[User](person)("score"->Some("100"))

    assert( user == User(123, "wangzhx", List(123), UserAddress("gd", "gz"), Some(100)))
  }

}
