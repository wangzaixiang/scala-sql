package wangzx.scala_commons.sql_test

import wsql.*

import scala.quoted.*

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
  )
  case class PersonAddress
  (
    province: String,
    city: String
  )


  given Conversion[String, Int] with
    def apply(x: String): Int = Integer.parseInt(x)

  def main(args: Array[String]): Unit = {

    val person = Person("123", "wangzhx", List("111"), PersonAddress("gd", "gz") )

    val user = BeanBuilder.build[User](person, Some("123"))( "score"-> Some(100) )

    println(s"person = $person, user = $user")
    assert( user == User(123, "wangzhx", List(111), UserAddress("gd", "gz"), Some(100)) )
    println("passed")
  }

}
