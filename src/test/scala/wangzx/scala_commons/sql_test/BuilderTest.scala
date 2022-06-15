package wangzx.scala_commons.sql_test

import wangzx.scala_commons.sql.given
import wangzx.scala_commons.sql.{BeanBuilder, ResultSetMapper}

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
    id: Int,
    name: String,
    opId: List[Int],
    address: UserAddress,
    score: Option[Int]
    //dady: Person
  )
  case class PersonAddress
  (
    province: String,
    city: String
  )



  def main(args: Array[String]): Unit = {

//    val someStr = Some("123")
    //val someInt = someStr.copyTo[Int]
//    val userMapper: ResultSetMapper[PersonAddress] = ResultSetMapper.material[PersonAddress]

    //val dady = Person("120", "wangzx", Some("120"), PersonAddress("gd", "gz"))
    val person = Person(123, "wangzhx", List(123), UserAddress("gd", "gz"), Some(100))

//    val userAddress = BuilderTestHelper.build1[UserAddress](person.address)
    val user = BeanBuilder.build[User](person, Some("123"))("score"->Some(100))
//
//    // new build method
//    val user2 = BeanBuilder.build[User](person)(_.copy(
//      score = Some(100), name="rainbow"))

    /**
     * val users = {
     *   val f1 = person.f1
     *   val f2 = peson.f2
     *   val f2 = Some(100)
     *   val f4 = "rainbow"
     *   User(f1, f2, f3, f4)
     * }
     */

    println("user: " + user)
    assert( user == User(123, "wangzhx", List(123), UserAddress("gd", "gz"), Some(100)))
  }

}
