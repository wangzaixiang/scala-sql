package wangzx.scala_commons.sql_test

import javax.sql.DataSource

import org.junit.{Before, Test}
import wangzx.scala_commons.sql._

/**
  * Created by wangzx on 15/12/2.
  */

class Employee {

  var name: String = _

  var email: Option[String] = None

  @Column(optionalType = classOf[Int])
  var age: Option[Int] = None

}

//class OptionalTest {
//
//  val dataSource = SampleDB.dataSource
//
//  @Test
//  def test1(): Unit = {
//
//    dataSource.executeUpdate(
//      """
//        delete from users where 1 = 1;
//
//        insert into users values('user1', 'wangzaixiang@gmail.com', 40);
//        insert into users values('user2', null, null);
//      """)
//    dataSource.rows[Employee]("select * from users order by name") match {
//      case x @ List(row1, row2) =>
//        assert(row1.email == Some("wangzaixiang@gmail.com"))
//        assert(row1.age == Some(40))
//
//        assert(row2.email == None)
//        assert(row2.age == None)
//    }
//
//
//  }
//
//}
