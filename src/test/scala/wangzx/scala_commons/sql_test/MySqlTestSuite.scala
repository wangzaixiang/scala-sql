package wangzx.scala_commons.sql_test

import com.mysql.jdbc.jdbc2.optional.MysqlDataSource
import org.junit.{Assert, Test}

import wangzx.scala_commons.sql._
/**
  * Created by wangzx on 15/12/18.
  */
class MySqlTestSuite {

  val dataSource = {

    val ds = new MysqlDataSource
    ds.setURL(s"jdbc:mysql://127.0.0.1/scalasql?useUnicode=true&characterEncoding=utf8&zeroDateTimeBehavior=convertToNull")
    ds.setUser("root")
    ds.setPassword("root")
    ds
  }

  private def checkLog[T](content: String)(f: =>T) : T = {
    MemoryAppender.reset()
    val result = f
    Assert.assertTrue(MemoryAppender.getContent.contains(content))
    result
  }

  // TODO need to test LOG content but now i have no idea
  @Test
  def testRowLog(): Unit = {
    // assert LOG: SQL Result: 1

    checkLog("WARN  RichConnection - expect 1 row but really more. SQL result: 1") {
      val row1 = dataSource.row[Row]("select * from users")
      println(row1)
    }

    checkLog("SQL result: 0") {
      val row0 = dataSource.row[Row]("select * from users where name = 'test'")
      println(row0)
    }

    checkLog("SQL result: 2") {
      val rows = dataSource.rows[Row]("select * from users")
      println(rows)
    }
  }

}
