package wangzx.scala_commons.sql_test

import org.junit.Test
import wsql.{given, *}

/**
  * Created by wangzx on 15/12/17.
  */
class TestSuite {

  val dataSource = SampleDB.dataSource

  @Test
  def testRow(): Unit = {

    val row = dataSource.row[Row]("select * from users where name = 'user1'")
    assert(row == None)
//    assert(row.get.getString("name") == "user1")

  }

}
