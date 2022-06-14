package wangzx.scala_commons.sql_test

import javax.sql.DataSource
import wangzx.scala_commons.sql.{given, *}


/**
  * Created by wangzx on 15/12/17.
  */
object SampleDB {

  val (dataSource, conn) = {
    val ds = new org.h2.jdbcx.JdbcDataSource
    ds.setURL("jdbc:h2:mem:db1")
    ds.setUser("sa")
    ds.setPassword("")

    val conn = ds.getConnection

    prepareDB(ds)

    (ds, conn)
  }

  private def prepareDB(dataSource: DataSource): Unit ={

    println("during setUp")
    dataSource.executeUpdate(
      """
       drop table if exists users;

       create table users(
        name varchar(20) not null,
        email varchar(32),
        age int
       );

       insert into users values('user1', 'wangzaixiang@gmail.com', 40);
       insert into users values('user2', 'rainbo.liu@gmail.com', 38);

      """)
  }

}
