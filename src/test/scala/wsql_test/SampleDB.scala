package wsql_test

import javax.sql.DataSource
import wsql.{given, *}

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

  private def prepareDB(dataSource: DataSource): Unit = {

    dataSource.executeUpdate(
      sql"""
       drop table if exists users;

       create table users(
        name varchar(20) not null,
        email varchar(32),
        age int
       );

       insert into users values('user1', 'user1@gmail.com', 40);
       insert into users values('user2', 'user2@gmail.com', 38);

      """)

    // table: test1 for column types test
    dataSource executeUpdate
      sql"""
        drop table if exists test1;

        create table test1(
          id int primary key,
          name varchar(255),
          is_active boolean,
          tiny_int tinyint,
          small_int smallint,
          normal_int int,
          big_int bigint,
          float_value float,
          double_value double,
          decimal_value decimal(10,2),
          birthday date,
          created_at timestamp,
          updated_at timestamp,
          blob_value blob,
          empty_value char(10),
          empty_int int
        );

        insert into test1 set
          id = 1,
          name = 'test1',
          is_active = true,
          tiny_int = 1,
          small_int = 500,
          normal_int = 100000,
          big_int = 4,
          float_value = 1.1,
          double_value = 2.2,
          decimal_value = 3.3,
          birthday = '2020-01-01',
          created_at = '2020-01-01 00:00:00',
          updated_at = '2020-01-01 00:00:00',
          blob_value = '74657374'
      """
  }

}
