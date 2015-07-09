package wangzx.scala_commons.sql_test

import java.sql.{Date, ResultSet}

import wangzx.scala_commons.sql._

import scala.util.Random

object TestSQL {

  val datasource = {
    val ds = new org.h2.jdbcx.JdbcDataSource
    ds.setURL("jdbc:h2:./db/test")
    ds.setUser("sa")
    ds.setPassword("")
    ds
  }

  @Table("student")
  class Student {
    var name: String = _
    var email: String = _
    var birthday: Date = _
    var mobile: String = _
  }

  def main(args: Array[String]) {

    datasource executeUpdate """drop table if exists student"""

    datasource executeUpdate """create table if not exists student (
    	name varchar(18),
    	email	varchar(32),
    	birthday	date,
    	mobile	varchar(11)
    )"""

    val name = "wangzx"
    val email = "wangzaixiang@gmail.com"
    val birthday = Date.valueOf("2000-01-01")
    val mobile = "18612345678"

    datasource executeUpdate sql"insert into student values(${name}, ${email}, ${birthday}, ${mobile})"

    // using simple orm to insert a record
    val student = new Student {
      name = "wangzx"
      email = s"wangzx${Random.nextInt}@gmail.com"
      birthday = Date.valueOf("2000-01-01")
      mobile = "18612345678"
    }
    datasource.insert(student)
    
    datasource.update(student)
    
    datasource.delete(student)

    println("iterate students using ResultSet")
    datasource.eachRow(sql"select * from student where name = ${name}") { rs: ResultSet =>
      println(s"""name = ${rs.getString("name")} email = ${rs.getString("email")} birthday = ${rs.getDate("birthday")}""")
    }
    println()

    val sharedSql = sql"""and mobile=${mobile}"""

    println("iterate students using sharedSql")
    datasource.eachRow(sql"select * from student where name = ${name} " + sharedSql + " and 1=1") { it: Student =>
      println(s"name = ${it.name} email=${it.email} birthday=${it.birthday} mobile=${mobile}")
    }
    println()

    println("iterate students using orm")
    datasource.eachRow(sql"select * from student where name = ${name}") { it: Student =>
      println(s"name = ${it.name} email=${it.email} birthday=${it.birthday}")
    }
    println()
    
    val rows = datasource.rows[Row](sql"select * from student where name = ${name}")
    rows.foreach { row =>
      println(s"""name = ${row.getString("name")} email=${row.getString("email")} birthday=${row.getDate("birthday")}""")
    }
    
    val students = datasource.rows[Student](sql"select * from student where name = ${name}")
    
    val count = datasource.rows[Row]("select count(*) from student").head.getInt(1)
    println(s"total rows: $count")

  }

}