package wangzx.scala_commons.sql

import java.io.FileInputStream
import java.sql.{Connection, Driver, ResultSet}

import scala.util.Properties
//import java.util.Properties
import javax.sql.DataSource

import scala.language.experimental.macros


object Macros {

  //def implictUserMapper: ResultSetMapper[User] = macro implictMapper[User]
  /*
  // Case class Style
  implicit object UserMapper extends CaseClassResultSetMapper[User](User) {

    override def from(rs: ResultSet): User = {
      val NAME = Field[String]("name")
      val AGE = Field[Int]("age")
      val CLASSROOM = Field[Int]("classRoom", Some("apply$default$3"))

      User(NAME(rs), AGE(rs), CLASSROOM(rs))
    }


  }
  }
   */
}
