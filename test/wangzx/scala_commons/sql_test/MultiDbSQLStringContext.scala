package wangzx.scala_commons.sql_test

import scala.language.experimental.macros
import wangzx.scala_commons.sql._
import scala.reflect.macros.Context

/**
 * if you project need to access different database and
 * would the compiler check the statment, you need write
 * a simple wrapper like this
 */
object MultiDbSQLStringContext {
  
  implicit class SQLStringContextWithCheck(sc: StringContext) {
    def db1(args: Any*) = macro checkDB1
    def db2(args: Any*) = macro checkDB2
  }

  def checkDB1(c: Context)(args: c.Expr[Any]*): c.Expr[SQLWithArgs] =
    StaticCheck.checkSQLImpl(c)(args: _*)("db1")
  def checkDB2(c: Context)(args: c.Expr[Any]*): c.Expr[SQLWithArgs] =
    StaticCheck.checkSQLImpl(c)(args: _*)("db2")

}
