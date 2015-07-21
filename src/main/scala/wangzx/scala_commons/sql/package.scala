package wangzx.scala_commons

import javax.sql.DataSource
import java.sql.Connection
import java.sql.ResultSet

import scala.language.implicitConversions

package object sql {

  implicit def enhanceConnection(conn: Connection) = new RichConnection(conn)

  implicit def enhanceDataSource(datasource: DataSource) = new RichDataSource(datasource)

  implicit def enhanceStringContext(sc: StringContext) = new SQLStringContext(sc)

  implicit def enhancePlainSql(stmt: String) = SQLWithArgs(stmt, null)

}

package sql {

case class SQLWithArgs(sql: String, args: Seq[Any]) {

    def +(other: SQLWithArgs): SQLWithArgs =
      SQLWithArgs(sql + other.sql, args ++ other.args)

    def +(other: String): SQLWithArgs = SQLWithArgs(sql + other, args)

  }

  class SQLStringContext(sc: StringContext) {
    def sql(args: Any*) = SQLWithArgs(sc.parts.mkString("?"), args)
  }

  /**
   * instead of using reflect mechanism, a bean maybe read field from ResultSet itself
   */
  trait ResultSetConvertable {
    def fromResultSet(rs: ResultSet)
  }

}