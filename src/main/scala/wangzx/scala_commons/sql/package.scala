package wangzx.scala_commons

import javax.sql.DataSource
import java.sql.Connection
import java.sql.ResultSet

import scala.language.implicitConversions

package object sql {

  implicit def enhanceConnection(conn: Connection)
                                (implicit jdbcValueMapperFactory: JdbcValueMapperFactory = NullJdbcValueMapperFactory) =
    new RichConnection(conn)(jdbcValueMapperFactory)

  implicit def enhanceDataSource(datasource: DataSource)
                                (implicit  jdbcValueMapperFactory: JdbcValueMapperFactory = NullJdbcValueMapperFactory) =
    new RichDataSource(datasource)(jdbcValueMapperFactory)

  implicit def enhanceStringContext(sc: StringContext) = new SQLStringContext(sc)

  implicit def enhancePlainSql(stmt: String) = SQLWithArgs(stmt, null)

  object NullJdbcValueMapperFactory extends JdbcValueMapperFactory {
    def getJdbcValueMapper[T](`type`: Class[T]) =
      if(classOf[JdbcValueMapper[_]].isAssignableFrom(`type`))
        `type`.newInstance.asInstanceOf[JdbcValueMapper[T]]
      else null
  }

}

package sql {

  case class SQLWithArgs(sql: String, args: Seq[Any]) {

    def +(other: SQLWithArgs): SQLWithArgs =
      SQLWithArgs(sql + other.sql, args ++ other.args)

    def +(other: String): SQLWithArgs = SQLWithArgs(sql + other, args)

  }

  // when pass a JdbcValue to sql as placeholder, we will call it's getJdbcValue
  // which should be JDBC compatiable, see java.sql.Types
  trait JdbcValueMapper[T] {
    def getJdbcValue(bean: T): AnyRef
    def getBeanValue(jdbcValue: AnyRef, `type`: Class[T]): T
  }

  // we can build a extension mapper factory for UDT such as enums and etc.
  trait JdbcValueMapperFactory {
    def getJdbcValueMapper[T](`type`: Class[T]): JdbcValueMapper[T]
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