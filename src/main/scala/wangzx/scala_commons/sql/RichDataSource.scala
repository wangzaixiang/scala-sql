package wangzx.scala_commons.sql

import java.sql._
import javax.sql.DataSource
import scala.reflect.ClassTag

class RichDataSource(val datasource: DataSource)(implicit val jdbcValueMapperFactory: JdbcValueMapperFactory) {

  def withConnection[T](f: Connection => T): T = {
    val conn = datasource.getConnection
    try {
      f(conn)
    } finally {
      conn.close()
    }
  }

  def withStatement[T](f: Statement => T): T = withConnection(_.withStatement(f))


  def withTransaction[T](f: Connection => T): T = withConnection(_.withTransaction(f))

  def executeUpdate(stmt: SQLWithArgs): Int =
    executeUpdateWithGenerateKey(stmt)(null)

  def executeUpdateWithGenerateKey(sql: SQLWithArgs)(processGenerateKeys: ResultSet => Unit): Int =
    withConnection(_.executeUpdateWithGenerateKey(sql)(processGenerateKeys))

  def eachRow[T <: AnyRef](sql: SQLWithArgs)(f: T => Unit)(implicit ct: ClassTag[T]) =
    withConnection(_.eachRow(sql)(f)(ct))


  def rows[T <: AnyRef](sql: SQLWithArgs)(implicit ct: ClassTag[T]): List[T] = withConnection(_.rows(sql)(ct))

  def queryInt(sql: SQLWithArgs): Int = withConnection(_.queryInt(sql))

}