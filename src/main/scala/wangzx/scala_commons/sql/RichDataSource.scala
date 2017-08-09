package wangzx.scala_commons.sql

import java.sql._
import javax.sql.DataSource
import scala.reflect.ClassTag

class RichDataSource(val datasource: DataSource) {

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

  def generateKey[T: JdbcValueAccessor](sql: SQLWithArgs): T =
    withConnection(_.generateKey[T](sql))

  def eachRow[T : ResultSetMapper](sql: SQLWithArgs)(f: T => Unit) =
    withConnection(_.eachRow[T](sql)(f))


  def rows[T : ResultSetMapper](sql: SQLWithArgs): List[T] = withConnection(_.rows[T](sql))

  def row[T : ResultSetMapper](sql: SQLWithArgs): Option[T] = withConnection(_.row[T](sql))

  def queryInt(sql: SQLWithArgs): Int = withConnection(_.queryInt(sql))

}