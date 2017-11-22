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

  def joinRows2[T1: ResultSetMapper, T2: ResultSetMapper](sql: SQLWithArgs): List[(T1, T2)] = withConnection(_.joinRows2[T1,T2](sql))
  def joinRows3[T1: ResultSetMapper, T2: ResultSetMapper, T3: ResultSetMapper](sql: SQLWithArgs): List[(T1, T2, T3)] = withConnection(_.joinRows3[T1,T2, T3](sql))
  def joinRows4[T1: ResultSetMapper, T2: ResultSetMapper, T3: ResultSetMapper, T4: ResultSetMapper](sql: SQLWithArgs): List[(T1, T2, T3, T4)] = withConnection(_.joinRows4[T1,T2,T3,T4](sql))

  def row[T : ResultSetMapper](sql: SQLWithArgs): Option[T] = withConnection(_.row[T](sql))
  def joinRow2[T1: ResultSetMapper, T2:ResultSetMapper](sql: SQLWithArgs): Option[(T1, T2)] = withConnection(_.joinRow2[T1, T2](sql))
  def joinRow3[T1: ResultSetMapper, T2:ResultSetMapper, T3:ResultSetMapper](sql: SQLWithArgs): Option[(T1, T2, T3)] = withConnection(_.joinRow3[T1, T2, T3](sql))
  def joinRow4[T1: ResultSetMapper, T2:ResultSetMapper, T3:ResultSetMapper, T4:ResultSetMapper](sql: SQLWithArgs): Option[(T1, T2, T3, T4)] = withConnection(_.joinRow4[T1, T2, T3, T4](sql))

  def queryInt(sql: SQLWithArgs): Int = withConnection(_.queryInt(sql))

}