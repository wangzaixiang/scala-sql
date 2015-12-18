package wangzx.scala_commons.sql

import java.sql._
import scala.reflect.ClassTag
import scala.collection.mutable.ListBuffer

object RichConnection {
  val ClassOfResultSet = classOf[ResultSet]
  val ClassOfRow = classOf[Row]
  val ClassOfJdbcValueMapping = classOf[JdbcValueMapper[_]]

  import org.slf4j.{LoggerFactory, Logger}
  val LOG: Logger = LoggerFactory.getLogger(classOf[RichConnection])

}

class RichConnection(val conn: Connection)(val jdbcValueMapperFactory: JdbcValueMapperFactory) {

  import RichConnection._
  import BeanMapping._

  /**
   * map a ResultSet to an object, either ResultSet or Row or JavaBean
   * 2015-11-10 add support for primitive type which maps to the first column only
   * TODO add support for JdbcValueMapper
   */
  private def rs2mapped[T](rsMeta: ResultSetMetaData, rs: ResultSet, tag: ClassTag[T]): T = {
    tag.runtimeClass match {
      case BeanMapping.ClassOfByte | java.lang.Byte.TYPE =>
        rs.getByte(1).asInstanceOf[T]
      case BeanMapping.ClassOfShort | java.lang.Short.TYPE =>
        rs.getShort(1).asInstanceOf[T]
      case BeanMapping.ClassOfInteger | java.lang.Integer.TYPE =>
        rs.getInt(1).asInstanceOf[T]
      case BeanMapping.ClassOfFloat | java.lang.Float.TYPE =>
        rs.getFloat(1).asInstanceOf[T]
      case BeanMapping.ClassOfDouble | java.lang.Double.TYPE =>
        rs.getDouble(1).asInstanceOf[T]
      case BeanMapping.ClassOfLong | java.lang.Long.TYPE =>
        rs.getLong(1).asInstanceOf[T]
      case BeanMapping.ClassOfBigDecimal =>
        rs.getBigDecimal(1).asInstanceOf[T]
      case BeanMapping.ClassOfScalaBigDecimal =>
        BigDecimal(rs.getBigDecimal(1)).asInstanceOf[T]
      case BeanMapping.ClassOfSQLDate =>
        rs.getDate(1).asInstanceOf[T]
      case BeanMapping.ClassOfSQLTime =>
        rs.getTime(1).asInstanceOf[T]
      case BeanMapping.ClassOfUtilDate | BeanMapping.ClassOfSQLTimestamp =>
        rs.getTimestamp(1).asInstanceOf[T]

      case ClassOfResultSet =>
        rs.asInstanceOf[T]
      case ClassOfRow =>
        Row.resultSetToRow(rsMeta, rs).asInstanceOf[T]
      case _ =>
        BeanMapping.rs2bean(rsMeta, rs, jdbcValueMapperFactory)(tag)
    }
  }

  def withStatement[T](f: Statement => T): T = {
    val stmt = conn.createStatement
    try {
      f(stmt)
    } finally {
      stmt.close()
    }
  }

  private def withPreparedStatement[T](sql: String)(f: PreparedStatement => T): T = {
    val stmt = conn.prepareStatement(sql)
    try {
      f(stmt)
    } finally {
      stmt.close()
    }
  }

  def withTransaction[T](f: Connection => T): T = {
    try {
      conn.setAutoCommit(false)
      val result = f(conn)
      conn.commit
      result
    } catch {
      case ex: Throwable =>
        conn.rollback
        throw ex
    }
  }

  def executeUpdate(stmt: SQLWithArgs): Int = executeUpdateWithGenerateKey(stmt)(null)

  @inline private def setStatementArgs(stmt: PreparedStatement, args: Seq[Any]) =
    args.zipWithIndex.foreach {
      case (v: JdbcValueMapper[AnyRef], idx) => stmt.setObject(idx+1, v.getJdbcValue(v))
      case (v, idx) if v != null && jdbcValueMapperFactory.getJdbcValueMapper(v.getClass) != null =>
        val mapper = jdbcValueMapperFactory.getJdbcValueMapper(v.getClass).asInstanceOf[JdbcValueMapper[Any]]
        stmt.setObject( idx+1, mapper.getJdbcValue(v) )
      case (v: BigDecimal, idx) => stmt.setBigDecimal(idx+1, v.bigDecimal)
      case (v, idx) => stmt.setObject(idx + 1, v)
    }

  def executeUpdateWithGenerateKey(stmt: SQLWithArgs)(processGenerateKeys: ResultSet => Unit = null): Int = {
    val prepared = conn.prepareStatement(stmt.sql,
      if (processGenerateKeys != null) Statement.RETURN_GENERATED_KEYS
      else Statement.NO_GENERATED_KEYS)

    try {
      if (stmt.args != null) setStatementArgs(prepared, stmt.args)

      LOG.debug("SQL Preparing: {} args: {}", Seq(stmt.sql, stmt.args): _*)

      val result = prepared.executeUpdate()

      if (processGenerateKeys != null) {
        val keys = prepared.getGeneratedKeys
        processGenerateKeys(keys)
      }

      LOG.debug("SQL result: {}", result)
      result
    }
    finally  {
      prepared.close
    }
  }

  def eachRow[T : ClassTag](sql: SQLWithArgs)(f: T => Unit) = withPreparedStatement(sql.sql){ prepared =>
//    val prepared = conn.prepareStatement(sql.sql)
    if (sql.args != null) setStatementArgs(prepared, sql.args)

    LOG.debug("SQL Preparing: {} args: {}", Seq(sql.sql, sql.args):_*)

    val rs = prepared.executeQuery()
    val rsMeta = rs.getMetaData
    while (rs.next()) {
      val mapped = rs2mapped(rsMeta, rs, implicitly[ClassTag[T]])
      f(mapped)
    }
    LOG.debug("SQL result: {}", rs.getRow)
  }

  def rows[T : ClassTag](sql: SQLWithArgs): List[T] = withPreparedStatement(sql.sql) { prepared =>
    val buffer = new ListBuffer[T]()
//    val prepared = conn.prepareStatement(sql.sql)
    if (sql.args != null) setStatementArgs(prepared, sql.args)

    LOG.debug("SQL Preparing: {} args: {}", Seq(sql.sql, sql.args):_*)

    val rs = prepared.executeQuery()
    val rsMeta = rs.getMetaData
    while (rs.next()) {
      val mapped = rs2mapped(rsMeta, rs, implicitly[ClassTag[T]])
      buffer += mapped

    }
    LOG.debug("SQL result: {}", buffer.size)
    buffer.toList
  }

  def row[T: ClassTag](sql: SQLWithArgs): Option[T] = withPreparedStatement(sql.sql) { prepared =>
    if (sql.args != null) setStatementArgs(prepared, sql.args)

    LOG.debug("SQL Preparing: {} args: {}", Seq(sql.sql, sql.args): _*)

    val rs = prepared.executeQuery()
    val rsMeta = rs.getMetaData

    var result: Option[T] = None
    var index = -1
    while (index == -1 && rs.next()) {
      index += 1
      result = Some(rs2mapped(rsMeta, rs, implicitly[ClassTag[T]]))
    }
    if(rs.next)
      LOG.warn("expect 1 row but really more. SQL result: {}", rs.getRow - 1)
    else
      LOG.debug("SQL result: {}", rs.getRow)

    result
  }

  def queryInt(sql: SQLWithArgs): Int = withPreparedStatement(sql.sql){ prepared =>
//    val prepared = conn.prepareStatement(sql.sql)
    if(sql.args != null) setStatementArgs(prepared, sql.args)

    LOG.debug("SQL Preparing: {} args: {}", Seq(sql.sql, sql.args):_*)

    val rs = prepared.executeQuery()

    if(rs.next) {
      rs.getInt(1)
    } else throw new IllegalArgumentException("query return no rows")
  }

}