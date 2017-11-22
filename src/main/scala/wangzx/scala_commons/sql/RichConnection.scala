package wangzx.scala_commons.sql

import java.sql._


import scala.collection.mutable.ListBuffer

object RichConnection {


  import org.slf4j.{LoggerFactory, Logger}
  val LOG: Logger = LoggerFactory.getLogger(classOf[RichConnection])

}

class RichConnection(val conn: Connection) {

  import RichConnection._

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

  @inline private def setStatementArgs(stmt: PreparedStatement, args: Seq[JdbcValue[_]]) =
    args.zipWithIndex.foreach {
      case (null, idx) => stmt.setNull( idx+1, Types.VARCHAR )
      case (v, idx) => v.passIn(stmt, idx+1)
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

  def generateKey[T: JdbcValueAccessor](stmt: SQLWithArgs): T = {
    var t: Option[T] = None

    executeUpdateWithGenerateKey(stmt){ rs =>
      if(rs.next)
        t = Some( implicitly[JdbcValueAccessor[T]].passOut(rs, 1) )
    }

    assert( t.isDefined, s"the sql doesn't return a generated key but expected" )
    t.get
  }

  def eachRow[T : ResultSetMapper](sql: SQLWithArgs)(f: T => Unit) = withPreparedStatement(sql.sql){ prepared =>
    if (sql.args != null) setStatementArgs(prepared, sql.args)

    LOG.debug("SQL Preparing: {} args: {}", Seq(sql.sql, sql.args):_*)

    val mapper = implicitly[ResultSetMapper[T]]
    val rs = prepared.executeQuery()
    val rsMeta = rs.getMetaData
    while (rs.next()) {
      val mapped = mapper.from(rs)
      f(mapped)
    }
    LOG.debug("SQL result: {}", rs.getRow)
  }

  def rows[T : ResultSetMapper](sql: SQLWithArgs): List[T] = withPreparedStatement(sql.sql) { prepared =>
    val buffer = new ListBuffer[T]()
    if (sql.args != null) setStatementArgs(prepared, sql.args)

    LOG.debug("SQL Preparing: {} args: {}", Seq(sql.sql, sql.args):_*)

    val rs = prepared.executeQuery()
    val rsMeta = rs.getMetaData
    while (rs.next()) {
      val mapped = implicitly[ResultSetMapper[T]].from(rs)
      buffer += mapped

    }
    LOG.debug("SQL result: {}", buffer.size)
    buffer.toList
  }

  def joinRows2[T1: ResultSetMapper, T2: ResultSetMapper](sql: SQLWithArgs): List[(T1, T2)] = withPreparedStatement(sql.sql) { prepared =>
    val buffer = new ListBuffer[(T1,T2)]()
    if(sql.args != null) setStatementArgs(prepared, sql.args)

    LOG.debug("SQL Preparing: {} args: {}", Seq(sql.sql, sql.args):_*)

    val rs = prepared.executeQuery()
    val rsMeta = rs.getMetaData
    while (rs.next()) {
      val t1: T1 = implicitly[ResultSetMapper[T1]].from(rs)
      val t2: T2 = implicitly[ResultSetMapper[T2]].from(rs)
      buffer += Tuple2(t1, t2)
    }

    LOG.debug("SQL result: {}", buffer.size)

    buffer.toList
  }

  def joinRows3[T1: ResultSetMapper, T2: ResultSetMapper, T3: ResultSetMapper](sql: SQLWithArgs): List[(T1, T2, T3)] = withPreparedStatement(sql.sql) { prepared =>
    val buffer = new ListBuffer[(T1,T2, T3)]()
    if(sql.args != null) setStatementArgs(prepared, sql.args)

    LOG.debug("SQL Preparing: {} args: {}", Seq(sql.sql, sql.args):_*)

    val rs = prepared.executeQuery()
    val rsMeta = rs.getMetaData
    while (rs.next()) {
      val t1: T1 = implicitly[ResultSetMapper[T1]].from(rs)
      val t2: T2 = implicitly[ResultSetMapper[T2]].from(rs)
      val t3: T3 = implicitly[ResultSetMapper[T3]].from(rs)
      buffer += Tuple3(t1, t2, t3)
    }

    LOG.debug("SQL result: {}", buffer.size)

    buffer.toList
  }

  def joinRows4[T1: ResultSetMapper, T2: ResultSetMapper, T3: ResultSetMapper, T4: ResultSetMapper](sql: SQLWithArgs): List[(T1, T2, T3, T4)] = withPreparedStatement(sql.sql) { prepared =>
    val buffer = new ListBuffer[(T1,T2, T3, T4)]()
    if(sql.args != null) setStatementArgs(prepared, sql.args)

    LOG.debug("SQL Preparing: {} args: {}", Seq(sql.sql, sql.args):_*)

    val rs = prepared.executeQuery()
    val rsMeta = rs.getMetaData
    while (rs.next()) {
      val t1: T1 = implicitly[ResultSetMapper[T1]].from(rs)
      val t2: T2 = implicitly[ResultSetMapper[T2]].from(rs)
      val t3: T3 = implicitly[ResultSetMapper[T3]].from(rs)
      val t4: T4 = implicitly[ResultSetMapper[T4]].from(rs)
      buffer += Tuple4(t1, t2, t3, t4)
    }

    LOG.debug("SQL result: {}", buffer.size)

    buffer.toList
  }

  def row[T: ResultSetMapper](sql: SQLWithArgs): Option[T] = withPreparedStatement(sql.sql) { prepared =>
    if (sql.args != null) setStatementArgs(prepared, sql.args)

    LOG.debug("SQL Preparing: {} args: {}", Seq(sql.sql, sql.args): _*)

    val rs = prepared.executeQuery()

    var result: Option[T] = None
    var index = -1
    while (index == -1 && rs.next()) {
      index += 1
      result = Some(implicitly[ResultSetMapper[T]].from(rs))
    }
    if(rs.next)
      LOG.warn("expect 1 row but really more. SQL result: {}", rs.getRow - 1)
    else
      LOG.debug("SQL result: {}", rs.getRow)

    result
  }

  def joinRow2[T1: ResultSetMapper, T2: ResultSetMapper](sql: SQLWithArgs): Option[(T1, T2)] = withPreparedStatement(sql.sql) { prepared =>
    if (sql.args != null) setStatementArgs(prepared, sql.args)

    LOG.debug("SQL Preparing: {} args: {}", Seq(sql.sql, sql.args): _*)

    val rs = prepared.executeQuery()

    var result: Option[(T1, T2)] = None
    var index = -1
    while (index == -1 && rs.next()) {
      index += 1
      val t1 = implicitly[ResultSetMapper[T1]].from(rs)
      val t2 = implicitly[ResultSetMapper[T2]].from(rs)
      result = Some(Tuple2(t1, t2))
    }
    if(rs.next)
      LOG.warn("expect 1 row but really more. SQL result: {}", rs.getRow - 1)
    else
      LOG.debug("SQL result: {}", rs.getRow)

    result
  }

  def joinRow3[T1: ResultSetMapper, T2: ResultSetMapper, T3: ResultSetMapper](sql: SQLWithArgs): Option[(T1, T2, T3)] = withPreparedStatement(sql.sql) { prepared =>
    if (sql.args != null) setStatementArgs(prepared, sql.args)

    LOG.debug("SQL Preparing: {} args: {}", Seq(sql.sql, sql.args): _*)

    val rs = prepared.executeQuery()

    var result: Option[(T1, T2, T3)] = None
    var index = -1
    while (index == -1 && rs.next()) {
      index += 1
      val t1 = implicitly[ResultSetMapper[T1]].from(rs)
      val t2 = implicitly[ResultSetMapper[T2]].from(rs)
      val t3 = implicitly[ResultSetMapper[T3]].from(rs)
      result = Some(Tuple3(t1, t2, t3))
    }
    if(rs.next)
      LOG.warn("expect 1 row but really more. SQL result: {}", rs.getRow - 1)
    else
      LOG.debug("SQL result: {}", rs.getRow)

    result
  }

  def joinRow4[T1: ResultSetMapper, T2: ResultSetMapper, T3: ResultSetMapper, T4: ResultSetMapper](sql: SQLWithArgs): Option[(T1, T2, T3, T4)] = withPreparedStatement(sql.sql) { prepared =>
    if (sql.args != null) setStatementArgs(prepared, sql.args)

    LOG.debug("SQL Preparing: {} args: {}", Seq(sql.sql, sql.args): _*)

    val rs = prepared.executeQuery()

    var result: Option[(T1, T2, T3, T4)] = None
    var index = -1
    while (index == -1 && rs.next()) {
      index += 1
      val t1 = implicitly[ResultSetMapper[T1]].from(rs)
      val t2 = implicitly[ResultSetMapper[T2]].from(rs)
      val t3 = implicitly[ResultSetMapper[T3]].from(rs)
      val t4 = implicitly[ResultSetMapper[T4]].from(rs)
      result = Some(Tuple4(t1, t2, t3, t4))
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