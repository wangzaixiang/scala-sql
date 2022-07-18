package wsql

import java.sql.*
import scala.collection.mutable.ListBuffer
import wsql.macros.BatchMacros

given ConnectionOps with

  import org.slf4j.{LoggerFactory, Logger}
  val LOG: Logger = LoggerFactory.getLogger("wsql").nn

  extension (conn: Connection)

    def withStatement[T](f: Statement => T): T =
      val stmt = conn.createStatement.nn
      try
        f(stmt)
      finally
        stmt.close()

    private def withPreparedStatement[T](sql: String)(f: PreparedStatement => T): T =
      val stmt = conn.prepareStatement(sql).nn
      try
        f(stmt)
      finally
        stmt.close()

    def withTransaction[T](f: Connection => T): T =
      try
        conn.setAutoCommit(false)
        val result = f(conn)
        conn.commit
        result
      catch
        case ex: Throwable =>
          conn.rollback
          throw ex

    inline def createBatch[T](inline proc: T => SQLWithArgs): Batch[T] =
      ${ BatchMacros.createBatchImpl[T]('proc, 'conn) }

    /**
      * translate the "insert into table set a = ?, b = ?" into "insert into table(a,b) values(?,?)
      */
    inline def createMysqlBatch[T](proc: T => SQLWithArgs): Batch[T] =
      ${ BatchMacros.createMysqlBatchImpl[T]('proc, 'conn) }

    def executeUpdate(stmt: SQLWithArgs): Int = executeUpdateWithGenerateKey(stmt)(rs=>())

    @inline private def setStatementArgs(stmt: PreparedStatement, args: Seq[JdbcValue[_]|Null]) =
      args.zipWithIndex.foreach { case (v, idx) =>
        //        case (v, idx) =>
        if (v == null) stmt.setNull(idx + 1, Types.VARCHAR) else v.passIn(stmt, idx + 1)
      }

    // TODO provide a NOOP operation
    def executeUpdateWithGenerateKey(stmt: SQLWithArgs)(processGenerateKeys: ResultSet => Unit): Int =
      val prepared = conn.prepareStatement(stmt.sql,
        if(processGenerateKeys != NoopProcessor) Statement.RETURN_GENERATED_KEYS
        else Statement.NO_GENERATED_KEYS ).nn

      try
        setStatementArgs(prepared, stmt.args)
        LOG.debug("SQL Preparing: {} args: {}", Seq(stmt.sql, stmt.args): _*)

        val result = prepared.executeUpdate().nn
        if(processGenerateKeys != NoopProcessor)
          val keys = prepared.getGeneratedKeys.nn
          processGenerateKeys(keys)

        LOG.debug("SQL result: {}", result)
        result
      finally
        prepared.close

    def generateKey[T: JdbcValueAccessor](stmt: SQLWithArgs): T =
      var t: Option[T] = None

      executeUpdateWithGenerateKey(stmt) { rs =>
        if (rs.next)
          val x = summon[JdbcValueAccessor[T]].passOut(rs, 1)
          Option(x.asInstanceOf[AnyRef]).asInstanceOf[Option[T]]
      }

      assert(t.isDefined, s"the sql doesn't return a generated key but expected")
      t.get


    def eachRow[T: ResultSetMapper](sql: SQLWithArgs)(f: T => Unit): Unit = withPreparedStatement(sql.sql) { prepared =>
      setStatementArgs(prepared, sql.args)

      LOG.debug("SQL Preparing: {} args: {}", Seq(sql.sql, sql.args): _*)

      val mapper = summon[ResultSetMapper[T]]
      val rs = prepared.executeQuery().nn

      var rowCount = 0
      while (rs.next())
        f( mapper.from(rs) )
        rowCount += 1
      LOG.debug("SQL result: {}", rowCount)
    }

    def rows[T: ResultSetMapper](sql: SQLWithArgs): List[T] = withPreparedStatement(sql.sql) { prepared =>
      val buffer = new ListBuffer[T]()
      setStatementArgs(prepared, sql.args)

      LOG.debug("SQL Preparing: {} args: {}", Seq(sql.sql, sql.args): _*)

      val rs = prepared.executeQuery().nn

      while rs.next() do
        buffer += summon[ResultSetMapper[T]].from(rs)

      LOG.debug("SQL result: {}", buffer.size)
      buffer.toList
    }

    def joinRows2[T1: ResultSetMapper, T2: ResultSetMapper](sql: SQLWithArgs): List[(T1, T2)] = withPreparedStatement(sql.sql) { prepared =>
      val buffer = new ListBuffer[(T1, T2)]()
      setStatementArgs(prepared, sql.args)

      LOG.debug("SQL Preparing: {} args: {}", Seq(sql.sql, sql.args): _*)

      val rs = prepared.executeQuery().nn

      while (rs.next())
        val t1: T1 = summon[ResultSetMapper[T1]].from(rs)
        val t2: T2 = summon[ResultSetMapper[T2]].from(rs)
        buffer += Tuple2(t1, t2)

      LOG.debug("SQL result: {}", buffer.size)

      buffer.toList
    }

    def joinRows3[T1: ResultSetMapper, T2: ResultSetMapper, T3: ResultSetMapper](sql: SQLWithArgs): List[(T1, T2, T3)] = withPreparedStatement(sql.sql) { prepared =>
      val buffer = new ListBuffer[(T1, T2, T3)]()
      setStatementArgs(prepared, sql.args)

      LOG.debug("SQL Preparing: {} args: {}", Seq(sql.sql, sql.args): _*)

      val rs = prepared.executeQuery().nn

      while (rs.next())
        val t1: T1 = summon[ResultSetMapper[T1]].from(rs)
        val t2: T2 = summon[ResultSetMapper[T2]].from(rs)
        val t3: T3 = summon[ResultSetMapper[T3]].from(rs)
        buffer += Tuple3(t1, t2, t3)

      LOG.debug("SQL result: {}", buffer.size)

      buffer.toList
    }

    def joinRows4[T1: ResultSetMapper, T2: ResultSetMapper, T3: ResultSetMapper, T4: ResultSetMapper](sql: SQLWithArgs): List[(T1, T2, T3, T4)] = withPreparedStatement(sql.sql) { prepared =>
      val buffer = new ListBuffer[(T1, T2, T3, T4)]()
      setStatementArgs(prepared, sql.args)

      LOG.debug("SQL Preparing: {} args: {}", Seq(sql.sql, sql.args): _*)

      val rs = prepared.executeQuery().nn

      while (rs.next())
        val t1: T1 = summon[ResultSetMapper[T1]].from(rs)
        val t2: T2 = summon[ResultSetMapper[T2]].from(rs)
        val t3: T3 = summon[ResultSetMapper[T3]].from(rs)
        val t4: T4 = summon[ResultSetMapper[T4]].from(rs)
        buffer += Tuple4(t1, t2, t3, t4)

      LOG.debug("SQL result: {}", buffer.size)

      buffer.toList
    }

    def row[T: ResultSetMapper](sql: SQLWithArgs): Option[T] = withPreparedStatement(sql.sql) { prepared =>
      setStatementArgs(prepared, sql.args)

      LOG.debug("SQL Preparing: {} args: {}", Seq(sql.sql, sql.args): _*)

      val rs = prepared.executeQuery().nn

      var result: Option[T] = None
      if (rs.next())
        result = Some(summon[ResultSetMapper[T]].from(rs))
        if (rs.next())
          LOG.warn("expect 1 row but has more")
        else
          LOG.debug("SQL result: 1")
      else
        LOG.debug("SQL result: 0")

      result
    }

    def joinRow2[T1: ResultSetMapper, T2: ResultSetMapper](sql: SQLWithArgs): Option[(T1, T2)] = withPreparedStatement(sql.sql) { prepared =>
      setStatementArgs(prepared, sql.args)

      LOG.debug("SQL Preparing: {} args: {}", Seq(sql.sql, sql.args): _*)

      val rs = prepared.executeQuery().nn
      var result: Option[(T1, T2)] = None

      if (rs.next())
        val t1 = summon[ResultSetMapper[T1]].from(rs)
        val t2 = summon[ResultSetMapper[T2]].from(rs)
        result = Some(Tuple2(t1, t2))
        if (rs.next())
          LOG.warn("expect 1 row but has more")
        else
          LOG.debug("SQL result: 1")
      else
        LOG.debug("SQL result: 0")

      result
    }

    def joinRow3[T1: ResultSetMapper, T2: ResultSetMapper, T3: ResultSetMapper](sql: SQLWithArgs): Option[(T1, T2, T3)] = withPreparedStatement(sql.sql) { prepared =>
      setStatementArgs(prepared, sql.args)

      LOG.debug("SQL Preparing: {} args: {}", Seq(sql.sql, sql.args): _*)

      val rs = prepared.executeQuery().nn

      var result: Option[(T1, T2, T3)] = None

      if (rs.next())
        val t1 = summon[ResultSetMapper[T1]].from(rs)
        val t2 = summon[ResultSetMapper[T2]].from(rs)
        val t3 = summon[ResultSetMapper[T3]].from(rs)
        result = Some(Tuple3(t1, t2, t3))
        if (rs.next())
          LOG.warn("expect 1 row but has more")
        else
          LOG.debug("SQL result: 1")
      else
        LOG.debug("SQL result: 0")

      result
    }

    def joinRow4[T1: ResultSetMapper, T2: ResultSetMapper, T3: ResultSetMapper, T4: ResultSetMapper](sql: SQLWithArgs): Option[(T1, T2, T3, T4)] = withPreparedStatement(sql.sql) { prepared =>
      setStatementArgs(prepared, sql.args)

      LOG.debug("SQL Preparing: {} args: {}", Seq(sql.sql, sql.args): _*)

      val rs = prepared.executeQuery().nn

      var result: Option[(T1, T2, T3, T4)] = None

      if (rs.next())
        val t1 = summon[ResultSetMapper[T1]].from(rs)
        val t2 = summon[ResultSetMapper[T2]].from(rs)
        val t3 = summon[ResultSetMapper[T3]].from(rs)
        val t4 = summon[ResultSetMapper[T4]].from(rs)
        result = Some(Tuple4(t1, t2, t3, t4))
        if (rs.next())
          LOG.warn("expect 1 row but has more")
        else
          LOG.debug("SQL result: 1")
      else
        LOG.debug("SQL result: 0")

      result
    }

    def queryInt(sql: SQLWithArgs): Int = withPreparedStatement(sql.sql) { prepared =>
      //    val prepared = conn.prepareStatement(sql.sql)
      setStatementArgs(prepared, sql.args)

      LOG.debug("SQL Preparing: {} args: {}", Seq(sql.sql, sql.args): _*)

      val rs = prepared.executeQuery().nn

      if (rs.next)
        rs.getInt(1)
      else throw new IllegalArgumentException("query return no rows")
    }

given DataSourceOps with

  extension (datasource: javax.sql.DataSource)
    private def withConnection[T](f: Connection => T): T =
      val conn = datasource.getConnection.nn
      try
        f(conn)
      finally
        conn.close()

    def withStatement[T](f: Statement => T): T = withConnection(_.withStatement(f))

    def withTransaction[T](f: Connection => T): T = withConnection(_.withTransaction(f))

    inline def createBatch[T](proc: T=>SQLWithArgs): Batch[T] =
      withConnection(conn => conn.createBatch(proc))

    inline def createMysqlBatch[T](proc: T=>SQLWithArgs): Batch[T] =
      withConnection(conn => conn.createMysqlBatch(proc))

    def executeUpdate(stmt: SQLWithArgs): Int =
      executeUpdateWithGenerateKey(stmt)(NoopProcessor)

    def executeUpdateWithGenerateKey(sql: SQLWithArgs)(processGenerateKeys: ResultSet => Unit = NoopProcessor): Int =
      withConnection(_.executeUpdateWithGenerateKey(sql)(processGenerateKeys))

    def generateKey[T: JdbcValueAccessor](sql: SQLWithArgs): T =
      withConnection(_.generateKey[T](sql))

    def eachRow[T : ResultSetMapper](sql: SQLWithArgs)(f: T => Unit): Unit =
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



