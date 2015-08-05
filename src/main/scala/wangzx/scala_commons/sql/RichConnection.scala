package wangzx.scala_commons.sql

import java.sql._
import scala.reflect.ClassTag
import scala.collection.mutable.ListBuffer
import java.beans.PropertyDescriptor
import java.lang.reflect.Field
import java.lang.reflect.Modifier
import scala.ref.SoftReference

object RichConnection {
  val ClassOfResultSet = classOf[ResultSet]
  val ClassOfRow = classOf[Row]

}

class RichConnection(conn: Connection) {

  import RichConnection._
  import BeanMapping._

  /**
   * map a ResultSet to an object, either ResultSet or Row or JavaBean
   */
  private def rs2mapped[T](rsMeta: ResultSetMetaData, rs: ResultSet, tag: ClassTag[T]): T = {
    tag.runtimeClass match {
      case ClassOfResultSet => rs.asInstanceOf[T]
      case ClassOfRow => Row.resultSetToRow(rsMeta, rs).asInstanceOf[T]
      case _ => rs2bean(rsMeta, rs)(tag)
    }
  }

  /**
   * mapping from ResultSet to JavaBean, field is JavaBean Property, and may annotated with @Column
   * TODO support JdbcValue such as enums.
   */
  def rs2bean[T: ClassTag](rsMeta: ResultSetMetaData, rs: ResultSet): T = {
    val bean: T = implicitly[ClassTag[T]].runtimeClass.newInstance().asInstanceOf[T]

    bean match {
      case rsConvertable: ResultSetConvertable =>
        rsConvertable.fromResultSet(rs)
      case _ =>
        val beanMapping = BeanMapping.getBeanMapping(bean.getClass).asInstanceOf[BeanMapping[T]]
        for (idx <- 1 to rsMeta.getColumnCount) {
          val label = rsMeta.getColumnLabel(idx).toLowerCase()
          beanMapping.getFieldByColumnName(label) match {
            case Some(fieldMapping) =>
              val value = fieldMapping.fieldType match {
                case java.lang.Boolean.TYPE | ClassOfBoolean => rs.getBoolean(idx)
                case java.lang.Byte.TYPE | ClassOfByte => rs.getByte(idx)
                case java.lang.Short.TYPE | ClassOfShort => rs.getShort(idx)
                case java.lang.Integer.TYPE | ClassOfInteger => rs.getInt(idx)
                case java.lang.Long.TYPE | ClassOfLong => rs.getLong(idx)
                case java.lang.Float.TYPE | ClassOfFloat => rs.getFloat(idx)
                case java.lang.Double.TYPE | ClassOfDouble => rs.getDouble(idx)
                case ClassOfBigDecimal => rs.getBigDecimal(idx)
                case ClassOfScalaBigDecimal => scala.math.BigDecimal(rs.getBigDecimal(idx))
                case ClassOfSQLDate => rs.getDate(idx)
                case ClassOfSQLTime => rs.getTime(idx)
                case ClassOfSQLTimestamp | ClassOfUtilDate => rs.getTimestamp(idx)
                case ClassOfString => rs.getString(idx)
                case ClassOfByteArray => rs.getBytes(idx)
              }
              fieldMapping.asInstanceOf[beanMapping.FieldMapping[Any]].set(bean, value)
            case None =>
          }
        }
    }

    bean
  }

  def withStatement[T](f: Statement => T): T = {
    val stmt = conn.createStatement
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
      case (v: JdbcValue, idx) => stmt.setObject(idx+1, v.getJdbcValue)
      case (v, idx) => stmt.setObject(idx + 1, v)
    }

  def executeUpdateWithGenerateKey(stmt: SQLWithArgs)(processGenerateKeys: ResultSet => Unit = null): Int = {
    val prepared = conn.prepareStatement(stmt.sql,
      if (processGenerateKeys != null) Statement.RETURN_GENERATED_KEYS
      else Statement.NO_GENERATED_KEYS)

    if (stmt.args != null) setStatementArgs(prepared, stmt.args)

    val result = prepared.executeUpdate()

    if (processGenerateKeys != null) {
      val keys = prepared.getGeneratedKeys
      processGenerateKeys(keys)
    }

    result
  }

  def eachRow[T : ClassTag](sql: SQLWithArgs)(f: T => Unit) {
    val prepared = conn.prepareStatement(sql.sql)
    if (sql.args != null) setStatementArgs(prepared, sql.args)

    val rs = prepared.executeQuery()
    val rsMeta = rs.getMetaData
    while (rs.next()) {
      val mapped = rs2mapped(rsMeta, rs, implicitly[ClassTag[T]])
      f(mapped)
    }
  }

  def rows[T : ClassTag](sql: SQLWithArgs): List[T] = {
    val buffer = new ListBuffer[T]()
    val prepared = conn.prepareStatement(sql.sql)
    if (sql.args != null) setStatementArgs(prepared, sql.args)

    val rs = prepared.executeQuery()
    val rsMeta = rs.getMetaData
    while (rs.next()) {
      val mapped = rs2mapped(rsMeta, rs, implicitly[ClassTag[T]])
      buffer += mapped

    }
    buffer.toList
  }

  def queryInt(sql: SQLWithArgs): Int = {
    val prepared = conn.prepareStatement(sql.sql)
    if(sql.args != null) setStatementArgs(prepared, sql.args)

    val rs = prepared.executeQuery()

    if(rs.next) {
      rs.getInt(1)
    } else throw new IllegalArgumentException("query return no rows")
  }

  def insert[T](bean: T) {
    val beanMapping = BeanMapping.getBeanMapping(bean.getClass).asInstanceOf[BeanMapping[T]]
    val idColumns = beanMapping.idFields
    val hasId = idColumns.forall { fieldMapping =>
      val value = fieldMapping.get(bean)
      
      (value != null) && (fieldMapping.fieldType match {
        case java.lang.Integer.TYPE | ClassOfInteger | java.lang.Short.TYPE | ClassOfShort |
          java.lang.Long.TYPE | ClassOfLong  =>
          value.asInstanceOf[Number].longValue != 0
        case ClassOfBigDecimal =>
          value.asInstanceOf[java.math.BigDecimal].longValue != 0
        case ClassOfScalaBigDecimal =>
          value.asInstanceOf[scala.math.BigDecimal].longValue != 0
        case _ => true
      })
    }

    if (!hasId) { // try auto generate
      val fields = beanMapping.fields.filterNot(_.isId)
      val sql = "insert into " +
        (if (beanMapping.catelog != null && beanMapping.catelog != "") beanMapping.catelog + "." else "") +
        beanMapping.tableName + "(" +
        fields.map(_.columnName).mkString(",") + ") values (" +
        fields.map(_ => '?').mkString(",") + ")"

      val args = fields.map(_.get(bean)).toSeq
      executeUpdateWithGenerateKey(SQLWithArgs(sql, args)) { rs =>
        if (rs.next) {
          idColumns.foreach { col =>
            val value = col.fieldType match {
              case java.lang.Boolean.TYPE | ClassOfBoolean => rs.getBoolean(1)  // TODO number types support
              case _ => throw new AssertionError
            }
            col.asInstanceOf[beanMapping.FieldMapping[Any]].set(bean, value)
          }
        }
      }

    } else {

      val fields = beanMapping.fields
      val sql = "insert into " +
        (if (beanMapping.catelog != null && beanMapping.catelog != "") beanMapping.catelog + "." else "") +
        beanMapping.tableName + "(" +
        fields.map(_.columnName).mkString(",") + ") values (" +
        fields.map(_ => '?').mkString(",") + ")"

      val args = fields.map(_.get(bean)).toSeq
      executeUpdate(SQLWithArgs(sql, args))
    }
  }

  // id required
  def update(bean: AnyRef) {
	  // TODO
  }

  // id required
  def delete(bean: AnyRef) {
	  // TODO
  }

}