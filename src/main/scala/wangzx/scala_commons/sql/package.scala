package wangzx.scala_commons

import javax.sql.DataSource
import java.sql.{Connection, PreparedStatement, ResultSet, Timestamp}
import java.util.Date

import scala.language.implicitConversions
import scala.language.experimental.macros

package object sql {

  implicit def enhanceConnection(conn: Connection) = new RichConnection(conn)

  implicit def enhanceDataSource(datasource: DataSource)  = new RichDataSource(datasource)

  //implicit def enhanceStringContext(sc: StringContext) = new SQLStringContext(sc)

  implicit def enhancePlainSql(stmt: String) = SQLWithArgs(stmt, Seq.empty)

  trait JdbcValueAccessor[T] {
    def passIn(stmt: PreparedStatement, index: Int, value: T)
    def passOut(rs: ResultSet, index: Int): T
    def passOut(rs: ResultSet, name: String): T
  }

  object JdbcValueAccessor {
    def apply[T](implicit v: JdbcValueAccessor[T]): JdbcValueAccessor[T] = v

    implicit def materialOption[T : JdbcValueAccessor]: JdbcValueAccessor[Option[T]] = new JdbcValueAccessor_Option[T]
  }

  trait ResultSetMapper[T] {
    def from(rs: ResultSet): T
  }

  object ResultSetMapper {
    implicit def meterial[T]: ResultSetMapper[T] = macro Macros.generateCaseClassResultSetMapper[T]
  }

  abstract class CaseClassResultSetMapper[T] extends ResultSetMapper[T] {

    case class Field[T: JdbcValueAccessor](name: String, default: Option[T] = None) {
      // val method: Option[java.lang.reflect.Method] = defaultName.map ( companion.getClass.getMethod(_) )

      def apply(rs: ResultSetEx): T = {
        if ( rs hasColumn name ){
          rs.get[T](name)
        }
        else {
          default match {
            case Some(m) => m
            case None => throw new RuntimeException(s"The ResultSet have no field $name but it is required")
          }
        }
      }
    }
  }

  sealed case class JdbcValue[T: JdbcValueAccessor](value: T) {
    def accessor: JdbcValueAccessor[T] = implicitly[JdbcValueAccessor[T]]
    def passIn(stmt: PreparedStatement, index: Int) = accessor.passIn(stmt, index, value)
  }
  object JdbcValue {
    implicit def wrap[T: JdbcValueAccessor](t: T): JdbcValue[T] = JdbcValue(t)
    implicit def wrap[T: JdbcValueAccessor](t: Option[T]): JdbcValue[Option[T]] = JdbcValue(t)(new JdbcValueAccessor_Option[T])
  }

  // native JdbcValueAccessors
  implicit object JdbcValueAccessor_Boolean extends JdbcValueAccessor[Boolean] {
    override def passIn(stmt: PreparedStatement, index: Int, value: Boolean): Unit = stmt.setBoolean(index, value)
    override def passOut(rs: ResultSet, index: Int): Boolean = rs.getBoolean(index)
    override def passOut(rs: ResultSet, name: String): Boolean = rs.getBoolean(name)
  }

  implicit object JdbcValueAccessor_Byte extends JdbcValueAccessor[Byte] {
    override def passIn(stmt: PreparedStatement, index: Int, value: Byte): Unit = stmt.setByte(index, value)
    override def passOut(rs: ResultSet, index: Int): Byte = rs.getByte(index)
    override def passOut(rs: ResultSet, name: String): Byte = rs.getByte(name)
  }

  implicit object JdbcValueAccessor_Short extends JdbcValueAccessor[Short] {
    override def passIn(stmt: PreparedStatement, index: Int, value: Short): Unit = stmt.setShort(index, value)
    override def passOut(rs: ResultSet, index: Int): Short = rs.getShort(index)
    override def passOut(rs: ResultSet, name: String): Short = rs.getShort(name)
  }

  implicit object JdbcValueAccessor_Int extends JdbcValueAccessor[Int] {
    override def passIn(stmt: PreparedStatement, index: Int, value: Int): Unit = stmt.setInt(index, value)
    override def passOut(rs: ResultSet, index: Int): Int = rs.getInt(index)
    override def passOut(rs: ResultSet, name: String): Int = rs.getInt(name)
  }

  implicit object JdbcValueAccessor_Long extends JdbcValueAccessor[Long] {
    override def passIn(stmt: PreparedStatement, index: Int, value: Long): Unit = stmt.setLong(index, value)
    override def passOut(rs: ResultSet, index: Int): Long = rs.getLong(index)
    override def passOut(rs: ResultSet, name: String): Long = rs.getLong(name)
  }

  implicit object JdbcValueAccessor_Float extends JdbcValueAccessor[Float] {
    override def passIn(stmt: PreparedStatement, index: Int, value: Float): Unit = stmt.setFloat(index, value)
    override def passOut(rs: ResultSet, index: Int): Float = rs.getFloat(index)
    override def passOut(rs: ResultSet, name: String): Float = rs.getFloat(name)
  }

  implicit object JdbcValueAccessor_Double extends JdbcValueAccessor[Double] {
    override def passIn(stmt: PreparedStatement, index: Int, value: Double): Unit = stmt.setDouble(index, value)
    override def passOut(rs: ResultSet, index: Int): Double = rs.getDouble(index)
    override def passOut(rs: ResultSet, name: String): Double = rs.getDouble(name)
  }

  implicit object JdbcValueAccessor_String extends JdbcValueAccessor[String] {
    override def passIn(stmt: PreparedStatement, index: Int, value: String): Unit = stmt.setString(index, value)
    override def passOut(rs: ResultSet, index: Int): String = rs.getString(index)
    override def passOut(rs: ResultSet, name: String): String = rs.getString(name)
  }

  implicit object JdbcValueAccessor_BigDecimal extends JdbcValueAccessor[java.math.BigDecimal] {
    override def passIn(stmt: PreparedStatement, index: Int, value: java.math.BigDecimal): Unit = stmt.setBigDecimal(index, value)
    override def passOut(rs: ResultSet, index: Int): java.math.BigDecimal = rs.getBigDecimal(index)
    override def passOut(rs: ResultSet, name: String): java.math.BigDecimal = rs.getBigDecimal(name)
  }

  // TODO Date Time Timestamp

  implicit object JdbcValueAccessor_Date extends JdbcValueAccessor[java.util.Date] {
    override def passIn(stmt: PreparedStatement, index: Int, value: Date): Unit = stmt.setTimestamp(index, new Timestamp(value.getTime))

    override def passOut(rs: ResultSet, index: Int): Date = rs.getTimestamp(index) match {
      case x: Timestamp => new Date(x.getTime)
      case null => null
    }

    override def passOut(rs: ResultSet, name: String): Date = rs.getTimestamp(name) match {
      case x: Timestamp => new Date(x.getTime)
      case null => null
    }
  }

  implicit object JdbcValueAccessor_Date2 extends JdbcValueAccessor[java.sql.Date] {
    override def passIn(stmt: PreparedStatement, index: Int, value: java.sql.Date): Unit = stmt.setDate(index, value)

    override def passOut(rs: ResultSet, index: Int): java.sql.Date = rs.getDate(index)

    override def passOut(rs: ResultSet, name: String): java.sql.Date = rs.getDate(name)
  }

  implicit object JdbcValueAccessor_Timestamp extends JdbcValueAccessor[Timestamp] {
    override def passIn(stmt: PreparedStatement, index: Int, value: Timestamp): Unit = stmt.setTimestamp(index, value)

    override def passOut(rs: ResultSet, index: Int): Timestamp = rs.getTimestamp(index)

    override def passOut(rs: ResultSet, name: String): Timestamp = rs.getTimestamp(name)
  }

  // extensions
  implicit object JdbcValueAccessor_ScalaBigDecimal extends JdbcValueAccessor[BigDecimal] {
    override def passIn(stmt: PreparedStatement, index: Int, value: BigDecimal): Unit = stmt.setBigDecimal(index, value.bigDecimal)
    override def passOut(rs: ResultSet, index: Int): BigDecimal = BigDecimal(rs.getBigDecimal(index))
    override def passOut(rs: ResultSet, name: String): BigDecimal = BigDecimal(rs.getBigDecimal(name))
  }

  class JdbcValueAccessor_Option[T: JdbcValueAccessor] extends JdbcValueAccessor[Option[T]] {
    override def passIn(stmt: PreparedStatement, index: Int, value: Option[T]): Unit = value match {
      case Some(t) => implicitly[JdbcValueAccessor[T]].passIn(stmt, index, t)
      case None => stmt.setObject(index, null) // TODO or setNull
    }

    override def passOut(rs: ResultSet, index: Int): Option[T] = implicitly[JdbcValueAccessor[T]].passOut(rs, index) match {
      case null => None
      case x => Some(x)
    }

    override def passOut(rs: ResultSet, name: String): Option[T] = implicitly[JdbcValueAccessor[T]].passOut(rs, name) match {
      case null => None
      case x => Some(x)
    }
  }

  //  implicit def aaa[T: Manifest] =

  implicit def intEnum2JdbcValue[T](enum: IntEnum[T]): JdbcValue[IntEnum[T]] = {
    val accessor = new JdbcValueAccessor[IntEnum[T]] {
      override def passIn(stmt: PreparedStatement, index: Int, value: IntEnum[T]): Unit =
        stmt.setInt(index, value.code())
      override def passOut(rs: ResultSet, index: Int): IntEnum[T] = ???
      override def passOut(rs: ResultSet, name: String): IntEnum[T] = ???
    }
    JdbcValue(enum)(accessor)
  }

  implicit class ResultSetEx(rs: ResultSet) {
    lazy val meta = rs.getMetaData
    lazy val columns: Set[String] = (for(i <- 1 to meta.getColumnCount) yield meta.getColumnLabel(i)).toSet
    lazy val columnsUpperCase: Set[String] = columns.map(_.toUpperCase)

    def hasColumn(column: String) = columnsUpperCase.contains(column.toUpperCase)

    def get[T: JdbcValueAccessor](index: Int): T = implicitly[JdbcValueAccessor[T]].passOut(rs, index)
    def get[T: JdbcValueAccessor](label: String): T = implicitly[JdbcValueAccessor[T]].passOut(rs, label)

    def getOption[T: JdbcValueAccessor](index: Int): Option[T] =
      if(rs.getObject(index)==null) None else Some(implicitly[JdbcValueAccessor[T]].passOut(rs, index))
  }

  implicit class SQLStringContext(sc: StringContext) {
    def sql(args: JdbcValue[_]*) = SQLWithArgs(sc.parts.mkString("?"), args)

    // SQL"" will validate the sql statement at compiler time
    def SQL(args: JdbcValue[_]*): SQLWithArgs = macro  Macros.parseSQL

  }

  case class SQLWithArgs(sql: String, args: Seq[JdbcValue[_]]) {

    def +(other: SQLWithArgs): SQLWithArgs =
      SQLWithArgs(sql + other.sql, args ++ other.args)

    def +(other: String): SQLWithArgs = SQLWithArgs(sql + other, args)

  }


  /**
   * instead of using reflect mechanism, a bean maybe read field from ResultSet itself
   */
  trait ResultSetConvertable {
    def fromResultSet(rs: ResultSet)
  }

}