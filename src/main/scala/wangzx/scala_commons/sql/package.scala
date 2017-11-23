package wangzx.scala_commons

import javax.sql.DataSource
import java.sql.{Blob, Clob, Connection, PreparedStatement, ResultSet, Timestamp}
import java.util.Date

import scala.language.implicitConversions
import scala.language.experimental.macros

package sql {

  /**
    * wrap a sql"select * from table where id = $id" object
    */
  case class SQLWithArgs(sql: String, args: Seq[JdbcValue[_]]) {

    def +(other: SQLWithArgs): SQLWithArgs =
      SQLWithArgs(sql + other.sql, args ++ other.args)

    def +(other: String): SQLWithArgs = SQLWithArgs(sql + other, args)

  }

  /**
    * for values(of type T) to passed into Statement or passed out from ResultSet, it should has a contxt bound of
    * JdbcValueAccessor[T]
    *
    * <ul> package wangzx.scala_commons.sql prdefined a lot of pre-defined implementation for the jdbc value types:
    * <li> boolean, byte, short, Int, Long, Float, Double, BigDecimal, scala.BigDecimal
    * <li> String
    * <li> Date, Time, Timestamp
    * <li> Blob, Clob, byte[]
    * <li> Option[T] if T has JdbcValueAccessor context bounded
    * </ul>
    *
    * <ul>
    * developer can define your's value type such as a MyDate which stored as database `Date`, you need only define
    * an implicit value of JdbcValueAccessor[MyDate], then you can:
    * <li> pass in statement using sql"... where date_field = $myDate"
    * <li> passout from ResultSet, using rs.get[MyDate](field index or name)
    * <li> mapping to a field of other CaseClass such as User, and then using rows[User](sql)
    * <li> mapping to a Row object and rows[Row](sql), and then using row.get[MyDate](field index of name).
    * </ul>
    */
  trait JdbcValueAccessor[T] {
    def passIn(stmt: PreparedStatement, index: Int, value: T)
    def passOut(rs: ResultSet, index: Int): T
    def passOut(rs: ResultSet, name: String): T
  }

  object JdbcValueAccessor {
    def apply[T](implicit v: JdbcValueAccessor[T]): JdbcValueAccessor[T] = v

    implicit def materialOption[T : JdbcValueAccessor]: JdbcValueAccessor[Option[T]] = new JdbcValueAccessor_Option[T]
  }

  /**
    * any record level(a table row) having a ResultSetMapper context bound can used in `rows[T](sql)`
    *
    * the scala-sql library provide a Macro to automate generate the implementation for a given case class T
    * if all it's field is JdbcValueAccess-able(having a JdbcValueAccess context bound).
    *
    * since the macro will generate a ResultSetMapper class for you anytime if there is not an explicit imported implicit value,
    * maybe a lot of anonymous class will be generated. that is no problem but a bigger jar. to avoid this problem, you can
    * define a implicit ResultSetMappper value in the Case Class's companion object.
    *
    * <pre>
    *   case class User(name: String, age: Int)
    *   object User {
    *     implicit val resultSetmapper = ResultSetMapper.material[User]
    *   }
    * </pre>
    */
  trait ResultSetMapper[T] {
    def from(rs: ResultSet): T
  }

  object ResultSetMapper {
    implicit def material[T]: ResultSetMapper[T] = macro Macros.generateCaseClassResultSetMapper[T]
  }

}

package object sql {

 implicit class SQLStringContext(sc: StringContext) {
    def sql(args: JdbcValue[_]*) = SQLWithArgs(sc.parts.mkString("?"), args)

    /**
     * SQL"" will validate the sql statement at compiler time.
     */
    def SQL(args: JdbcValue[_]*): SQLWithArgs = macro  Macros.parseSQL
  }

  //

  implicit def enhanceConnection(conn: Connection) = new RichConnection(conn)

  implicit def enhanceDataSource(datasource: DataSource)  = new RichDataSource(datasource)

  implicit def enhancePlainSql(stmt: String) = SQLWithArgs(stmt, Seq.empty)



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
  implicit object ResultSetMapper_Boolean extends ResultSetMapper[Boolean] {
    override def from(rs: ResultSet): Boolean = rs.getBoolean(1)
  }

  implicit object JdbcValueAccessor_Byte extends JdbcValueAccessor[Byte] {
    override def passIn(stmt: PreparedStatement, index: Int, value: Byte): Unit = stmt.setByte(index, value)
    override def passOut(rs: ResultSet, index: Int): Byte = rs.getByte(index)
    override def passOut(rs: ResultSet, name: String): Byte = rs.getByte(name)
  }
  implicit object ResultSetMapper_Byte extends ResultSetMapper[Byte] {
    override def from(rs: ResultSet): Byte = rs.getByte(1)
  }

  implicit object JdbcValueAccessor_Short extends JdbcValueAccessor[Short] {
    override def passIn(stmt: PreparedStatement, index: Int, value: Short): Unit = stmt.setShort(index, value)
    override def passOut(rs: ResultSet, index: Int): Short = rs.getShort(index)
    override def passOut(rs: ResultSet, name: String): Short = rs.getShort(name)
  }
  implicit object ResulSetMapper_Short extends ResultSetMapper[Short] {
    override def from(rs: ResultSet): Short = rs.getShort(1)
  }

  implicit object JdbcValueAccessor_Int extends JdbcValueAccessor[Int] {
    override def passIn(stmt: PreparedStatement, index: Int, value: Int): Unit = stmt.setInt(index, value)
    override def passOut(rs: ResultSet, index: Int): Int = rs.getInt(index)
    override def passOut(rs: ResultSet, name: String): Int = rs.getInt(name)
  }
  implicit object ResultSetMapper_Int extends ResultSetMapper[Int] {
    override def from(rs: ResultSet): Int = rs.getInt(1)
  }

  implicit object JdbcValueAccessor_Long extends JdbcValueAccessor[Long] {
    override def passIn(stmt: PreparedStatement, index: Int, value: Long): Unit = stmt.setLong(index, value)
    override def passOut(rs: ResultSet, index: Int): Long = rs.getLong(index)
    override def passOut(rs: ResultSet, name: String): Long = rs.getLong(name)
  }
  implicit object ResultSetMapper_Long extends ResultSetMapper[Long] {
    override def from(rs: ResultSet): Long = rs.getLong(1)
  }

  implicit object JdbcValueAccessor_Float extends JdbcValueAccessor[Float] {
    override def passIn(stmt: PreparedStatement, index: Int, value: Float): Unit = stmt.setFloat(index, value)
    override def passOut(rs: ResultSet, index: Int): Float = rs.getFloat(index)
    override def passOut(rs: ResultSet, name: String): Float = rs.getFloat(name)
  }
  implicit object ResultSetMapper_Float extends ResultSetMapper[Float] {
    override def from(rs: ResultSet): Float = rs.getFloat(1)
  }

  implicit object JdbcValueAccessor_Double extends JdbcValueAccessor[Double] {
    override def passIn(stmt: PreparedStatement, index: Int, value: Double): Unit = stmt.setDouble(index, value)
    override def passOut(rs: ResultSet, index: Int): Double = rs.getDouble(index)
    override def passOut(rs: ResultSet, name: String): Double = rs.getDouble(name)
  }
  implicit object ResulSetMapper_Double extends ResultSetMapper[Double] {
    override def from(rs: ResultSet): Double = rs.getDouble(1)
  }

  implicit object JdbcValueAccessor_String extends JdbcValueAccessor[String] {
    override def passIn(stmt: PreparedStatement, index: Int, value: String): Unit = stmt.setString(index, value)
    override def passOut(rs: ResultSet, index: Int): String = rs.getString(index)
    override def passOut(rs: ResultSet, name: String): String = rs.getString(name)
  }
  implicit object ResultSetMapper_String extends ResultSetMapper[String] {
    override def from(rs: ResultSet): String = rs.getString(1)
  }

  implicit object JdbcValueAccessor_BigDecimal extends JdbcValueAccessor[java.math.BigDecimal] {
    override def passIn(stmt: PreparedStatement, index: Int, value: java.math.BigDecimal): Unit = stmt.setBigDecimal(index, value)
    override def passOut(rs: ResultSet, index: Int): java.math.BigDecimal = rs.getBigDecimal(index)
    override def passOut(rs: ResultSet, name: String): java.math.BigDecimal = rs.getBigDecimal(name)
  }
  implicit object ResultSetMapper_BigDecimal extends ResultSetMapper[java.math.BigDecimal] {
    override def from(rs: ResultSet): java.math.BigDecimal = rs.getBigDecimal(1)
  }

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
  implicit object ResultSetMapper_Date extends ResultSetMapper[java.util.Date] {
    override def from(rs: ResultSet): Date = new java.util.Date(rs.getTimestamp(1).getTime)
  }

  implicit object JdbcValueAccessor_Date2 extends JdbcValueAccessor[java.sql.Date] {
    override def passIn(stmt: PreparedStatement, index: Int, value: java.sql.Date): Unit = stmt.setDate(index, value)

    override def passOut(rs: ResultSet, index: Int): java.sql.Date = rs.getDate(index)

    override def passOut(rs: ResultSet, name: String): java.sql.Date = rs.getDate(name)
  }
  implicit object ResultSetMapper_Date2 extends ResultSetMapper[java.sql.Date] {
    override def from(rs: ResultSet): java.sql.Date = rs.getDate(1)
  }

  implicit object JdbcValueAccessor_Timestamp extends JdbcValueAccessor[Timestamp] {
    override def passIn(stmt: PreparedStatement, index: Int, value: Timestamp): Unit = stmt.setTimestamp(index, value)

    override def passOut(rs: ResultSet, index: Int): Timestamp = rs.getTimestamp(index)

    override def passOut(rs: ResultSet, name: String): Timestamp = rs.getTimestamp(name)
  }

  implicit object ResultSetMapper_Timestamp extends ResultSetMapper[Timestamp] {
    override def from(rs: ResultSet): Timestamp = rs.getTimestamp(1)
  }

  // extensions
  implicit object JdbcValueAccessor_ScalaBigDecimal extends JdbcValueAccessor[BigDecimal] {
    override def passIn(stmt: PreparedStatement, index: Int, value: BigDecimal): Unit = stmt.setBigDecimal(index, value.bigDecimal)
    override def passOut(rs: ResultSet, index: Int): BigDecimal = {
      val it = rs.getBigDecimal(index); if(it != null) BigDecimal(it) else null
    }
    override def passOut(rs: ResultSet, name: String): BigDecimal = {
      val it = rs.getBigDecimal(name); if(it != null) BigDecimal(it) else null
    }
  }

  implicit object ResultSetMapper_ScalaBigDecimal extends ResultSetMapper[BigDecimal] {
    override def from(rs: ResultSet): BigDecimal = {
      val it = rs.getBigDecimal(1); if(it != null) BigDecimal(it) else null
    }
  }

  class JdbcValueAccessor_Option[T: JdbcValueAccessor] extends JdbcValueAccessor[Option[T]] {
    override def passIn(stmt: PreparedStatement, index: Int, value: Option[T]): Unit = value match {
      case Some(t) => implicitly[JdbcValueAccessor[T]].passIn(stmt, index, t)
      case None => stmt.setObject(index, null) // TODO or setNull
    }

    override def passOut(rs: ResultSet, index: Int): Option[T] = {
      if( rs.getObject(index) == null) None
      else {
        Some( implicitly[JdbcValueAccessor[T]].passOut(rs, index) )
      }
    }

    override def passOut(rs: ResultSet, name: String): Option[T] = {
      if( rs.getObject(name) == null) None
      else {
        Some( implicitly[JdbcValueAccessor[T]].passOut(rs, name) )
      }
    }
  }

  implicit object JdbcValueAccessor_ArrayBytes extends JdbcValueAccessor[Array[Byte]] {
    override def passIn(stmt: PreparedStatement, index: Int, value: Array[Byte]): Unit = stmt.setBytes(index, value)

    override def passOut(rs: ResultSet, index: Int): Array[Byte] = rs.getBytes(index)

    override def passOut(rs: ResultSet, name: String): Array[Byte] = rs.getBytes(name)
  }

  implicit object JdbcValueAccessor_Blob extends  JdbcValueAccessor[Blob] {
    override def passIn(stmt: PreparedStatement, index: Int, value: Blob): Unit = stmt.setBlob(index, value)

    override def passOut(rs: ResultSet, index: Int): Blob = rs.getBlob(index)

    override def passOut(rs: ResultSet, name: String): Blob = rs.getBlob(name)
  }

  implicit object JdbcValueAccessor_Clob extends JdbcValueAccessor[Clob] {
    override def passIn(stmt: PreparedStatement, index: Int, value: Clob): Unit = stmt.setClob(index, value)

    override def passOut(rs: ResultSet, index: Int): Clob = rs.getClob(index)

    override def passOut(rs: ResultSet, name: String): Clob = rs.getClob(name)
  }

  implicit class ResultSetEx(rs: ResultSet) {
    lazy val meta = rs.getMetaData
    lazy val columns: Set[String] = (for(i <- 1 to meta.getColumnCount) yield meta.getColumnLabel(i)).toSet
    lazy val columnsUpperCase: Set[String] = columns.map(_.toUpperCase)

    // TODO if database is case-sensitive, maybe need check
    def hasColumn(column: String) = columnsUpperCase.contains(column.toUpperCase)

    def get[T: JdbcValueAccessor](index: Int): T = implicitly[JdbcValueAccessor[T]].passOut(rs, index)
    def get[T: JdbcValueAccessor](label: String): T = implicitly[JdbcValueAccessor[T]].passOut(rs, label)

    def getOption[T: JdbcValueAccessor](index: Int): Option[T] =
      if(rs.getObject(index)==null) None else Some(implicitly[JdbcValueAccessor[T]].passOut(rs, index))
  }

  /**
    * the base class used in automate generated ResultSetMapper.
    */
  abstract class CaseClassResultSetMapper[T] extends ResultSetMapper[T] {

    // cammel case mapping support such as userId -> user_id, postURL -> post_url
    case class Field[T: JdbcValueAccessor](name: String, default: Option[T] = None) {
      // val method: Option[java.lang.reflect.Method] = defaultName.map ( companion.getClass.getMethod(_) )
      val underscoreName: Option[String] = {
        val sb = new StringBuilder
        var i = 0
        var lastChar: Char = 0
        while(i < name.length) {
          val ch = name.charAt(i)
          if(i == 0) sb.append(ch)
          else {
            if(Character.isLowerCase(lastChar) && Character.isUpperCase(ch)) {
              sb.append('_')
              sb.append(ch)
            }
            else sb.append(ch)
          }
          lastChar = ch
          i += 1
        }
        val newName = sb.toString
        if(newName != name) Some(newName)
        else None
      }

      def apply(rs: ResultSetEx): T = {
        if ( rs hasColumn name ){
          rs.get[T](name)
        }
        else if(underscoreName.nonEmpty && rs.hasColumn(underscoreName.get)) {
          rs.get[T](underscoreName.get)
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

}