package wsql

import ResultSetMapperMacro.resultSetMapperImpl

import javax.sql.DataSource
import java.sql.{Blob, Clob, Connection, PreparedStatement, ResultSet, Statement, Timestamp}
import java.util.Date
import scala.annotation.StaticAnnotation

/**
  * wrap a sql"select * from table where id = $id" object
  */
case class SQLWithArgs(sql: String, args: Seq[JdbcValue[?]]):

  def +(other: SQLWithArgs): SQLWithArgs =
    SQLWithArgs(sql + other.sql, args ++ other.args)

  def +(other: String): SQLWithArgs = SQLWithArgs(sql + other, args)


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
trait JdbcValueAccessor[T]:
  def passIn(stmt: PreparedStatement, index: Int, value: T): Unit
  def passOut(rs: ResultSet, index: Int): T
  def passOut(rs: ResultSet, name: String): T

object JdbcValueAccessor:
  def apply[T](using v: JdbcValueAccessor[T]): JdbcValueAccessor[T] = v

  given [T: JdbcValueAccessor]: JdbcValueAccessor[Option[T]] with //new JdbcValueAccessor_Option[T]
    inline def passIn(stmt: PreparedStatement, index: Int, value: Option[T]): Unit = value match
      case Some(t) => summon[JdbcValueAccessor[T]].passIn(stmt, index, t)
      case None => stmt.setObject(index, null) // TODO or setNull

    inline def passOut(rs: ResultSet, index: Int): Option[T] =
      if (rs.getObject(index) == null) None
      else
        Some(summon[JdbcValueAccessor[T]].passOut(rs, index))

    inline def passOut(rs: ResultSet, name: String): Option[T] =
      if (rs.getObject(name) == null) None
      else
        Some(summon[JdbcValueAccessor[T]].passOut(rs, name))

  given JdbcValueAccessor[Boolean] with
    inline def passIn(stmt: PreparedStatement, index: Int, value: Boolean): Unit = stmt.setBoolean(index, value)
    inline def passOut(rs: ResultSet, index: Int): Boolean = rs.getBoolean(index)
    inline def passOut(rs: ResultSet, name: String): Boolean = rs.getBoolean(name)

  given JdbcValueAccessor[Byte] with
    inline def passIn(stmt: PreparedStatement, index: Int, value: Byte): Unit = stmt.setByte(index, value)
    inline def passOut(rs: ResultSet, index: Int): Byte = rs.getByte(index)
    inline def passOut(rs: ResultSet, name: String): Byte = rs.getByte(name)

  given JdbcValueAccessor[Short] with
    inline def passIn(stmt: PreparedStatement, index: Int, value: Short): Unit = stmt.setShort(index, value)
    inline def passOut(rs: ResultSet, index: Int): Short = rs.getShort(index)
    inline def passOut(rs: ResultSet, name: String): Short = rs.getShort(name)

  given JdbcValueAccessor[Int] with
    inline def passIn(stmt: PreparedStatement, index: Int, value: Int): Unit = stmt.setInt(index, value)
    inline def passOut(rs: ResultSet, index: Int): Int = rs.getInt(index)
    inline def passOut(rs: ResultSet, name: String): Int = rs.getInt(name)

  given JdbcValueAccessor[Long] with
    inline def passIn(stmt: PreparedStatement, index: Int, value: Long): Unit = stmt.setLong(index, value)
    inline def passOut(rs: ResultSet, index: Int): Long = rs.getLong(index)
    inline def passOut(rs: ResultSet, name: String): Long = rs.getLong(name)

  given JdbcValueAccessor[Float] with
    inline def passIn(stmt: PreparedStatement, index: Int, value: Float): Unit = stmt.setFloat(index, value)
    inline def passOut(rs: ResultSet, index: Int): Float = rs.getFloat(index)
    inline def passOut(rs: ResultSet, name: String): Float = rs.getFloat(name)

  given JdbcValueAccessor[Double] with
    inline def passIn(stmt: PreparedStatement, index: Int, value: Double): Unit = stmt.setDouble(index, value)
    inline def passOut(rs: ResultSet, index: Int): Double = rs.getDouble(index)
    inline def passOut(rs: ResultSet, name: String): Double = rs.getDouble(name)

  given JdbcValueAccessor[String] with
    inline def passIn(stmt: PreparedStatement, index: Int, value: String): Unit = stmt.setString(index, value)
    inline def passOut(rs: ResultSet, index: Int): String = rs.getString(index)
    inline def passOut(rs: ResultSet, name: String): String = rs.getString(name)

  given JdbcValueAccessor[java.math.BigDecimal] with
    inline def passIn(stmt: PreparedStatement, index: Int, value: java.math.BigDecimal): Unit = stmt.setBigDecimal(index, value)
    inline def passOut(rs: ResultSet, index: Int): java.math.BigDecimal = rs.getBigDecimal(index)
    inline def passOut(rs: ResultSet, name: String): java.math.BigDecimal = rs.getBigDecimal(name)

  given JdbcValueAccessor[java.util.Date] with
    inline def passIn(stmt: PreparedStatement, index: Int, value: Date): Unit = stmt.setTimestamp(index, new Timestamp(value.getTime))
    inline def passOut(rs: ResultSet, index: Int): Date = rs.getTimestamp(index) match
      case x: Timestamp => new Date(x.getTime)
      case null => null

    inline def passOut(rs: ResultSet, name: String): Date = rs.getTimestamp(name) match
      case x: Timestamp => new Date(x.getTime)
      case null => null

  given _jva_date: JdbcValueAccessor[java.sql.Date] with
    inline def passIn(stmt: PreparedStatement, index: Int, value: java.sql.Date): Unit = stmt.setDate(index, value)
    inline def passOut(rs: ResultSet, index: Int): java.sql.Date = rs.getDate(index)
    inline def passOut(rs: ResultSet, name: String): java.sql.Date = rs.getDate(name)

  given JdbcValueAccessor[Timestamp] with
    inline def passIn(stmt: PreparedStatement, index: Int, value: Timestamp): Unit = stmt.setTimestamp(index, value)
    inline def passOut(rs: ResultSet, index: Int): Timestamp = rs.getTimestamp(index)
    inline def passOut(rs: ResultSet, name: String): Timestamp = rs.getTimestamp(name)

  given _jva_bd: JdbcValueAccessor[BigDecimal] with
    inline def passIn(stmt: PreparedStatement, index: Int, value: BigDecimal): Unit = stmt.setBigDecimal(index, value.bigDecimal)
    inline def passOut(rs: ResultSet, index: Int): BigDecimal =
      val it = rs.getBigDecimal(index);
      if (it != null) BigDecimal(it) else null

    inline def passOut(rs: ResultSet, name: String): BigDecimal =
      val it = rs.getBigDecimal(name);
      if (it != null) BigDecimal(it) else null

  given JdbcValueAccessor[Array[Byte]] with
    inline def passIn(stmt: PreparedStatement, index: Int, value: Array[Byte]): Unit = stmt.setBytes(index, value)
    inline def passOut(rs: ResultSet, index: Int): Array[Byte] = rs.getBytes(index)
    inline def passOut(rs: ResultSet, name: String): Array[Byte] = rs.getBytes(name)

  given JdbcValueAccessor[Blob] with
    inline def passIn(stmt: PreparedStatement, index: Int, value: Blob): Unit = stmt.setBlob(index, value)
    inline def passOut(rs: ResultSet, index: Int): Blob = rs.getBlob(index)
    inline def passOut(rs: ResultSet, name: String): Blob = rs.getBlob(name)

  given JdbcValueAccessor[Clob] with
    inline def passIn(stmt: PreparedStatement, index: Int, value: Clob): Unit = stmt.setClob(index, value)
    inline def passOut(rs: ResultSet, index: Int): Clob = rs.getClob(index)
    inline def passOut(rs: ResultSet, name: String): Clob = rs.getClob(name)


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
  * case class User(name: String, age: Int)
  * object User {
  * implicit val resultSetmapper = ResultSetMapper.material[User]
  * }
  * </pre>
  */
trait ResultSetMapper[T]:
  def from(rs: ResultSet): T

object ResultSetMapper:
  inline given material[T: deriving.Mirror.ProductOf]: ResultSetMapper[T] = ${ ResultSetMapperMacro.resultSetMapperImpl[T] }
  inline def derived[T: deriving.Mirror.ProductOf]: ResultSetMapper[T] = ${ ResultSetMapperMacro.resultSetMapperImpl[T] }

  given ResultSetMapper[Boolean] with
    def from(rs: ResultSet): Boolean = rs.getBoolean(1)

  given ResultSetMapper[Byte] with
    def from(rs: ResultSet): Byte = rs.getByte(1)

  given ResultSetMapper[Short] with
    def from(rs: ResultSet): Short = rs.getShort(1)

  given ResultSetMapper[Int] with
    def from(rs: ResultSet): Int = rs.getInt(1)

  given ResultSetMapper[Long] with
    override def from(rs: ResultSet): Long = rs.getLong(1)

  given ResultSetMapper[Float] with
    override def from(rs: ResultSet): Float = rs.getFloat(1)

  given ResultSetMapper[Double] with
    def from(rs: ResultSet): Double = rs.getDouble(1)

  given ResultSetMapper[String] with
    def from(rs: ResultSet): String = rs.getString(1)

  given ResultSetMapper[java.math.BigDecimal] with
    def from(rs: ResultSet): java.math.BigDecimal = rs.getBigDecimal(1)

  given ResultSetMapper[java.util.Date] with
    override def from(rs: ResultSet): Date = new java.util.Date(rs.getTimestamp(1).getTime)

  given _rsm_date: ResultSetMapper[java.sql.Date] with
    def from(rs: ResultSet): java.sql.Date = rs.getDate(1)

  given ResultSetMapper[Timestamp] with
    def from(rs: ResultSet): Timestamp = rs.getTimestamp(1)

  given _rsm_bd: ResultSetMapper[BigDecimal] with
    def from(rs: ResultSet): BigDecimal =
      val it = rs.getBigDecimal(1);
      if (it != null) BigDecimal(it) else null

sealed case class JdbcValue[T: JdbcValueAccessor](value: T):
  def accessor: JdbcValueAccessor[T] = summon[JdbcValueAccessor[T]]
  def passIn(stmt: PreparedStatement, index: Int) = accessor.passIn(stmt, index, value)

object JdbcValue:
  given [T: JdbcValueAccessor]: Conversion[T, JdbcValue[T]] with
    override def apply(t: T) = JdbcValue(t)

  // implicit def convert1[T](t: T) = JdbcValue(t)

  given [T: JdbcValueAccessor]: Conversion[Option[T], JdbcValue[Option[T]]] with
    override def apply(t: Option[T]) = JdbcValue(t)(summon[JdbcValueAccessor[Option[T]]])

extension (sc: StringContext)
  def sql(args: JdbcValue[_]*) = SQLWithArgs(sc.parts.mkString("?"), args)

  /**
    * SQL"" will validate the sql statement at compiler time.
    */
  def SQL(args: JdbcValue[_]*): SQLWithArgs = ??? // macro  Macros.parseSQL

given Conversion[String, SQLWithArgs] with
  override def apply(stmt: String) = SQLWithArgs(stmt, Seq.empty)

extension (rs: ResultSet)

  inline def get[T: JdbcValueAccessor](index: Int): T = summon[JdbcValueAccessor[T]].passOut(rs, index)

  inline def get[T: JdbcValueAccessor](label: String): T = summon[JdbcValueAccessor[T]].passOut(rs, label)

  inline def getOption[T: JdbcValueAccessor](index: Int): Option[T] =
    if (rs.getObject(index) == null) None else Some(summon[JdbcValueAccessor[T]].passOut(rs, index))

/**
 * currently, only classes supprt, dont support object yet.
 */
trait CaseClassColumnMapper:
  def columnName(field: String): String

class Camel2UnderscoreMapper extends CaseClassColumnMapper:
  def columnName(field: String): String =
    val sb = new StringBuilder
    var i = 0
    var lastChar: Char = 0
    while i < field.length do
      val ch = field.charAt(i)
      if (i == 0) sb.append(ch)
      else
        if Character.isLowerCase(lastChar) && Character.isUpperCase(ch) then
          sb.append('_')
          sb.append(ch.toLower)
        else sb.append(ch)
      lastChar = ch
      i += 1
    sb.toString

class IdentityMapping extends CaseClassColumnMapper:
  def columnName(field: String): String = field

class UseColumnMapper(value: Class[_ <: CaseClassColumnMapper]) extends StaticAnnotation

/**
 * inlined into ResultSetMapperMacro to access the field's value with Default
 */
private inline def withDefault[T:JdbcValueAccessor](inline name:String, inline primtive:Boolean, inline deff: Some[T], rs: ResultSet): T =
  try
    val v = rs.get[T](name)
    inline if primtive then
        if rs.wasNull then deff.value else v
    else
        if v == null then deff.value else v
  catch
    case ex: java.sql.SQLException => deff.value

/**
 * inlined into ResultSetMapperMacro to access the field's value without Default
 */
private inline def withoutDefault[T:JdbcValueAccessor](inline name: String, rs: ResultSet): T =
    rs.get[T](name)

private inline def withDefaultOption[T:JdbcValueAccessor](inline name:String, inline primtive:Boolean, inline deff: Some[T], rs: ResultSet): Option[T] =
  try
    val v = rs.get[T](name)
    inline if primtive then
    if rs.wasNull then deff else Some(v)
    else
      if v == null then deff else Some(v)
  catch
    case ex: java.sql.SQLException => deff

/**
 * TODO optimize Option[T]'s macro code
 */
private inline def withoutDefaultOption[T](inline name: String, rs: ResultSet)(inline accessor: JdbcValueAccessor[T]): Option[T] =
  try
    if (rs.getObject(name) == null) None
    else
      Some(accessor.passOut(rs, name))
  catch
    case ex: java.sql.SQLException => None

trait ConnectionOps:
  extension (conn: Connection)
    def withStatement[T](f: Statement => T): T
    def withTransaction[T](f: Connection=>T): T
    inline def createBatch[T](proc: T=>SQLWithArgs): Batch[T]
    inline def createMysqlBatch[T](proc: T=>SQLWithArgs): Batch[T]
    def executeUpdate(stmt: SQLWithArgs): Int
    def executeUpdateWithGenerateKey(stmt: SQLWithArgs)(proc: ResultSet=>Unit = null): Int
    def generateKey[T: JdbcValueAccessor](stmt: SQLWithArgs): T
    def eachRow[T: ResultSetMapper](sql: SQLWithArgs)(f: T=>Unit): Unit
    def rows[T: ResultSetMapper](sql:SQLWithArgs): List[T]
    def joinRows2[T1:ResultSetMapper, T2:ResultSetMapper](sql: SQLWithArgs): List[(T1,T2)]
    def joinRows3[T1:ResultSetMapper, T2: ResultSetMapper, T3: ResultSetMapper](sql: SQLWithArgs): List[(T1,T2,T3)]
    def joinRows4[T1:ResultSetMapper, T2: ResultSetMapper, T3: ResultSetMapper, T4:ResultSetMapper](sql: SQLWithArgs): List[(T1,T2,T3, T4)]
    def row[T: ResultSetMapper](sql:SQLWithArgs): Option[T]
    def joinRow2[T1:ResultSetMapper, T2:ResultSetMapper](sql: SQLWithArgs): Option[(T1,T2)]
    def joinRow3[T1:ResultSetMapper, T2:ResultSetMapper, T3: ResultSetMapper](sql: SQLWithArgs): Option[(T1,T2, T3)]
    def joinRow4[T1:ResultSetMapper, T2:ResultSetMapper, T3: ResultSetMapper, T4:ResultSetMapper](sql: SQLWithArgs): Option[(T1,T2,T3,T4)]
    def queryInt(sql:SQLWithArgs): Int

trait DataSourceOps:
  extension (dataSource: DataSource)
    def withStatement[T](f: Statement => T): T
    def withTransaction[T](f: Connection=>T): T
    inline def createBatch[T](proc: T=>SQLWithArgs): Batch[T]
    inline def createMysqlBatch[T](proc: T=>SQLWithArgs): Batch[T]
    def executeUpdate(stmt: SQLWithArgs): Int
    def executeUpdateWithGenerateKey(stmt: SQLWithArgs)(processGenerateKeys: ResultSet=>Unit = null): Int
    def generateKey[T: JdbcValueAccessor](stmt: SQLWithArgs): T
    def eachRow[T: ResultSetMapper](sql: SQLWithArgs)(f: T=>Unit): Unit
    def rows[T: ResultSetMapper](sql:SQLWithArgs): List[T]
    def joinRows2[T1:ResultSetMapper, T2:ResultSetMapper](sql: SQLWithArgs): List[(T1,T2)]
    def joinRows3[T1:ResultSetMapper, T2: ResultSetMapper, T3: ResultSetMapper](sql: SQLWithArgs): List[(T1,T2,T3)]
    def joinRows4[T1:ResultSetMapper, T2: ResultSetMapper, T3: ResultSetMapper, T4:ResultSetMapper](sql: SQLWithArgs): List[(T1,T2,T3, T4)]
    def row[T: ResultSetMapper](sql:SQLWithArgs): Option[T]
    def joinRow2[T1:ResultSetMapper, T2:ResultSetMapper](sql: SQLWithArgs): Option[(T1,T2)]
    def joinRow3[T1:ResultSetMapper, T2:ResultSetMapper, T3: ResultSetMapper](sql: SQLWithArgs): Option[(T1,T2, T3)]
    def joinRow4[T1:ResultSetMapper, T2:ResultSetMapper, T3: ResultSetMapper, T4:ResultSetMapper](sql: SQLWithArgs): Option[(T1,T2,T3,T4)]
    def queryInt(sql:SQLWithArgs): Int
