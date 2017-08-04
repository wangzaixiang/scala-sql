package wangzx.scala_commons.sql

import java.sql._
import java.lang.{Boolean=>JBoolean, Byte=>JByte, Short=>JShort, Integer=>JInteger, Long=>JLong}
import java.lang.{Float=>JFloat, Double=>JDouble}
import java.math.{BigDecimal => JBigDecimal}
import java.io.InputStream
import java.math
import scala.Array
import scala.reflect.ClassTag

/**
 * provide a offline Box for ResultSet which is scrollable and only valid with an open connection.
 */
object Row {

  sealed abstract class Cell[T](val name: String, val sqltype: Int, val value: T) {
    def ??? = throw new UnsupportedOperationException
    def getString: String = if (value == null) null else value.toString
    def getLong: Long  = ???
    def getInt: Int = getLong.toInt
    def getByte: Byte = getLong.toByte
    def getShort: Short = getLong.toShort
    def getDouble: Double = getLong.toDouble
    def getFloat: Float = getDouble.toFloat
    def getBoolean: Boolean = getLong != 0
    def getBigDecimal: JBigDecimal = {
      val long = getLong
      new JBigDecimal(long)
    }
    def getScalaBigDecimal = BigDecimal(getBigDecimal)

    def getDate: java.sql.Date = ???
    def getTime: java.sql.Time = ???
    def getTimestamp: java.sql.Timestamp = ???
    def getBytes: Array[Byte] = {
      val string = getString
      if (string == null) null
      else string.getBytes
    }
    def getAsciiStream: InputStream = ???
    def getBinaryStream: InputStream = ???
    def getObject: AnyRef = value.asInstanceOf[AnyRef]

    override def toString = s"$name:$value"
  }
  class BooleanCell(name:String, sqlType: Int, value: Boolean) extends Cell(name, sqlType, value) {
    override def getBoolean = value
    override def getString = value.toString
  }
  class ByteCell(name:String, sqltype: Int, value: Byte) extends Cell(name, sqltype, value) {
    override def getLong = value.toLong
  }
  class ShortCell(name:String, sqltype:Int, value: Short) extends Cell(name, sqltype, value) {
    override def getLong = value.toLong
  }
  class IntegerCell(name:String, sqltype:Int, value: Int) extends Cell(name, sqltype, value) {
    override def getLong = value.toLong
  }
  class LongCell(name:String, sqltype:Int, value: Long) extends Cell(name, sqltype, value) {
    override def getLong = value
  }
  class FloatCell(name:String, sqltype: Int, value: Float) extends Cell(name, sqltype, value) {
    override def getLong = value.toLong
    override def getDouble = value.toDouble
  }
  class DoubleCell(name:String, sqltype: Int, value: Double) extends Cell(name, sqltype, value) {
    override def getLong = value.toLong
    override def getDouble = value
  }
  class StringCell(name:String, sqltype: Int, value: String) extends Cell(name, sqltype, value) {
    override def getLong = if (value == null) 0 else value.toLong
    override def getDouble = if (value == null) 0 else value.toDouble
    override def getBigDecimal = if (value == null) null else new JBigDecimal(value)
  }
  class BigDecimalCell(name:String, sqltype:Int, value: JBigDecimal) extends Cell(name, sqltype, value){
    override def getLong = if (value == null) 0 else value.longValue
    override def getDouble = if (value == null) 0 else value.doubleValue
    override def getBigDecimal = value
  }
  class DateCell(name:String, sqltype:Int, value: java.sql.Date) extends Cell(name, sqltype, value) {
    override def getDate = value
    override def getTime = if (value == null) null else new Time(value.getTime)
    override def getTimestamp = if (value == null) null else new Timestamp(value.getTime)
  }
  class TimeCell(name:String, sqltype: Int, value: java.sql.Time) extends Cell(name, sqltype, value) {
    override def getTime = value
    override def getDate = if (value == null) null else new Date(value.getTime)
    override def getTimestamp = if (value == null) null else new Timestamp(value.getTime)
  }
  class TimestampCell(name:String, sqltype: Int, value: java.sql.Timestamp) extends Cell(name, sqltype, value){
    override def getTimestamp = value
    override def getDate = if (value == null) null else new Date(value.getTime)
    override def getTime = if (value == null) null else new Time(value.getTime)
  }
  class BytesCell(name:String, sqltype: Int, value: Array[Byte]) extends Cell(name, sqltype, value) {
    override def getString = if (value == null) null else new String(value)
    override def getBytes = value
  }
  class NullCell[T](name: String, sqltype:Int) extends Cell[T](name, sqltype, null.asInstanceOf[T]) {
    override def getLong = 0
    override def getString = null
    override def getBigDecimal = null
    override def getDate = null
    override def getTime = null
    override def getTimestamp = null
    override def getBytes = null
  }

  def resultSetToRow(meta: ResultSetMetaData, rs: ResultSet): Row = {
    val cells: Seq[Cell[_]] = {
      for (i <- 1 to meta.getColumnCount) yield {
        val name = meta.getColumnLabel(i)
        val sqltype = meta.getColumnType(i)
        val isnull = rs.getObject(i) == null

        if (isnull) new NullCell(name, sqltype)
        else sqltype match {
          case Types.DECIMAL | Types.NUMERIC => new BigDecimalCell(name, sqltype, rs.getBigDecimal(i))
          case Types.BINARY | Types.BLOB | Types.LONGVARBINARY | Types.VARBINARY => new BytesCell(name, sqltype, rs.getBytes(i))
          case Types.BIT | Types.BOOLEAN => new BooleanCell(name, sqltype, rs.getBoolean(i))
          case Types.CLOB | Types.NCLOB | Types.CHAR | Types.NCHAR | Types.VARCHAR | Types.NVARCHAR | Types.LONGNVARCHAR | Types.LONGVARCHAR =>
            new StringCell(name, sqltype, rs.getString(i))
          case Types.DATE => new DateCell(name, sqltype, rs.getDate(i))
          case Types.FLOAT => new FloatCell(name, sqltype, rs.getFloat(i))
          case Types.DOUBLE | Types.REAL => new DoubleCell(name, sqltype, rs.getDouble(i))
          case Types.TINYINT | Types.SMALLINT | Types.INTEGER | Types.BIGINT => new LongCell(name, sqltype, rs.getLong(i))
          case Types.TIME => new TimeCell(name, sqltype, rs.getTime(i))
          case Types.TIMESTAMP => new TimestampCell(name, sqltype, rs.getTimestamp(i))
        }
      }
    }
    new Row(cells)
  }

  implicit val resultSetMapper: ResultSetMapper[Row] = new ResultSetMapper[Row] {
    override def from(rs: ResultSet): Row = resultSetToRow(rs.getMetaData, rs)
  }

}

class Row(val cells: Seq[Row.Cell[_]]) {
  import Row._

  private lazy val cellsByName: Map[String, Cell[_]] = cells.map { cell =>
    (cell.name.toLowerCase, cell)
  }.toMap

  override def toString = cells.map(_.toString).mkString("Row(", ",", ")")

  @inline def cell(index:Int) = cells(index-1)
  @inline def cell(key: String) = cellsByName(key.toLowerCase)

  def getString(index: Int): String = cell(index).getString
  def getString(key: String): String = cell(key).getString

  def getByte(index: Int): Byte = cell(index).getByte
  def getByte(key: String): Byte = cell(key).getByte

  def getShort(index: Int): Short = cell(index).getShort
  def getShort(key: String): Short = cell(key).getShort

  def getInt(index: Int): Int = cell(index).getInt
  def getInt(key: String): Int = cell(key).getInt

  def getLong(index: Int): Long = cell(index).getLong
  def getLong(key: String): Long = cell(key).getLong

  def getFloat(index: Int): Float = cell(index).getFloat
  def getFloat(key: String): Float = cell(key).getFloat

  def getDouble(index: Int): Double = cell(index).getDouble
  def getDouble(key: String): Double = cell(key).getDouble

  def getBoolean(index: Int): Boolean = cell(index).getBoolean
  def getBoolean(key: String): Boolean = cell(key).getBoolean

  def getBigDecimal(index: Int): java.math.BigDecimal = cell(index).getBigDecimal
  def getBigDecimal(key: String): java.math.BigDecimal = cell(key).getBigDecimal

  def getScalaBigDecimal(index: Int): BigDecimal = cell(index).getScalaBigDecimal
  def getScalaBigDecimal(key: String): BigDecimal = cell(key).getScalaBigDecimal

  def getDate(index: Int): java.sql.Date = cell(index).getDate
  def getDate(key: String): java.sql.Date = cell(key).getDate

  def getTime(index: Int): java.sql.Time = cell(index).getTime
  def getTime(key: String): java.sql.Time = cell(key).getTime

  def getTimestamp(index: Int): java.sql.Timestamp = cell(index).getTimestamp
  def getTimestamp(key: String): java.sql.Timestamp = cell(key).getTimestamp

  def getBytes(index: Int): Array[Byte] = cell(index).getBytes
  def getBytes(key: String): Array[Byte] = cell(key).getBytes

  def getAsciiStream(index: Int): InputStream = cell(index).getAsciiStream
  def getAsciiStream(key: String): InputStream = cell(key).getAsciiStream

  def getBinaryStream(index: Int): InputStream = cell(index).getBinaryStream
  def getBinaryStream(key: String): InputStream = cell(key).getBinaryStream

  def getObject(index: Int): AnyRef = cell(index).getObject
  def getObject(key: String): AnyRef = cell(key).getObject


}

