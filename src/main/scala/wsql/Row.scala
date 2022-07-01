package wsql

import java.sql._
import java.lang.{Boolean => JBoolean, Byte => JByte, Integer => JInteger, Long => JLong, Short => JShort}
import java.lang.{Double => JDouble, Float => JFloat}
import java.math.{BigDecimal => JBigDecimal}
import java.io.{InputStream, Reader}
import java.net.URL
import java.{math, sql, util}
import java.util.Calendar

import scala.Array
import scala.reflect.ClassTag

/**
 * provide a offline Box for ResultSet which is scrollable and only valid with an open connection.
 */
object Row {

  sealed abstract class Cell[T](val name: String, val sqltype: Int, val value: T) {
    inline def ??? = throw new UnsupportedOperationException
    def getString: String = if (value == null) null else value.toString
    def getLong: Long  = ???
    def getInt: Int = getLong.toInt
    def getByte: Byte = getLong.toByte
    def getShort: Short = getLong.toShort
    def getDouble: Double = getLong.toDouble
    def getFloat: Float = getDouble.toFloat
    def getBoolean: Boolean = getLong != 0
    def getBigDecimal: JBigDecimal = new JBigDecimal(getLong)
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
  class Cell_???(name: String, sqltype: Int) extends Cell[Unit](name, sqltype, ()) {
    override def getString: String = ???
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
          case _ => new Cell_???(name, sqltype) // no error, but that's is not accessable
        }
      }
    }
    new Row(cells)
  }

  given resultSetMapper: ResultSetMapper[Row] with
    override def from(rs: ResultSet): Row = resultSetToRow(rs.getMetaData, rs)
  
}

// make Row extends ResultSet so we can using JavaValueAccessor.
class Row(val cells: Seq[Row.Cell[_]]) extends ResultSet {
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

  def get[T: JdbcValueAccessor](index: Int): T = summon[JdbcValueAccessor[T]].passOut(this, index)
  def get[T: JdbcValueAccessor](key: String): T = summon[JdbcValueAccessor[T]].passOut(this, key)

  //
  def getBigDecimal(columnIndex: Int, scale: Int): JBigDecimal = ???
  def getBigDecimal(columnLabel: String, scale: Int): JBigDecimal = ???
  def getClob(columnIndex: Int): Clob = ???
  def getClob(columnLabel: String): Clob = ???
  def updateShort(columnIndex: Int, x: Short): Unit = ???
  def updateShort(columnLabel: String, x: Short): Unit = ???
  def isFirst: Boolean = ???
  def wasNull(): Boolean = ???
  def updateBytes(columnIndex: Int, x: Array[Byte]): Unit = ???
  def updateBytes(columnLabel: String, x: Array[Byte]): Unit = ???
  def getNString(columnIndex: Int): String = ???
  def getNString(columnLabel: String): String = ???
  def getNClob(columnIndex: Int): NClob = ???
  def getNClob(columnLabel: String): NClob = ???
  def getObject(columnIndex: Int, map: util.Map[String, Class[_]]): AnyRef = ???
  def getObject(columnLabel: String, map: util.Map[String, Class[_]]): AnyRef = ???
  def getObject[T](columnIndex: Int, `type`: Class[T]): T = ???
  def getObject[T](columnLabel: String, `type`: Class[T]): T = ???
  def updateByte(columnIndex: Int, x: Byte): Unit = ???
  def updateByte(columnLabel: String, x: Byte): Unit = ???
  def beforeFirst(): Unit = ???
  def getRow: Int = ???
  def afterLast(): Unit = ???
  def refreshRow(): Unit = ???
  def updateClob(columnIndex: Int, x: Clob): Unit = ???
  def updateClob(columnLabel: String, x: Clob): Unit = ???
  def updateClob(columnIndex: Int, reader: Reader, length: Long): Unit = ???
  def updateClob(columnLabel: String, reader: Reader, length: Long): Unit = ???
  def updateClob(columnIndex: Int, reader: Reader): Unit = ???
  def updateClob(columnLabel: String, reader: Reader): Unit = ???
  def getType: Int = ???
  def updateArray(columnIndex: Int, x: sql.Array): Unit = ???
  def updateArray(columnLabel: String, x: sql.Array): Unit = ???
  def getMetaData: ResultSetMetaData = ???
  def relative(rows: Int): Boolean = ???
  def updateDate(columnIndex: Int, x: Date): Unit = ???
  def updateDate(columnLabel: String, x: Date): Unit = ???
  def getNCharacterStream(columnIndex: Int): Reader = ???
  def getNCharacterStream(columnLabel: String): Reader = ???
  def isLast: Boolean = ???
  def getWarnings: SQLWarning = ???
  def updateObject(columnIndex: Int, x: scala.Any, scaleOrLength: Int): Unit = ???
  def updateObject(columnIndex: Int, x: scala.Any): Unit = ???
  def updateObject(columnLabel: String, x: scala.Any, scaleOrLength: Int): Unit = ???
  def updateObject(columnLabel: String, x: scala.Any): Unit = ???
  def updateBlob(columnIndex: Int, x: Blob): Unit = ???
  def updateBlob(columnLabel: String, x: Blob): Unit = ???
  def updateBlob(columnIndex: Int, inputStream: InputStream, length: Long): Unit = ???
  def updateBlob(columnLabel: String, inputStream: InputStream, length: Long): Unit = ???
  def updateBlob(columnIndex: Int, inputStream: InputStream): Unit = ???
  def updateBlob(columnLabel: String, inputStream: InputStream): Unit = ???
  def updateRowId(columnIndex: Int, x: RowId): Unit = ???
  def updateRowId(columnLabel: String, x: RowId): Unit = ???
  def getDate(columnIndex: Int, cal: Calendar): Date = ???
  def getDate(columnLabel: String, cal: Calendar): Date = ???
  def close(): Unit = ???
  def updateSQLXML(columnIndex: Int, xmlObject: SQLXML): Unit = ???
  def updateSQLXML(columnLabel: String, xmlObject: SQLXML): Unit = ???
  def moveToCurrentRow(): Unit = ???
  def setFetchSize(rows: Int): Unit = ???
  def updateTime(columnIndex: Int, x: Time): Unit = ???
  def updateTime(columnLabel: String, x: Time): Unit = ???
  def clearWarnings(): Unit = ???
  def getCharacterStream(columnIndex: Int): Reader = ???
  def getCharacterStream(columnLabel: String): Reader = ???
  def updateTimestamp(columnIndex: Int, x: Timestamp): Unit = ???
  def updateTimestamp(columnLabel: String, x: Timestamp): Unit = ???
  def getBlob(columnIndex: Int): Blob = ???
  def getBlob(columnLabel: String): Blob = ???
  def rowDeleted(): Boolean = ???
  def isAfterLast: Boolean = ???
  def insertRow(): Unit = ???
  def isClosed: Boolean = ???
  def absolute(row: Int): Boolean = ???
  def getUnicodeStream(columnIndex: Int): InputStream = ???
  def getUnicodeStream(columnLabel: String): InputStream = ???
  def updateFloat(columnIndex: Int, x: Float): Unit = ???
  def updateFloat(columnLabel: String, x: Float): Unit = ???
  def first(): Boolean = ???
  def updateRow(): Unit = ???
  def getCursorName: String = ???
  def getHoldability: Int = ???
  def getArray(columnIndex: Int): sql.Array = ???
  def getArray(columnLabel: String): sql.Array = ???
  def updateNClob(columnIndex: Int, nClob: NClob): Unit = ???
  def updateNClob(columnLabel: String, nClob: NClob): Unit = ???
  def updateNClob(columnIndex: Int, reader: Reader, length: Long): Unit = ???
  def updateNClob(columnLabel: String, reader: Reader, length: Long): Unit = ???
  def updateNClob(columnIndex: Int, reader: Reader): Unit = ???
  def updateNClob(columnLabel: String, reader: Reader): Unit = ???
  def getFetchSize: Int = ???
  def getConcurrency: Int = ???
  def setFetchDirection(direction: Int): Unit = ???
  def updateAsciiStream(columnIndex: Int, x: InputStream, length: Int): Unit = ???
  def updateAsciiStream(columnLabel: String, x: InputStream, length: Int): Unit = ???
  def updateAsciiStream(columnIndex: Int, x: InputStream, length: Long): Unit = ???
  def updateAsciiStream(columnLabel: String, x: InputStream, length: Long): Unit = ???
  def updateAsciiStream(columnIndex: Int, x: InputStream): Unit = ???
  def updateAsciiStream(columnLabel: String, x: InputStream): Unit = ???
  def cancelRowUpdates(): Unit = ???
  def getStatement: Statement = ???
  def getFetchDirection: Int = ???
  def last(): Boolean = ???
  def updateNull(columnIndex: Int): Unit = ???
  def updateNull(columnLabel: String): Unit = ???
  def isBeforeFirst: Boolean = ???
  def updateBoolean(columnIndex: Int, x: Boolean): Unit = ???
  def updateBoolean(columnLabel: String, x: Boolean): Unit = ???
  def getURL(columnIndex: Int): URL = ???
  def getURL(columnLabel: String): URL = ???
  def deleteRow(): Unit = ???
  def getSQLXML(columnIndex: Int): SQLXML = ???
  def getSQLXML(columnLabel: String): SQLXML = ???
  def updateBigDecimal(columnIndex: Int, x: JBigDecimal): Unit = ???
  def updateBigDecimal(columnLabel: String, x: JBigDecimal): Unit = ???
  def rowInserted(): Boolean = ???
  def updateInt(columnIndex: Int, x: Int): Unit = ???
  def updateInt(columnLabel: String, x: Int): Unit = ???
  def updateLong(columnIndex: Int, x: Long): Unit = ???
  def updateLong(columnLabel: String, x: Long): Unit = ???
  def next(): Boolean = ???
  def getTime(columnIndex: Int, cal: Calendar): Time = ???
  def getTime(columnLabel: String, cal: Calendar): Time = ???
  def getRowId(columnIndex: Int): RowId = ???
  def getRowId(columnLabel: String): RowId = ???
  def findColumn(columnLabel: String): Int = ???
  def rowUpdated(): Boolean = ???
  def updateString(columnIndex: Int, x: String): Unit = ???
  def updateString(columnLabel: String, x: String): Unit = ???
  def getRef(columnIndex: Int): Ref = ???
  def getRef(columnLabel: String): Ref = ???
  def getTimestamp(columnIndex: Int, cal: Calendar): Timestamp = ???
  def getTimestamp(columnLabel: String, cal: Calendar): Timestamp = ???
  def updateRef(columnIndex: Int, x: Ref): Unit = ???
  def updateRef(columnLabel: String, x: Ref): Unit = ???
  def previous(): Boolean = ???
  def moveToInsertRow(): Unit = ???
  def updateNString(columnIndex: Int, nString: String): Unit = ???
  def updateNString(columnLabel: String, nString: String): Unit = ???
  def updateDouble(columnIndex: Int, x: Double): Unit = ???
  def updateDouble(columnLabel: String, x: Double): Unit = ???
  def updateNCharacterStream(columnIndex: Int, x: Reader, length: Long): Unit = ???
  def updateNCharacterStream(columnLabel: String, reader: Reader, length: Long): Unit = ???
  def updateNCharacterStream(columnIndex: Int, x: Reader): Unit = ???
  def updateNCharacterStream(columnLabel: String, reader: Reader): Unit = ???
  def updateCharacterStream(columnIndex: Int, x: Reader, length: Int): Unit = ???
  def updateCharacterStream(columnLabel: String, reader: Reader, length: Int): Unit = ???
  def updateCharacterStream(columnIndex: Int, x: Reader, length: Long): Unit = ???
  def updateCharacterStream(columnLabel: String, reader: Reader, length: Long): Unit = ???
  def updateCharacterStream(columnIndex: Int, x: Reader): Unit = ???
  def updateCharacterStream(columnLabel: String, reader: Reader): Unit = ???
  def updateBinaryStream(columnIndex: Int, x: InputStream, length: Int): Unit = ???
  def updateBinaryStream(columnLabel: String, x: InputStream, length: Int): Unit = ???
  def updateBinaryStream(columnIndex: Int, x: InputStream, length: Long): Unit = ???
  def updateBinaryStream(columnLabel: String, x: InputStream, length: Long): Unit = ???
  def updateBinaryStream(columnIndex: Int, x: InputStream): Unit = ???
  def updateBinaryStream(columnLabel: String, x: InputStream): Unit = ???
  def unwrap[T](iface: Class[T]): T = ???
  def isWrapperFor(iface: Class[_]): Boolean = ???
}

