package wangzx.scala_commons.sql
import java.sql.{DriverManager, PreparedStatement, ResultSet}

object mysql {

  // support for Mysql Bit(n)
  // which can't mapping to getInt/getString correctly
  case class MySqlBitSet(val mask: Long) {

    def isSet(n: Int) = {
      assert(n >= 0 && n < 64)
      ((mask >> n) & 0x1L) == 1
    }

    override def toString: String = s"b'${mask.toBinaryString}'"

  }

  object MySqlBitSet {

    implicit object jdbcValueAccessor extends JdbcValueAccessor[MySqlBitSet] {

      override def passIn(stmt: PreparedStatement, index: Int, value: MySqlBitSet): Unit =
        stmt.setBytes(index, toByteArray(value.mask))

      override def passOut(rs: ResultSet, index: Int): MySqlBitSet = {
        val v = rs.getBytes(index)
        if(v != null)
          new MySqlBitSet(fromByteArray(v))
        else null
      }

      override def passOut(rs: ResultSet, name: String): MySqlBitSet = {
        val v = rs.getBytes(name)
        if(v != null)
          new MySqlBitSet(fromByteArray(v))
        else null
      }

      def toByteArray(l: Long): Array[Byte] = {
        val b0 = (l & 0xFF).toByte
        val b1 = ((l >> 8) & 0xFF).toByte
        val b2 = ((l >> 16) & 0xFF).toByte
        val b3 = ((l >> 24) & 0xFF).toByte
        val b4 = ((l >> 32) & 0xFF).toByte
        val b5 = ((l >> 40) & 0xFF).toByte
        val b6 = ((l >> 48) & 0xFF).toByte
        val b7 = ((l >> 56) & 0xFF).toByte
        Array(b7, b6, b5, b4, b3, b2, b1, b0)
      }

      def fromByteArray(bytes: Array[Byte]): Long = {
        assert(bytes.length <= 8)
        var shift = (bytes.length - 1) * 8

        var mask = 0
        bytes.foreach { b =>
          mask = mask | ((b & 0xFF) << shift)
          shift -= 8
        }

        mask
      }
    }
  }


}
