package wangzx.scala_commons.sql_test

import java.sql.{PreparedStatement, ResultSet}

/**
  * Created by wangzx on 2017/1/4.
  */
object Test2 {

  def dummySql(args: JdbcValue[_]*): Unit = {

  }

  /*
  @implicitly( "JdbcValueAccessor" = someClass )
  public enum OrderStatus implements IntEnum<OrderStatus> {

    STATUS_1( 1 ),
    STATUS_2( 2 );

    private int code;

    OrderStatus(int code) {
        this.code  = code;
    }

    public int code() { return code; }

    public static OrderStatus apply(int code) {
        return STATUS_1.fromCode( code );
    }
   }
   */
//  implicit object JdbcValueAccessor_OrderStatus extends JdbcValueAccessor[OrderStatus] {
//    override def passIn(stmt: PreparedStatement, index: Int, value: OrderStatus): Unit =
//      stmt.setInt(index, value.code())
//
//    override def passOut(rs: ResultSet, index: Int): OrderStatus =
//      OrderStatus.apply(rs.getInt(index))
//  }

  def testPassIn(): Unit ={

    dummySql(true, 1.toByte, 1.toShort, 1, 1L, 1.toFloat, 1.toDouble, "Hello")

    val dec: BigDecimal = BigDecimal(1)
    dummySql(dec)

    val optInt = Some(10)
    dummySql(optInt)

    val orderStatus = OrderStatus.STATUS_1
    dummySql(orderStatus)


  }

  def testPassout(): Unit = {
    val rs: ResultSet = null

    val boolean: Boolean = rs.get[Boolean](1)
    val byte: Byte = rs.get[Byte](1)
    val short: Short = rs.get[Short](1)
    val string: String = rs.get[String](1)
    val jdec: java.math.BigDecimal = rs.get[java.math.BigDecimal](1)
    val sdec: BigDecimal = rs.get[BigDecimal](1)

    // JdbcValueAccessor[Option[Int]]
    val optInt: Option[Int] = rs.getOption[Int](1)

//    val orderStatus = rs.get[OrderStatus](1)
//    val x = implicitly[JdbcValueAccessor[]]

  }

}
