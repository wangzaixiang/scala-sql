package wangzx.scala_commons.sql_tet

import java.sql.{PreparedStatement, ResultSet}
import javax.sql.DataSource

import wangzx.scala_commons.sql.DbEnum
import wangzx.scala_commons.sql._

object EnumTest {

  object OrderStatus {

    val NEW = new OrderStatus(0, "NEW")

    val CONFIRMED = new OrderStatus(1, "Confirmed")

    val CANCELED = new OrderStatus(2, "Canceled")

    def unknowne(id: Int) = new OrderStatus(id, s"<$id>")

    def valueOf(id: Int): OrderStatus = id match {
      case 0 => NEW
      case 1 => CONFIRMED
      case 2 => CANCELED
      case _ => unknowne(id)
    }

    implicit object Accessor extends DbEnumJdbcValueAccessor[OrderStatus](valueOf)

  }
  class OrderStatus private(val id:Int, val name:String) extends DbEnum

  case class Order
  (
    id: Int,
    orderNo: String,
    orderStatus: OrderStatus
  )
  object Order {
    implicit val resultSetMapper: ResultSetMapper[Order] = ResultSetMapper.meterial[Order]
  }

  def main(args: Array[String]): Unit = {
    val dataSource: DataSource = null

    val orderStatus = OrderStatus.NEW

    println(s"orderStatus = $orderStatus")

    dataSource.executeUpdate(sql"update orders set status = ${orderStatus}")

    val rows1 = dataSource.rows[Order](sql"select * from orders")


  }

}
