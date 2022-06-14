package wangzx.scala_commons.sql_test

import java.sql.{PreparedStatement, ResultSet}
import javax.sql.DataSource
import scala.language.implicitConversions

import wangzx.scala_commons.sql.DbEnum
import wangzx.scala_commons.sql.{given, *}

object EnumTest {

  class TOrderStatus private (val id: Int, val name:String)

  object TOrderStatus {
    val NEW = new TOrderStatus(0, "NEW")
    val CONFIRMED = new TOrderStatus(1, "CONFIRMED")
    val CANCELED = new TOrderStatus(2, "CANCELED")

    def apply(id: Int): TOrderStatus = id match {
      case 0 => NEW
      case 1 => CONFIRMED
      case 2 => CANCELED
      case _ => new TOrderStatus(id, s"<$id>")
    }
  }


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
    implicit val resultSetMapper: ResultSetMapper[Order] = ResultSetMapper.material[Order]
  }
  
  case class TOrder
  (
    id: Int,
    orderNo: String,
    orderStatus: TOrderStatus
//    orderStatus: Int
//      orderStatus: Option[Int]    // not works
  )

  implicit def orderStatus2TOrderStatus(obj: OrderStatus): TOrderStatus = TOrderStatus(obj.id)
  implicit def torderStatus2OrderStatus(obj: TOrderStatus): OrderStatus = OrderStatus.valueOf(obj.id)

  def main(args: Array[String]): Unit = {
    val dataSource: DataSource = null

    val orderStatus: OrderStatus = OrderStatus.NEW

    println(s"orderStatus = $orderStatus")

    dataSource.executeUpdate(sql"update orders set status = ${orderStatus}")

    val rows1 = dataSource.rows[Order](sql"select * from orders")

    // SomeEnum1 -> SomeEnum2
    // SomeEnum2.apply[T](...)
    // SomeEnum1.unapply[T](value)

    // val status2 = orderStatus.copyTo[TOrderStatus]
    val order: Order = null
//    val torder:TOrder = BeanBuilder.build[TOrder](order)() // require Int=>TOrderStatus

//    val order2 = BeanBuilder.build[Order](torder)()
  }

}
