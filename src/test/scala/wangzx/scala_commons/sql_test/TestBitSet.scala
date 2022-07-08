package wangzx.scala_commons.sql_test

import java.sql.DriverManager
import wsql.mysql.MySqlBitSet
import wsql.{given, *}

object TestBitSet {

  case class SkuBarCode(id: Int, flag: MySqlBitSet)

  case class TSkuBarCode(id:Int, flag: Long)

  def main(args: Array[String]): Unit = {
    MySqlBitSet.jdbcValueAccessor.toByteArray(825241904L).foreach(x => println(x & 0xFF))

    println(MySqlBitSet.jdbcValueAccessor.fromByteArray(Array(49, 48, 49, 48)))
    println(MySqlBitSet.jdbcValueAccessor.fromByteArray(Array(0, 49, 48, 49, 48)))
    println(MySqlBitSet.jdbcValueAccessor.fromByteArray(Array(0, 0, 49, 48, 49, 48)))
    println(MySqlBitSet.jdbcValueAccessor.fromByteArray(Array(0, 0, 0, 49, 48, 49, 48)))
    println(MySqlBitSet.jdbcValueAccessor.fromByteArray(Array(0, 0, 0, 0, 49, 48, 49, 48)))

    println(MySqlBitSet.jdbcValueAccessor.fromByteArray(Array(10)))

    val conn = DriverManager.getConnection("jdbc:mysql://127.0.0.1:3307/test1", "root", "root").nn

    val mask = new MySqlBitSet(0x1234)
//    conn.executeUpdate(sql"insert into sku_barcode values(5, $mask)")

    val sku6 = SkuBarCode(6, MySqlBitSet(0x8080))
    // conn.executeUpdate(sql"insert into sku_barcode values(${sku6.id}, ${sku6.flag})")


    conn.eachRow[Row]("select * from sku_barcode") { row =>
      val id = row.get[Int](1)
      val flag = row.get[MySqlBitSet|Null](2).nn
      println(s"id = $id flags = ${flag.mask}")
    }

    println("+=======")

    conn.rows[SkuBarCode]("select * from sku_barcode").foreach(println)


    val x: Long = MySqlBitSet.unapply(sku6.flag).mask
//    val tbean = BeanBuilder.build[TSkuBarCode](sku6)()
//    println(tbean)

//    val y = BeanBuilder.build[SkuBarCode](tbean)()
//    println(y)


  }
}
