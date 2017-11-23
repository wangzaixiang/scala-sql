package wangzx.scala_commons.sql_test

import java.net.InetAddress
import java.sql.{PreparedStatement, ResultSet}

/**
  * Created by wangzx on 2017/1/4.
  */
object Test2 {

  def main(args: Array[String]): Unit = {
    val begin = System.currentTimeMillis()
    println( InetAddress.getLocalHost )
    val end = System.currentTimeMillis()
    println(s"time = ${(end-begin)/1000}")
  }
}
