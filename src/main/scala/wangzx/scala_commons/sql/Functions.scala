package wangzx.scala_commons.sql

import java.sql.Timestamp

object Functions {

  def from_unixtime(eppo: Int): Timestamp = new Timestamp(eppo.toLong * 1000L)

  def now(): Timestamp = new Timestamp(System.currentTimeMillis)

}
