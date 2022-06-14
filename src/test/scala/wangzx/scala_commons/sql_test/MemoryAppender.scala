package wangzx.scala_commons.sql_test

import java.io.ByteArrayOutputStream

import ch.qos.logback.core.OutputStreamAppender

/**
  * Created by wangzx on 15/12/18.
  */
object MemoryAppender {

  val buffer = new ByteArrayOutputStream()

  def reset(): Unit = {
    buffer.reset()
  }

  def getContent = buffer.toString

}
class MemoryAppender[E] extends OutputStreamAppender[E] {

  override def start(): Unit ={

    setOutputStream(MemoryAppender.buffer)

    super.start()
  }

}
